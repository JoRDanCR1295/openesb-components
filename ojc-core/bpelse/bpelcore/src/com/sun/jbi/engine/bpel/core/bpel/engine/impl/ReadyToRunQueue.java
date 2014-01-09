/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ReadyToRunQueue.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactivityReason;


/**
 * Read-to-run queue
 *
 * @author Sun Microsystems
 */
public class ReadyToRunQueue {

    private LinkedList<BusinessProcessInstanceThread> mBPIList;
    private Set<BusinessProcessInstanceThread> mWaitingBPIsSet;
    private BusinessProcessInstanceThread mMostRecentlyExpiringBPIT;
    private BPELProcessManager mProcMgr = null;
    
    /**
     * Creates a new instance of ReadyToRunQueue
     */
    public ReadyToRunQueue(BPELProcessManager procMgr) {
        mProcMgr = procMgr;
        mBPIList = new LinkedList<BusinessProcessInstanceThread>();
        mWaitingBPIsSet = new HashSet<BusinessProcessInstanceThread>();
    }

    /**
     * Gets ready-to-run BP process instance thread
     * @return BusinessProcessInstanceThread BP process instance thread
     */
    public BusinessProcessInstanceThread getReadyInstance() {
        BusinessProcessInstanceThread bpiRet = null;

        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized (processLock) {
            
            Iterator<BusinessProcessInstanceThread> itr = mBPIList.iterator();

            while (itr.hasNext()) {
                Object object = itr.next();
                BusinessProcessInstanceThread bpi = (BusinessProcessInstanceThread) object;

                if (bpi.isReady()) {
                    bpiRet = bpi;
                    itr.remove();
                    break;
                }
            }

            // NOTE: Moving the waiting bpit to after checking the the mBPITList for ready bpits
            // as putting it before causes two EventHandler JUnits (testProcessLevelNestedEventHdlr and test_nestedScopeLevelEveHdlr)
            // to fail due to some limitations of these failing junits. These JUnits need to be revisited
            // to ensure that they pass regardless of the order of the bpit return from this method call.
            if (bpiRet == null) {
                bpiRet = getExpiredWaitingBPIT();
                
                if (bpiRet != null 
            			&& !(bpiRet instanceof BusinessProcessInstanceThreadForClusteredPick)
            			&& bpiRet.getType() != BusinessProcessInstanceThread.SCALABILITY_PASSIVATED) {
					
					boolean isOnAlarmForPick = false;
					
					if (bpiRet instanceof BusinessProcessInstanceThreadForPick) {
						isOnAlarmForPick = true;
					} else {
						isOnAlarmForPick = false;
					}
					
					ICallFrame frame = bpiRet.getCallFrame();
					frame.getProcessInstance().removeFromInactiveList(frame, isOnAlarmForPick);
				}
            }
        }
        
        if (bpiRet != null 
    			&& bpiRet.getType() == BusinessProcessInstanceThread.SCALABILITY_PASSIVATED) {
        	String instanceId = bpiRet.getProcessInstanceId();
			mProcMgr.getMemoryMgr().activateScalabilityPassInstance(instanceId, InactivityReason.WAITING);
        }

        return bpiRet;
    }
    
    
	/**
	 * Check the locally cached bpit and check if it expired already. If yes,
	 * select the next recently expiring bpit and return the locally cached one.
	 * If the locally cached bpit is marked as scalability passivated, schedule
	 * the instance for recovery.
	 * 
	 * @return
	 */
    private BusinessProcessInstanceThread getExpiredWaitingBPIT() {
        BusinessProcessInstanceThread bpiRet = null;

        if (mMostRecentlyExpiringBPIT != null) {
        	
            // check if the locally cached bpit is not suspended
            if (mMostRecentlyExpiringBPIT.isReady()) {
                long now = System.currentTimeMillis();
                
                if (now >= mMostRecentlyExpiringBPIT.getTimeout()) {
					bpiRet = mMostRecentlyExpiringBPIT;
					selectNextBPITForCaching();
				}
            } else {
                bpiRet = getNextReadyBPIT();
            }
        }

        return bpiRet;
    }

    private BusinessProcessInstanceThread getNextReadyBPIT() {
        Iterator<BusinessProcessInstanceThread> iter = mWaitingBPIsSet.iterator();
        BusinessProcessInstanceThread retBpit = null;
        
        while(iter.hasNext()) {
            retBpit = iter.next();
            
            // check if the bpit is not suspended and is expired
            if (retBpit.isReady() && retBpit.getTimeout() <= System.currentTimeMillis()) {
                mWaitingBPIsSet.remove(retBpit);
                return retBpit;
            }
        }
        
        return null;
    }
    
    private BusinessProcessInstanceThread getNextNonSuspendedBPIT() {
        Iterator<BusinessProcessInstanceThread> iter = mWaitingBPIsSet.iterator();
        BusinessProcessInstanceThread retBpit = null;
        
        while(iter.hasNext()) {
            retBpit = iter.next();
            
            // check if the bpit is not suspended and is expired
            if (!retBpit.isSuspended() && retBpit.getTimeout() < System.currentTimeMillis()) {
                return retBpit;
            }
        }        
        return null;
    }    

    /**
     * select the next recently expiring bpit
     */
    public void selectNextBPITForCaching() {
        mMostRecentlyExpiringBPIT = null;
        
        BusinessProcessInstanceThread bpit = null;
        
        Iterator<BusinessProcessInstanceThread> iter = mWaitingBPIsSet.iterator();
        while(iter.hasNext()) {
            bpit = iter.next();
            
            if (mMostRecentlyExpiringBPIT == null) {
                mMostRecentlyExpiringBPIT = bpit;
                continue;
            }
            
            if (bpit.getTimeout() <= mMostRecentlyExpiringBPIT.getTimeout() ) {
                mMostRecentlyExpiringBPIT = bpit; 
            }
        }
        
        if (mMostRecentlyExpiringBPIT != null) {
            mWaitingBPIsSet.remove(mMostRecentlyExpiringBPIT);
        }
    }
    
    /**
     * Adds BP process instance thread
     * 
     * @param bpit BP process instance thread
     */
    public void addBPI(BusinessProcessInstanceThread bpit) {
        assert bpit != null;
        assert (bpit.getType() == 0) || (bpit.getType() == 1);
        
        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized (processLock) {
        	if (bpit.getType() == BusinessProcessInstanceThread.TIMEOUT) {
        		
        		if (!(bpit instanceof BusinessProcessInstanceThreadForClusteredPick)) {
        		
                    /* If this activity is being terminated, then set the timeout to the current time so that 
                     * it is ready to execute. This check is required here since the thread executing the
                     * activateWaitingCallFrames() may look for the callframe just before the timeout
                     * activity puts the BPIT in the ReadyToRunQueue. In case the thread executing the
                     * activateWaitingCallFrames() looks for the callframe just after the timeout activity
                     * puts the BPIT in the ReadyToRunQueue, the timeout could be set to 0 twice (once here)
                     * and one by makeTimeoutBpitReadyToRun() method. But that is harmless.
                     */
        			if (bpit.getCallFrame().getProgramCounter().getContext().getFaultHandlingContext().isBeingTerminated()) {
                        bpit.setTimeout(System.currentTimeMillis());
                    } else {
                    	InactivityReason reason = null;

                    	if (bpit instanceof BusinessProcessInstanceThreadForPick) {
                    		reason = InactivityReason.ONALARM_FOR_PICK;
                    	} else {
                    		reason = InactivityReason.WAITING;
                    	}

                    	ICallFrame frame = bpit.getCallFrame();
                    	frame.getProcessInstance().addToInactiveList(frame, reason);
                    }
        		}

        		if (mMostRecentlyExpiringBPIT == null) {
        			mMostRecentlyExpiringBPIT = bpit;
        			return;
        		} 

        		if (bpit.getTimeout() < mMostRecentlyExpiringBPIT.getTimeout()) {
        			mWaitingBPIsSet.add(mMostRecentlyExpiringBPIT);
        			mMostRecentlyExpiringBPIT = bpit;
        		} else {
        			mWaitingBPIsSet.add(bpit);
        		}

        	} else {
                mBPIList.add(bpit);
            }
        }
    }

    /**
     * Gets next scheduled time
     * @return long next scheduled time
     */
    public long getNextScheduledTime() {
        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized(processLock) {
            if (mMostRecentlyExpiringBPIT != null) {
                if (!mMostRecentlyExpiringBPIT.isSuspended()) {
                    return mMostRecentlyExpiringBPIT.getTimeout();
                }else {
                    BusinessProcessInstanceThread bpit = getNextNonSuspendedBPIT ();
                    if (bpit != null) {
                        return bpit.getTimeout();
                    }else {
                        return 0;
                    }                    
                }
            } else {
                return 0;
            }
        }
    }
    
    /**
     * Removes all the BusinessProcessInstanceThreads, that belong to the process instance 
     * with the specified bpInstanceId, from the ReadyToRunQueue 
     * @param bpInstanceId The bpInstanceId for which the BusinessProcessInstanceThreads 
     * should be removed
     * @param isScalabilityPassivation TODO
     */
    public void terminate(String bpInstanceId, boolean isScalabilityPassivation) {
    	BusinessProcessInstanceThread bpit = null;

        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized (processLock) {
            
            BusinessProcessInstanceThread bpit1 = null;
			ICallFrame callframe = null;
			
			if (mMostRecentlyExpiringBPIT != null) {
			
			    // NOTE: The waitingBPISSet must be iterated first before checking
			    // the mMostRecentlyExpiringBPIT for the callframe match because
			    // if there are callframe(s) in the waitingBPISSet and also mMostRecentlyExpiringBPIT
			    // for the given instance, you need to get rid of the bpit in mMostRecentlyExpiringBPIT
			    // first so that when the locally cached bpit (mMostRecentlyExpiringBPIT) is removed
			    // the next one from the set (waitingBPISSet) can be picked up.
			    
			    Iterator<BusinessProcessInstanceThread> iter = mWaitingBPIsSet.iterator();
			    while (iter.hasNext()) {
			        bpit1 = iter.next();
			        callframe = bpit1.getCallFrame();
					if ((bpit1.getCallFrame() != null) && (bpInstanceId.equals(callframe.getBPId()))) {
						mWaitingBPIsSet.remove(bpit1);
					}
				}
			    
			    callframe = mMostRecentlyExpiringBPIT.getCallFrame();
			    if ((callframe != null) && (bpInstanceId.equals(callframe.getBPId()))) {
					selectNextBPITForCaching();
				}
			}
            
    		for (int i = 0; i < mBPIList.size(); i++) {
    			bpit = mBPIList.get(i);
    			// DEVNOTE: In the case of the Business Process Instance Thread (bpit) for clustered pick
    			// there will not be a call frame associated with it. Hence the null check for the callframe for
    			// the bpit.
    			if ((bpit.getCallFrame() != null) && (bpInstanceId.equals(bpit.getCallFrame().getBPId()))) {
    				mBPIList.remove(i);
    			}
    		}
    	}
    }
    
    /**
     * Makes a TimeOut BusinessProcessInstanceThread associated with the callFrame ready to be
     * executed. The method simply sets the Timeout value of the BusinessProcessInstanceThread to 0.
     * Wait, pick onAlarm as well as eventHandlers onAlarms would all be activated using this call.
     * 
     * @param callFrame
     */
    public void expireTimeoutBpit(Collection callFrames){
        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized (processLock) {
            if (mMostRecentlyExpiringBPIT != null) {
                if (callFrames.remove(mMostRecentlyExpiringBPIT.getCallFrame())) {
                    mMostRecentlyExpiringBPIT.setTimeout(System.currentTimeMillis());
                }

                BusinessProcessInstanceThread bpit = null;
                Iterator<BusinessProcessInstanceThread> itr = mWaitingBPIsSet.iterator();
                while (itr.hasNext()) {
                    bpit = itr.next();
                    if (callFrames.remove(bpit.getCallFrame())) {
                        bpit.setTimeout(System.currentTimeMillis());
                    }
                }
            }
        }
    }
    
    /**
     * Removes a BusinessProcessInstanceThread from the ReadyToRunQueue
     * @param bpit BusinessProcessInstanceThread to be removed
     */
    public boolean remove(BusinessProcessInstanceThread bpit) {
        Object processLock = mProcMgr.getProcessLevelLock();
        
        synchronized (processLock) {
            if (bpit.getType() == BusinessProcessInstanceThread.TIMEOUT) {
            	boolean returnFlag = false;
            	
                if (bpit == mMostRecentlyExpiringBPIT) {
                    selectNextBPITForCaching();
                    returnFlag = true;
                } else {
                	returnFlag = mWaitingBPIsSet.remove(bpit);
                }
                
                if (returnFlag) {
                	/* due to execution of any of the on message event,
                	 * the on alarm timer for pick is removed,
                	 * hence, this frame need to be removed from the inactive list
                	 * so that the scalability does not consider this branch
                	 * (with pick) as inactive
                	 */
                	ICallFrame frame = bpit.getCallFrame();
                	frame.getProcessInstance().removeFromInactiveList(frame, true);
                }
                return returnFlag;
                
            } else {
                return mBPIList.remove(bpit);
            }
        }
    }
    
    public BusinessProcessInstanceThread getMostRecentlyExpiringBPIT() {
    	return mMostRecentlyExpiringBPIT;
    }
    
    public Set<BusinessProcessInstanceThread> getWaitingBPIsSet() {
        return mWaitingBPIsSet;
    }

    public void cleanUp(final String bpId) {
        ReentrantLock lock = new ReentrantLock();
        lock.lock();
        try {
            for (Iterator<BusinessProcessInstanceThread> i = mWaitingBPIsSet.iterator(); i.hasNext();) {
                final BusinessProcessInstanceThread bpit = i.next();
                if (bpId.equals(bpit.getProcessInstanceId())) {
                    i.remove();
                }
            }
        } finally {
            lock.unlock();
        }
    }
}
