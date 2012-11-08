/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RReceive;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPITForPendingInstancePassivation;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BusinessProcessInstanceThreadForClusteredPick;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.persist.NonExpiredOnAlarmInstanceInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author Sun Inc
 * Aug 30, 2007
 */
public class ClusterManager {
    private static Logger LOGGER = Logger.getLogger(ClusterManager.class.getName());

    Engine mEng;
    private boolean mPassivated = false;
    private Object mActiveMessagingActivitiesLock = new Object();
    private int mActiveMessagingActivitiesCount = 0;
    
    public ClusterManager(Engine eng) {
        mEng = eng;
    }
    /**
     * For bpel engine running in clustered environment, the instance is passivated for
     * correlated receive when the incoming message is not available to be consumed.
     * @param events TODO
     * @param rObjs
     * @param frame
     * @param pick
     * @param deadline
     * 
     * @return boolean true means that the instance is passivated
     */
    private boolean passivateInstance(InComingEventKeyImpl[] events,
                                        ICallFrame frame,
                                        Pick pick,
                                        long deadline) {
        if (checkFlowQualification(frame)) {
            // The following is take care of the fact that if this receive is part of a flow
            // branch and any of the the other branch of flow there is at least one more
            // messaging activity that is active, we cannot passivate the instance till the
            // other active messaging activity completes. Example, there is one active
            // invoke (one way or two way invoke waiting on status or response)
            return passivateForFlowBranchCase(events, frame, mEng, pick, deadline);
        } else {
            passivateInstance(frame, pick, deadline);
        }
        

        
        return true;
    }
    
    boolean passivateInstance(ReceiveUnitImpl unit, ICallFrame frame, InComingEventKeyImpl event) {

        if (isClusteringEnabled(frame)) {
            InComingEventKeyImpl[] events = {event};
            boolean isPassivated = passivateInstance(events, frame, null, -1);
            
            if (isPassivated) {
            	logMessage(unit, frame);
            }
            return isPassivated;
        }
        return false;
    }
    
    private void logMessage(ActivityUnit unit, ICallFrame frame) {
		LOGGER.log(Level.INFO, I18n.loc("BPCOR-5013: Engine running in Cluster Mode, " +
				" passivating the instance with id {0}", frame.getBPId() + " while waiting for event for activity : " + unit.getStaticModelActivity().getAttributeValue("name") + " (Activity Type :" + unit.getStaticModelActivity().getQualifiedName() + ")"));
    }

    boolean passivateInstance(PickUnitImpl unit, InComingEventKeyImpl[] events, 
            ICallFrame frame, long deadline) {
        
        if (isClusteringEnabled(frame)) {
            boolean isPassivated = passivateInstance(events, frame, 
                    (Pick) unit.getStaticModelActivity(), deadline);
            if (isPassivated) {
            	logMessage(unit, frame);
            }

            return isPassivated;
        }
        return false;
    }
    
    /**
     * Used in the case of bpel service engine running in clustered mode and for IMA and invoke activities
     * defined in a flow. This marks the completion of this activity unit.
     * @param frame
     */
    void decrementActiveMessagingActivitiesCount(ICallFrame frame) {
        if (checkFlowQualification(frame)) {
			// marks that this receive is completed so that the instance if
			// marked for passivation can be passivated.
			decrementActiveMessagingActivitiesCount();
		}
    }
    
    /**
	 * For bpel engine running in clustered environment and business process
	 * defined with flow, active execution of IMA or Invoke (in flow branch),
	 * the following api will keep track of active executions so that another
	 * branch of the flow does not passive the instance will any of the IMA or
	 * Invokes is active.
	 * 
	 * @param frame
	 * @param event
	 * @param request
	 * 
	 * @return
	 */
    boolean incrementActiveMessagingActivitiesCount(ICallFrame frame,
                                                              InComingEventKeyImpl event,
                                                              MessageContainer request) {
        if (checkFlowQualification(frame)) {
            // marks that this receive is active so that other similar messaging activity
            // in other flow branch should not passivate the instance.
            return incrementActiveMessagingActivitiesCount(event, request, frame.getProcessInstance().getBPELProcessManager());
        }
        return true;
    }
    
    private boolean isClusteringEnabled(ICallFrame frame) {
    	if (frame.getProcessInstance().getBPELProcessManager().isPersistenceEnabled() && mEng.isClustered()) {
    		return true;
    	}
    	return false;
    }
    
    private boolean checkFlowQualification(ICallFrame frame) {
    	if (frame.getProcessInstance().getBPELProcessManager().isPersistenceEnabled() 
        		&& mEng.isClustered() && frame.isFlowBranchCF()) {
    		return true;
    	}
    	
    	return false;
    }

    private void passivateInstance(ICallFrame frame, Pick pick, long deadline) {
        BPELProcessInstance procInstance = frame.getProcessInstance();
        BPELProcessManager procMgr = procInstance.getBPELProcessManager();
        
        Object processLock = procMgr.getProcessLevelLock();
        
        synchronized (processLock) {
        	passivateInstance(frame);

        	if (null != pick && pick.getOnAlarms().size() > 0) {
        		BusinessProcessInstanceThread bpitForClusteredPick = new BusinessProcessInstanceThreadForClusteredPick(
        				procMgr, mEng, procInstance.getId(), deadline);
        		NonExpiredOnAlarmInstanceInfo instanceInfo = new NonExpiredOnAlarmInstanceInfo(
        				frame.getProcessInstance().getId(), deadline);
        		procMgr.addNonExpiredOnAlarmRunningInstances(instanceInfo);
        		procMgr.addToReadyToRunQueue(bpitForClusteredPick);
        	}
        	
        	procMgr.cleanUp(procInstance);
		}
    }
    
    /**
	 * 1. set the OWNERLOCK to NO for the state in the State table for this instance 
	 * 2. put an entry in the WaitingIMA table, to indicate the
	 * waiting activity that should activate the instance for further
	 * execution.
	 * 
	 * @param frame
	 */
    public void passivateInstance(ICallFrame frame) {

        StateManager mgr = mEng.getStateManager();
        BPELProcessInstance procInstance = frame.getProcessInstance();

        RActivity act = frame.getProgramCounter().getStaticModelActivity();
        Collection messageActivities = null;

        if (act instanceof RReceive) {
            RReceive receive = (RReceive) act;
            messageActivities = new ArrayList();
            messageActivities.add(receive);
        } else if (act instanceof Pick) {
            Pick pick = (Pick) act;
            messageActivities = pick.getOnMessages();
        }

        mgr.passivateInstance(procInstance.getId(), mEng.getId(), messageActivities, 
        		frame.getProgramCounter().getContext().getStateContext().getState());

        if (LOGGER.isLoggable(Level.FINE)) {
        	LOGGER.log(Level.FINE, I18n.loc("Instance Id {0} * Passivated * by Engine {1}", procInstance.getId(), mEng.getId()));
        }
    }

    private boolean decrementActiveMessagingActivitiesCount() {
        if (mPassivated) {
            return false;
        }
        synchronized (mActiveMessagingActivitiesLock) {
            this.mActiveMessagingActivitiesCount--;
            return true;
        }
    }

    public boolean isPassivated() {
        synchronized (mActiveMessagingActivitiesLock) {
            return this.mPassivated;
        }
    }

    public int getActiveMessagingActivitiesCount() {
        synchronized (mActiveMessagingActivitiesLock) {
            return mActiveMessagingActivitiesCount;
        }
    }
    
    private boolean incrementActiveMessagingActivitiesCount(InComingEventKeyImpl event, 
            MessageContainer request, BPELProcessManager procManager) {
        synchronized (mActiveMessagingActivitiesLock) {
            if (mPassivated) {
                if (null != event) {
                    // for invoke case, the event will be null
                    procManager.addToCorrelatedWaitingEventsMap(event, request);
                }
                return false;
            } else {
                mActiveMessagingActivitiesCount++;
                return true;
            }
        }
    }
    
    /** passivate for flow branch case.
     * @param events
     * @param frame
     * @param eng
     * @param pick
     * @param deadline
     * @return
     */
    public boolean passivateForFlowBranchCase(InComingEventKeyImpl[] events, ICallFrame frame, 
            Engine eng, Pick pick, long deadline) {
        synchronized (mActiveMessagingActivitiesLock) {
            if (mPassivated) {
                return true;
            }

            if (mActiveMessagingActivitiesCount == 0) {
                // passivate instance
                passivateInstance(frame, pick, deadline);
                // instance will be marked as passivated to prevent other flow branch messaging
                // activities to proceed furhter.
                mPassivated = true;
                return true;
            } else {
                // instance cannot passivate, because this callframe is for a flow branch
                // and there are multiple messaging activities defined and at least one other
                // messaging activity (on any other branch) is active.
                // Execution cannot proceed futher, as the We will put the instance back to
                // ready to run queue to periodically check if the instance can be passivated
                BPELProcessManager procMgr = frame.getProcessInstance().getBPELProcessManager();
                BPITForPendingInstancePassivation bpitForPendingPassivation = new BPITForPendingInstancePassivation(
                        events, procMgr, eng, frame, pick, deadline);
                procMgr.addToReadyToRunQueue(bpitForPendingPassivation);
                return false;
            }
        }
    }
}
