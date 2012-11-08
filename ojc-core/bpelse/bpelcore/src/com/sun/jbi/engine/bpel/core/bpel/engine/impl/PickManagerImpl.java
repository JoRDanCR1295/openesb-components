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
 * @(#)PickManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;


import java.util.Hashtable;
import java.util.Map;

import com.sun.bpel.model.Activity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactivityReason;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;


/**
 * Pick Manager implementation
 *
 * @author Sun Microsystems
 */
class PickManagerImpl {
    
    /** Map<PickOnMsgEventKey, InComingEventKeyImpl[]>  */
    private Hashtable mPickOnMsgEventMap = new Hashtable();
    
    /** Map<PickOnMsgEventKey, BusinessProcessInstanceThreadForPick>  */
    private Hashtable mPickTimerEventMap = new Hashtable();
    
    /** Map<InComingEventKeyImpl, ICallFrame>  */
    private Map mPickRequestPendingInstances = new Hashtable();
    
    private BPELProcessManagerImpl mProcMgr = null;
    
    PickManagerImpl(BPELProcessManagerImpl procMgr) {
        mProcMgr = procMgr;
    }
    
    /**
     * removes OnMessage pending events from pending queue
     *
     * @param event incoming event key
     *
     * @return
     */
    ICallFrame getPendingInstance(InComingEventKeyImpl event) {
        ICallFrame frame = null;
        frame = (ICallFrame) mPickRequestPendingInstances.get(event);
        
        if (frame == null) { // means the instance hasn't reached this Pick yet.
            return null;
        }
        
        BusinessProcessInstanceThread bpit = removeEntriesFromMapsAndGetBPIT(frame);
        if ((bpit != null) && (!mProcMgr.removeFromReadyToRunQueue(bpit))) {
            // means timer thread already fired
            return null;
        }
        return frame;
    }
    
    boolean instancePending(ICallFrame callFrame) {
    	return mPickRequestPendingInstances.containsValue(callFrame);
    }
    
    boolean removePendingInstanceAndCheckIfTimerFired(ICallFrame callFrame) {
        BusinessProcessInstanceThread bpit = removeEntriesFromMapsAndGetBPIT(callFrame);
        if ((bpit != null) && (mProcMgr.removeFromReadyToRunQueue(bpit)) == false) {
            // means timer thread already fired
            return true;
        }
        return false;
    }

    /**
     * @param engine 
     * @param procMgr 
     * @param frame
     * @param deadline
     * @param events
     * @return returns BusinessProcessInstanceThread, if deadline value is greater than 0
     */
    BusinessProcessInstanceThread addToPendingQueue(BPELProcessManager procMgr, Engine engine, ICallFrame frame, 
            long deadline, InComingEventKeyImpl[] events) {
        
        BusinessProcessInstanceThread bpit = null;
        
        Activity act = frame.getProgramCounter().getStaticModelActivity();
        PickOnMsgEventKey key = new PickOnMsgEventKey(act, frame.getBPId());
        
        if (deadline > 0) {
            bpit = new BusinessProcessInstanceThreadForPick(procMgr, engine, frame, deadline, 
                    events.length, this);
            // NOTE: Don't need to add the frame for inactive list as it will be added
            // when the on alarm bpit is added to ready to run queue.
            mPickTimerEventMap.put(key, bpit);
        }
        
        for (int i = 0; i < events.length; i++) {
            mPickRequestPendingInstances.put(events[i], frame);
        }
        
        // NOTE: even for multiple on messages defined, only one frame is added
        // to in active list as only one on message will be executed.
        if (events.length > 0) {
        	frame.getProcessInstance().addToInactiveList(frame, InactivityReason.PENDING_REQUEST_FOR_PICK);
        }

        mPickOnMsgEventMap.put(key, events);
        
        return bpit;
    }

    /**
     * called by the BusinessprocessInstanceThread and getPendingInstance to 
     * remove the pick from  the pending queue
     *
     * @param frame callframe
     * @return true if it successfully deleted the pick OnMessages. 
     */
    boolean removeFromPendingQueue(ICallFrame frame) {
        if(removeEntriesFromMapsAndGetBPIT(frame) != null){
            return true;
        }
        return false;
    }
    
    public BusinessProcessInstanceThread removeEntriesFromMapsAndGetBPIT (ICallFrame frame) {
        Activity act = frame.getProgramCounter().getStaticModelActivity();
        PickOnMsgEventKey key = new PickOnMsgEventKey(act, frame.getBPId());

        BusinessProcessInstanceThread bpit = null;
        
        synchronized (mProcMgr.getProcessLevelLock()) {
        	// remove events
        	InComingEventKeyImpl[] events = (InComingEventKeyImpl[]) mPickOnMsgEventMap.remove(key);
        	if (events != null) {
        		for (int i = 0; i < events.length; i++) {
        			ICallFrame cf = (ICallFrame) mPickRequestPendingInstances.remove(events[i]);
        			if (cf != null) {
        				frame.getProcessInstance().removeFromInactiveList(frame, false);
        			}
        		}
        	}
        	bpit = (BusinessProcessInstanceThread) mPickTimerEventMap.remove(key);
		}
        // remove & return BusinessProcessInstanceThread for deadline
        return bpit;
    }
    
    public Map getPickRequestPendingInstances() {
    	return mPickRequestPendingInstances;
    }

    private static class PickOnMsgEventKey {

        /** BPID */
        String mBPId;
        Activity mPick;

        /** hash code */
        int mHashCode;

        /**
         * Creates a new PickOnMsgEventKey object.
         *
         * @param pick Pick activity
         * @param bpId BPID
         */
        PickOnMsgEventKey(Activity pick, String bpId) {
            mBPId = bpId;
            mPick = pick;
            mHashCode = pick.hashCode();
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        public boolean equals(Object obj) {
            if (!(obj instanceof PickOnMsgEventKey)) {
                return false;
            }

            if (obj == this) {
                return true;
            }

            return (Utility.areEqual(mBPId, ((PickOnMsgEventKey) obj).mBPId))
                    && Utility.areEqual(mPick, ((PickOnMsgEventKey) obj).mPick);
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        public int hashCode() {
            return mHashCode;
        }
    }

	public void cleanUp(ICallFrame frame) {
		Activity act = frame.getProgramCounter().getStaticModelActivity();
		PickOnMsgEventKey key = new PickOnMsgEventKey(act, frame.getBPId());
		// remove events
		InComingEventKeyImpl[] events = (InComingEventKeyImpl[]) mPickOnMsgEventMap.remove(key);
		if (events != null) {
			for (int i = 0; i < events.length; i++) {
				mPickRequestPendingInstances.remove(events[i]);
			}
		}

		mPickTimerEventMap.remove(key);
	}
}
