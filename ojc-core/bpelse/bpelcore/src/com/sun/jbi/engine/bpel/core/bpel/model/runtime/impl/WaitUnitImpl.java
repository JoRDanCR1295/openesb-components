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
 * @(#)WaitUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.Wait;
import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.TimerActUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Wait activity unit implementation
 *
 * @author Sun Microsystems
 */
public class WaitUnitImpl extends ActivityUnitImpl implements TimerActUnit {
	
	private static final Logger LOGGER = Logger.getLogger(WaitUnitImpl.class.getName());
	
    /**
     * toggling of this field is required to support while loop. in a while loop, an activity will
     * be called many times, and as many times, execution enters and exits the particular
     * activity.
     */
    private boolean mWaiting = false;

    /**
     * Creates a new WaitUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public WaitUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
    }

    /**
     * @see ActivityUnitImpl#doAction(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {

        if (mWaiting) {
        	if(mContext.getFaultHandlingContext().isBeingTerminated()){
        		frame.onActivityTerminate(this);
        		return doPassControlToParent(frame, bpit, rObjs);
        	} else {
                mWaiting = false;
                frame.onActivityComplete(this);                
                frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
                return doPassControlToParent(frame, bpit, rObjs);
        	}
        } else {
            frame.setProgramCounter(this);            
            frame.onLineChange(this);
            BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
            frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	
            
            boolean waitCompleted = doWait(frame, rObjs);
            if (waitCompleted) {
            	BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
               frame.onActivityComplete(this);
               frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
               return true;
            } else {
            	return false;
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doActionOnRecovery(RecoveredCallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doActionOnRecovery(RecoveredCallFrame frame, 
                                      BusinessProcessInstanceThread bpit, 
                                      RequiredObjects rObjs) throws Exception {
		frame.convert();
		
		if (frame.getPC() != null) {

			if (LOGGER.isLoggable(Level.FINE)) {
				LOGGER.log(Level.FINE, 
						I18n.loc("BPCOR-3070: Recovery started from activity : {0}", frame.getPC().getName()));
			}
		}
		
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return doPassControlToParent(frame, bpit, rObjs);
		}
    	
        boolean waitCompleted = doWaitOnRecovery(frame, rObjs);
        if (waitCompleted) {
            frame.onActivityComplete(this);
            frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
			return doPassControlToParent(frame, bpit, rObjs);
        } else {
        	return false;
        }
    }

    private boolean doWait(ICallFrame frame, RequiredObjects rObjs) throws Exception {
    	
        Wait wait = (Wait) mAct;
        boolean isFor = true;
        String expr = wait.getFor();

        // If it is not <for>, it is <until>
        if (expr == null) {
            expr = wait.getUntil();
            isFor = false;
        }
        
        Date date = Utility.getDateFromExpr(mAct, mContext, expr, isFor);
        long waitTime = date.getTime();

        if (waitTime > System.currentTimeMillis()) {
        	// Do persistence only if wait is a <for/>, since until is absolute in time. 
            // Even if there was a crash while the instance was waiting in case of until, 
            // recovery starts from the previous point and evaluate the until expression.
        	if (isFor) {
                frame.getProcessInstance().getPersistenctMgr().updateState(this, waitTime, 
                        mContext.getStateContext().getState(), frame.getBranchInvokeCounter());
        	}
            mWaiting = true;
            rObjs.getBPELProcessManager().addToReadyToRunQueueWithTimeout(waitTime, frame);
        	return false;
        }  else {
        	return true;
        }
    }

    private boolean doWaitOnRecovery(RecoveredCallFrame frame, RequiredObjects rObjs) {
        long waitTime = frame.getTimerVal();
        if (waitTime > System.currentTimeMillis()) {
            rObjs.getBPELProcessManager()
            	.addToReadyToRunQueueWithTimeout(waitTime, frame);
            mWaiting = true;
            return false;
        }

        return true;
    }
}
