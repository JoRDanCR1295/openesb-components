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
 * @(#)EventHandlersOnAlarmRepeatEveryUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Date;

import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * @author Sun Microsystems
 */
public class EventHandlersOnAlarmRepeatEveryUnitImpl extends EventHandlersOnAlarmUnitImpl {

    private long mRepeatEveryValue;
    private enum ExecutionState {WaitOn_ForOrUnitl, CompletedForOrUntil, WaitOn_RepeatUntil};
    private ExecutionState mState;
    private boolean mIsForOrUntilDefined = false;
    
    /**
     * Constructor
     * 
     * @param context
     * @param act
     * @param eh TODO
     */
    public EventHandlersOnAlarmRepeatEveryUnitImpl(Context context, Unit parentUnit, RActivity act,
            RuntimeEventHandlers eh) {
        super(context, parentUnit, act, eh);
        Date date = Utility.getDateFromExpr((EventHandlersOnAlarm) act, context, 
                ((EventHandlersOnAlarm) mAct).getRepeatEvery(), true);
        long repeatEveryValue = date.getTime();
        // the time for which this action needs to be repeated.
        mRepeatEveryValue = (repeatEveryValue - System.currentTimeMillis());
        mIsForOrUntilDefined = isForOrUntilDefined((EventHandlersOnAlarm) mAct);
    }

    public EventHandlersOnAlarmRepeatEveryUnitImpl(Context context, Unit parentUnit, RActivity act,
            RuntimeEventHandlers eh, long repeatEveryValue) {
        super(context, parentUnit, act, eh);
        mState = ExecutionState.CompletedForOrUntil;
        mRepeatEveryValue = repeatEveryValue;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#doAction(
     * 	com.sun.jbi.engine.bpel.core.bpms.bpel.runtime.ICallFrame,
     * 	com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     * 	com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doAction(ICallFrame frame,
                            BusinessProcessInstanceThread bpit,
                            RequiredObjects rObjs) throws Exception {
        if (mIsForOrUntilDefined) {
            if (mState == null) {
                mState = ExecutionState.WaitOn_ForOrUnitl;
                return super.doAction(frame, bpit, rObjs);
            } else if (mState == ExecutionState.WaitOn_ForOrUnitl) {
                mState = ExecutionState.CompletedForOrUntil;
                getRuntimeEventHandlers().createAndSchedule((EventHandlersOnAlarm) mAct, mRepeatEveryValue);
                return super.doAction(frame, bpit, rObjs);
            }
        } else {
            if (mState == null) {
                mState = ExecutionState.CompletedForOrUntil;
                getRuntimeEventHandlers().createAndSchedule((EventHandlersOnAlarm) mAct, mRepeatEveryValue);
                return true;
            } else if (mState == ExecutionState.CompletedForOrUntil) {       
                mState = ExecutionState.WaitOn_RepeatUntil;
                markActivityStarted(frame, rObjs, bpit);
                frame.getProcessInstance().getPersistenctMgr().updateState(this, 
                        System.currentTimeMillis(), mRepeatEveryValue,
                        mContext.getStateContext().getState());
                rObjs.getBPELProcessManager().addToReadyToRunQueueWithTimeout(System.currentTimeMillis() + mRepeatEveryValue, frame);
                return false;
            } else {
                // (mState == ExecutionState.WaitOn_RepeatUntil) {
                // the execution state should be ExecutionState.WaitOn_RepeatUntil
                return handleOnAlarmRepeatEvery(frame, bpit, rObjs);
            }
        }
        // Code will never be here.
        throw new RuntimeException();
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    @Override
    public boolean doActionOnRecovery(RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
    	
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.convert();
            frame.onActivityTerminate(this);
            return true;
		}
    	
        if (mIsForOrUntilDefined) {
            long waitTime = frame.getTimerVal();
            if (waitTime > System.currentTimeMillis()) {
                mState = ExecutionState.WaitOn_ForOrUnitl;
                ICallFrame frm = (ICallFrame) frame.convert();
                return super.waitAfterRecovery(waitTime, rObjs, frm);
            } else {
                mState = ExecutionState.CompletedForOrUntil;
                getRuntimeEventHandlers().createAndSchedule((EventHandlersOnAlarm) mAct, mRepeatEveryValue);
                return super.doActionOnRecovery(frame, bpit, rObjs);
            }
        } else {
            ICallFrame frm = (ICallFrame) frame.convert();
            long waitTime = frame.getTimerVal();
            long remainingWaitTime = mRepeatEveryValue - (System.currentTimeMillis() - waitTime);
            if (remainingWaitTime <= 0) {
                // If only repeat Every is defined, and then the repeatEvery time 
                // has passed in the recovery time of engine
                return handleOnAlarmRepeatEvery(frm, bpit, rObjs);
            } else {
                // launch the first repeatEvery for the remaining time.
                mState = ExecutionState.WaitOn_RepeatUntil;
                rObjs.getBPELProcessManager().addToReadyToRunQueueWithTimeout(System.currentTimeMillis() 
                        + remainingWaitTime, frm);
                return false;
            }
        }
    }

    /**
     * Handle On Alarm repeat every only
     * 
     * @param frame
     * @param bpit
     * @param rObjs
     * @return
     */
    private boolean handleOnAlarmRepeatEvery(ICallFrame frame,
                                               BusinessProcessInstanceThread bpit,
                                               RequiredObjects rObjs) throws Exception {
        
        
        if(mContext.getFaultHandlingContext().isBeingTerminated()) {
            return true;
        }
        
        boolean scopeCompleted = true;
        if (canGoAhead(frame, rObjs)) {
            // since the associated scope need to be executed repeatedly, till the main process
            // in the enclosing scope completes, put the callframe back to the ready to run queue 
            // for repeated execution
            getRuntimeEventHandlers().createAndSchedule((EventHandlersOnAlarm) mAct, mRepeatEveryValue);
            scopeCompleted = executeScope(frame, bpit, rObjs);
        }
        return scopeCompleted;
    }

    private static boolean isForOrUntilDefined(EventHandlersOnAlarm onAlarm) {
        String forUntilQuery = onAlarm.getFor();
        if (!Utility.isEmpty(forUntilQuery)) {
            return true;
        } else {
            forUntilQuery = onAlarm.getUntil();
        }

        if (!Utility.isEmpty(forUntilQuery)) {
            return true;
        }
        return false;
    }
}
