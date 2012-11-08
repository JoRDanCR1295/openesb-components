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
 * @(#)EventHandlersOnAlarmUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnAlarmUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeEventHandlersImpl.StateContextImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * The Impl class for EventHandlersOnAlarmUnit
 * 
 * @author Sun Microsystems.
 */
public class EventHandlersOnAlarmUnitImpl extends StructuredActivityUnitImpl implements
        EventHandlersOnAlarmUnit {

    private static final Logger LOGGER = Logger.getLogger(EventHandlersOnAlarmUnitImpl.class.getName());
	
    private boolean mIsWaiting = false;
    protected RuntimeEventHandlers mEh;
    private long mWaitTime = -1; 
    private String mUID;
    
    /**
     * Creates a new EventHandlersOnEventUnitImpl object.
     * @param act activity
     * @param branchId branch ID
     * @param eh TODO
     */
    public EventHandlersOnAlarmUnitImpl(Context context, Unit parentUnit, RActivity act, RuntimeEventHandlers eh) {
        super(context, parentUnit, act, act.getUniqueId());
        mEh = eh;
        mUID = ((StateContextImpl)context.getStateContext()).getEventHandlerStateId();
        // Expression is evaluated here, so if it fails fault is handled by the enclosing scope
        mWaitTime = computeWaitTime((EventHandlersOnAlarm) act, context);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {

        if (mIsWaiting) {
        	
        	if(mContext.getFaultHandlingContext().isBeingTerminated()) {
        		return true;
        	}
        	
            mIsWaiting = false;
            boolean scopeCompleted = true;
            
            //TODO there is a boundary condition wherein for the <for> or <until> defined with 0 or -ve 
            //(past duration) and the main process has activity that completes very fast (before this 
            //doAction gets called), the associated scope with on alarm event handler may not complete 
            //(canGoAhead will return false), but the spec mandates execution of scope with 0 or -ve time 
            //(past time). This need to be addressed.
            
            if (canGoAhead(frame, rObjs)) {
                scopeCompleted = executeScope(frame, bpit, rObjs);
            }
            
            return scopeCompleted;
        }
        mIsWaiting = true;
        
        markActivityStarted(frame, rObjs, bpit);
        
        frame.getProcessInstance().getPersistenctMgr().updateState(this, mWaitTime, 
                mContext.getStateContext().getState());
        rObjs.getBPELProcessManager().addToReadyToRunQueueWithTimeout(mWaitTime, frame);

        return false;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    @Override
    public boolean doActionOnRecovery(RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        long waitTime = frame.getTimerVal();
        frame.convert();
        
		if (frame.getPC() != null) {
			if (LOGGER.isLoggable(Level.FINE)) {
        		LOGGER.log(Level.FINE, 
        				I18n.loc("BPCOR-3070: Recovery started from activity : {0}", frame.getPC().getName()));
        	}
		}
		
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
            frame.onActivityTerminate(this);
            return true;
		}
        
        if (waitTime > System.currentTimeMillis()) {
            return waitAfterRecovery(waitTime, rObjs, frame);
        } else {
            boolean scopeCompleted = true;
            if (canGoAhead(frame, rObjs)) {
                scopeCompleted = executeScope(frame, bpit, rObjs);
            }            
            return scopeCompleted;
        }        
    }
    
    protected boolean waitAfterRecovery(long waitTime, RequiredObjects rObjs, ICallFrame frm) {
        mIsWaiting = true;
        rObjs.getBPELProcessManager().addToReadyToRunQueueWithTimeout(waitTime, frm);
        return false;
    }

    /**
     * @param frame
     * @param rObjs
     * @return
     */
    protected boolean canGoAhead(ICallFrame frame, RequiredObjects rObjs) {
        boolean goAhead = getRuntimeEventHandlers().onAlarmStarts(this, frame);
        if (!goAhead) {
            markActivityComplete(frame, rObjs, false);
            return false;
        }
        return true;
    }

    /**
     * @param frame
     * @param rObjs
     * @param associatedScopeCompleted
     */
    protected void markActivityComplete(ICallFrame frame, RequiredObjects rObjs, boolean associatedScopeCompleted) {
    	if(associatedScopeCompleted) {
            getRuntimeEventHandlers().taskCompletes(this, rObjs);    		
    	}

        frame.setProgramCounter(this);
        frame.onActivityComplete(this);
        frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
    }
    
    protected void markActivityStarted(ICallFrame frame, RequiredObjects rObjs, 
            BusinessProcessInstanceThread bpit) {
        frame.setProgramCounter(this);
        frame.onLineChange(this);
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);
    }

    private static long computeWaitTime(EventHandlersOnAlarm onAlarm, Context ctx) {
        // Start the timer
        boolean isFor = true;
        long waitTime = -1;
        String forUntilQuery = onAlarm.getFor();

        // If it is not <for>, it is <until>
        if (Utility.isEmpty(forUntilQuery)) {
            forUntilQuery = onAlarm.getUntil();
            isFor = false;
        }
        if (!Utility.isEmpty(forUntilQuery)) {
            Date date = Utility.getDateFromExpr(onAlarm, ctx, forUntilQuery, isFor);
            waitTime = date.getTime();
        }
        return waitTime;
    }

    /**
     * @param frame
     * @param bpit
     * @param objs
     * @return
     * @throws Exception
     */
    protected boolean executeScope(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects objs) throws Exception {
    	
        RActivity childAct = getChildActivity();
        ActivityUnit scopeUnit = ActivityUnitFactory.getInstance().createActivityUnit(mContext,
                this, childAct, getBranchId());
        
		if(objs.getBPELProcessManager().isPersistenceEnabled()) {
			((MutableEventState)mContext.getStateContext().getState()).setAssociatedScopeGuid(
					((FaultHandlingContext)scopeUnit).getScopeGuid());
		}
		
        boolean scopeCompleted = executeChildActivities(frame, bpit, objs, scopeUnit);
        if (scopeCompleted) {
            markActivityComplete(frame, objs, true);
        }
        return scopeCompleted;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct#getRuntimeEventHandlers()
     */
    public RuntimeEventHandlers getRuntimeEventHandlers() {
        return mEh;
    }

    /**
     * @see StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        return ((RActivityHolder) mAct).getChildActivity();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doPassControlToParent(
     * ICallFrame, BusinessProcessInstanceThread, RequiredObjects)
     */
    protected boolean doResumeAction(ActivityUnit childActUnit,
                                            ICallFrame frame,
                                            BusinessProcessInstanceThread bpit,
                                            RequiredObjects rObjs) throws Exception {
    	
        // Set the program counter, so that when BPELInterpreter checks the current 
    	// program counter when the doAction completes,
    	// TODO: Do we still need this
        frame.setProgramCounter(this);
        markActivityComplete(frame, rObjs, true);
        return true;
    }
    

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct#getUID()
	 */
	public String getUID() {
		return mUID;
	}
    
}
