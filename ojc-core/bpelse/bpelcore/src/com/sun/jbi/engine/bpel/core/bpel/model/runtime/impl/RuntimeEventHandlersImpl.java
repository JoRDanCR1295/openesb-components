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
 * @(#)RuntimeEventHandlersImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.RecoveredCallFrameImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnAlarmUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnEventUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.EventStateImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * @author Sun Inc
 * Sep 11, 2007
 */
public class RuntimeEventHandlersImpl implements RuntimeEventHandlers {
    /**
    The result of execution of event handlers could result in some callframes to be 
    waiting on the pending queue even after the event handler is complete. 
    If an onEvent or onAlarm gets triggered after the Event Handler is out of execution 
    context (not to be confused with Context interface), the execution goes into the 
    onEvent or onAlarm units and it determines that the scope is complete and aborts 
    execution. 
    There could be a situation when the onEvent or onAlarm doesn't get triggered, 
    in such a situation this would be considered a leak as the pendingQueue is not
    cleared. But the solution lies in the fact that instance is supposed to clean 
    data structures either when it is complete or when it is terminated.
    These frames in pendingQueue will be pulled out after the instance is complete.
    */
    
    /* indicates that the associated scope has completed and is waiting for the event handler to complete. */
    private boolean mToFinish;

    /*
     * Indicates if the object is initialized. Initialization occurs the first time the doAction()
     * method is called
     */
    private boolean mInitialized;

    private int mActiveEventCount = 0;

    // The lock for read/write mCreatedFrameMap and mActiveFrameMap
    private Object activeCounterLock = new Object();

    private Context mContext;
    private BPELProcessInstance mInstance;
    private EventHandlers mStaticModel;
    private long mBranchId;
    private String mAssociatedScopeId;
    private BPELInterpreter mInterp;
    private BPELProcessManager mBPELProcMgr;
    private Engine mEng;
    private Unit mParentActUnit;
    private ScopeOrProcessUnit mScopeOrUnit;
    private Set<ICallFrame> mEventCallFrames = new HashSet<ICallFrame>();
    private Set<ICallFrame> mAlarmCallFrames = new HashSet<ICallFrame>(); 
    
    /**
     * @param context
     * @param instance
     * @param scopeOrUnit
     * @param act
     * @param branchId
     * @param eng
     * @param interp
     * @param mgr
     * @param scopeId
     */
    public RuntimeEventHandlersImpl(Context context, BPELProcessInstance instance, ScopeOrProcessUnit scopeOrUnit, 
    		Unit parentActUnit, EventHandlers act, long branchId, Engine eng, BPELInterpreter interp, 
    		BPELProcessManager mgr, String scopeId) {

        mContext = context;
        mInstance = instance;
        mScopeOrUnit = scopeOrUnit;
        mParentActUnit = parentActUnit;
        mEng = eng;
        mInterp = interp;
        mBPELProcMgr = mgr;
        mStaticModel = act;
        mBranchId = branchId;
        mInitialized = false;
        mToFinish = false;
        mAssociatedScopeId = scopeId;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#startEventHandlers()
     */
    public void startEventHandlers() {

        synchronized (activeCounterLock) {
            if ((!mInitialized) && (!mToFinish)) {

                mInitialized = true;
                EventHandlersOnEvent[] allOnEvents = mStaticModel.getOnEvents();
                EventHandlersOnAlarm[] allOnAlarms = mStaticModel.getOnAlarms();
                if (allOnEvents != null) {
                    for (int i = 0; i < allOnEvents.length; i++) {
                        createAndSchedule(allOnEvents[i]);
                    }
                }
                if (allOnAlarms != null) {
                    for (int i = 0; i < allOnAlarms.length; i++) {
                        createAndSchedule(allOnAlarms[i]);
                    }
                }
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createAndSchedule(EventHandlersOnEvent)
     */
    public void createAndSchedule(EventHandlersOnEvent onEvent) {
        ICallFrame frame = createFrame(onEvent, null);
        synchronized (activeCounterLock) {
            if (!mToFinish) {
                mBPELProcMgr.addToReadyToRunQueue(frame);
            }
        }
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createFrameForRecovery(com.sun.bpel.model.EventHandlersOnEvent, String)
     */
    public ICallFrame createFrameForRecovery(EventHandlersOnEvent onEvent, String eveHdlrStateId) {
        return createFrame(onEvent, eveHdlrStateId);
    }
    
    private ICallFrame createFrame(EventHandlersOnEvent onEvent, String eveHdlrStateId) {
        EventHandlersOnEventUnit onEventUnit;
        Context ctx;
        ICallFrame childFrame;
        if (Utility.isEmpty(eveHdlrStateId)) {
            StateContext stateCtx = new StateContextImpl((RActivityHolder) onEvent);
            ctx = new ContextImpl(stateCtx);           
            onEventUnit = new EventHandlersOnEventUnitImpl(ctx, (RActivity) onEvent, this);
            childFrame = mInterp.createCallFrame(this.mBPELProcMgr.getBPELProcess(), this.mInstance, onEventUnit, ctx);
            mEventCallFrames.add(childFrame);
        } else {
            StateContext stateCtx = new StateContextImpl((RActivityHolder) onEvent, eveHdlrStateId);
            ctx = new ContextImpl(stateCtx);
            onEventUnit = new EventHandlersOnEventUnitImpl(ctx, (RActivity) onEvent, this);
            childFrame = new RecoveredCallFrameImpl(mBPELProcMgr.getBPELProcess(), mInterp, mInstance, onEventUnit, 0);
            mActiveEventCount++;
        }
        return childFrame;
    }

    private void createAndSchedule(EventHandlersOnAlarm onAlarm) {
        
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onAlarm);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnAlarmUnit onAlarmUnit = null;
        if (Utility.isRepeatEveryDefined(onAlarm)) {
            onAlarmUnit = new EventHandlersOnAlarmRepeatEveryUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        } else {
            onAlarmUnit = new EventHandlersOnAlarmUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        }
        scheduleOnAlarm(onAlarmUnit, ctx);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createFrameForRecovery(com.sun.bpel.model.EventHandlersOnAlarm)
     */
    public ICallFrame createFrameForRecovery(EventHandlersOnAlarm onAlarm) {
        
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onAlarm);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnAlarmUnit onAlarmUnit = null;
        if (Utility.isRepeatEveryDefined(onAlarm)) {
            onAlarmUnit = new EventHandlersOnAlarmRepeatEveryUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        } else {
            onAlarmUnit = new EventHandlersOnAlarmUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        }
        ICallFrame childFrame = mInterp.createCallFrame(mBPELProcMgr.getBPELProcess(), mInstance, onAlarmUnit, ctx);
        mAlarmCallFrames.add(childFrame);
        return childFrame;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createFrameForRecovery(com.sun.bpel.model.EventHandlersOnAlarm, java.lang.String, long, long)
     */
    public ICallFrame createFrameForRecovery(EventHandlersOnAlarm onAlarm, String eveHdlrStateId,
            long timerVal, long repeatEveryValue) {
        
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onAlarm, eveHdlrStateId);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnAlarmUnit onAlarmUnit = null;
        if (Utility.isRepeatEveryDefined(onAlarm)) {
            onAlarmUnit = new EventHandlersOnAlarmRepeatEveryUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, 
                    this, repeatEveryValue);
        } else {
            onAlarmUnit = new EventHandlersOnAlarmUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        }
        ICallFrame childFrame = new RecoveredCallFrameImpl(mBPELProcMgr.getBPELProcess(), mInterp, 
                timerVal, mInstance, onAlarmUnit, 0);
        mAlarmCallFrames.add(childFrame);
        return childFrame;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createOnEventUnitForRecovery(com.sun.bpel.model.EventHandlersOnEvent, java.lang.String)
     */
    public EventHandlersOnEventUnit createOnEventUnitForRecovery(EventHandlersOnEvent onEvent, String eveHdlrStateId) {
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onEvent, eveHdlrStateId);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnEventUnit onEventUnit = 
        	new EventHandlersOnEventUnitImpl(ctx, (RActivity) onEvent, this);
        mActiveEventCount++;
        return onEventUnit;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createOnAlarmUnitForRecovery(com.sun.bpel.model.EventHandlersOnAlarm, java.lang.String)
     */
    public EventHandlersOnAlarmUnit createOnAlarmUnitForRecovery(EventHandlersOnAlarm onAlarm, String eveHdlrStateId) {
        
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onAlarm, eveHdlrStateId);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnAlarmUnit onAlarmUnit = null;
        if (Utility.isRepeatEveryDefined(onAlarm)) {
            onAlarmUnit = new EventHandlersOnAlarmRepeatEveryUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        } else {
            onAlarmUnit = new EventHandlersOnAlarmUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this);
        }
        mActiveEventCount++;
        return onAlarmUnit;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#createAndSchedule(com.sun.bpel.model.EventHandlersOnAlarm, long)
     */
    public void createAndSchedule(EventHandlersOnAlarm onAlarm, long repeatWaitTime) {
        StateContext stateCtx = new StateContextImpl((RActivityHolder) onAlarm);
        Context ctx = new ContextImpl(stateCtx);
        EventHandlersOnAlarmUnit onAlarmUnit = 
            new EventHandlersOnAlarmRepeatEveryUnitImpl(ctx, mParentActUnit, (RActivity) onAlarm, this, repeatWaitTime);

        scheduleOnAlarm(onAlarmUnit, ctx);
    }

    private void scheduleOnAlarm(EventHandlersOnAlarmUnit onAlarmUnit, Context ctx) {
        ICallFrame childFrame = mInterp.createCallFrame(mBPELProcMgr.getBPELProcess(), mInstance, onAlarmUnit, ctx);
        mAlarmCallFrames.add(childFrame);
        mBPELProcMgr.addToReadyToRunQueueWithTimeout(0, childFrame);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#onEventStarts(com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct, InComingEventKeyImpl, ICallFrame)
     */
    public Object[] onEventStarts(EventHandlersChildAct onEventOronAlarm, 
            InComingEventKeyImpl waitingForEvent, ICallFrame callFrame) {
        Object[] retVal = new Object[2];
        synchronized (activeCounterLock) {
            if (mToFinish) {
            	mEventCallFrames.remove(callFrame);
                // The scope associated with this event handler unit is complete. hence return
                // true.
                retVal[0] = null;
                retVal[1] = true;
                return retVal;
            }
            
            retVal = mBPELProcMgr.receiveRequestOrPutInPendingQueue(waitingForEvent, callFrame);
            MessageContainer request = (MessageContainer) retVal[0];

            if (request == null) {
                // There is no message yet for the OnEvent to continue.
            	// Do not remove from the event call frames set in this case.
                return retVal;
            } else {
            	mEventCallFrames.remove(callFrame);
                mActiveEventCount++;
                EventHandlersOnEvent act = (EventHandlersOnEvent) onEventOronAlarm.getStaticModelActivity();
                createAndSchedule(act);
                return retVal;
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#taskCompletes(EventHandlersChildAct)
     */
    public boolean onAlarmStarts(EventHandlersChildAct onEventOronAlarm, ICallFrame callFrame) {
        synchronized (activeCounterLock) {
            if (mToFinish) {
            	mAlarmCallFrames.remove(callFrame);
                // The scope associated with this event handler unit is complete. hence return
                // false.
                return false;
            }
            mAlarmCallFrames.remove(callFrame);
            mActiveEventCount++;
            return true;
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#taskCompletes(EventHandlersChildAct)
     */
    public void taskCompletes(EventHandlersChildAct onEventOronAlarm, RequiredObjects rObjs) {
        synchronized (activeCounterLock) {
            mActiveEventCount--;
            // If the associated scope is to complete and it is time to complete it,
            // make it complete by adding the callframe to the runqueue.
            if (mToFinish && mActiveEventCount == 0) {
            	mBPELProcMgr.cleanUpEventHandlerCallFrames(mEventCallFrames, mAlarmCallFrames);
                mScopeOrUnit.eventHandlersComplete();
            }
        }
    }

    /**
     * When the business instance runs to complete, it checks whether the completion should be
     * deferred because the running onEvent/onAlarm callframes are not all completed.
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#associatedInstanceComplets()
     */
    public void associatedScopeCompletes() {

        synchronized (activeCounterLock) {
            // The associated scope is saying it is complete. 
            // IDEALLY we need to remove all the pending call frames right here. 

            // BUT The pending call frames either are
            // are removed at the end of the instance, when the instance completes.
            // Or when the onAlarm or onEvent is triggered, they would enter the corresponding
            // unit and then they will abort executing the unit.
            
            // TODO the following !mToFinish check seems unnecessary. verify this!!
            if (!mToFinish) {
                mToFinish = true;
                if (mActiveEventCount == 0) {
                    // This means that no OnEvents or OnAlarms are in progress. Hence the scope can
                    // complete and RuntimeEventHandlers can complete
                	mBPELProcMgr.cleanUpEventHandlerCallFrames(mEventCallFrames, mAlarmCallFrames);
                    mScopeOrUnit.eventHandlersComplete();
                }
            }

        }
    }


	private class ContextImpl implements Context {

        StateContext mStateCtx;
        ContextImpl(StateContext stateCtx) {
            mStateCtx = stateCtx;
        }
        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getFaultHandlingContext()
         */
        public FaultHandlingContext getFaultHandlingContext() {
            return mContext.getFaultHandlingContext();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getParentContext()
         */
        public Context getParentContext() {
            return mContext.getParentContext();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getProcessInstance()
         */
        public BPELProcessInstance getProcessInstance() {
            return mContext.getProcessInstance();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getStateContext()
         */
        public StateContext getStateContext() {
            return mStateCtx;
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
         */
        public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
            throw new UnsupportedOperationException();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initEHUponRecovery(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers)
         */
        public void initEHUponRecovery(RuntimeEventHandlers rEH) {
            throw new UnsupportedOperationException();
        }
        
        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariable(com.sun.bpel.model.meta.RVariable)
         */
        public RuntimeVariable getRuntimeVariable(RVariable variable) {
            return mContext.getRuntimeVariable(variable);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
         */
        public Map getRuntimeVariables() {
            return mContext.getRuntimeVariables();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
         */
        public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
            mContext.setRuntimeVariable(runtimeVariable);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#getRuntimePartnerLink(com.sun.bpel.model.PartnerLink)
         */
        public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
            return mContext.getRuntimePartnerLink(pLink);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#setRuntimePartnerLink(com.sun.bpel.model.PartnerLink, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink)
         */
        public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
            mContext.setRuntimePartnerLink(pLink, runtimePLink);
        }
        
        public Map getRuntimePartnerLinks() {
            return mContext.getRuntimePartnerLinks();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addRequest(com.sun.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
         */
        public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
            mContext.addRequest(rcv, req);
        }

        /**
         * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RStartElement, 
         * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
         */
        public void removeRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
             mContext.removeRequest(rcv, req);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(java.lang.String)
         */
        public void addToCRMPUpdateList(String updateValueKey) {
            mContext.addToCRMPUpdateList(updateValueKey);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(java.lang.String)
         */
        public boolean crmpUpdateListContains(String updateValueKey) {
            return mContext.crmpUpdateListContains(updateValueKey);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
         */
        public void declareDefaultMessageExchange() {
            mContext.declareDefaultMessageExchange();
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RReply)
         */
        public MessageContainer removeRequest(RReply reply) {
            return mContext.removeRequest(reply);
        }

        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(java.lang.Exception)
         */
        public void sendErrorsForPendingRequests(Exception error) {
        	throw new UnsupportedOperationException();
        }

        /**
         * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
         */
        public void completePendingInOnlyRequests() {
        	throw new UnsupportedOperationException();
        }
        
        /**
         * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#createRuntimeVariable(com.sun.bpel.model.meta.RVariable)
         */
        public RuntimeVariable createRuntimeVariable(RVariable variable) {
        	return mContext.createRuntimeVariable(variable);
        }
        
        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
         */
        public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
            return mContext.createRuntimePartnerLink(partnerlink);
        }
        
        /*
         * (non-Javadoc)
         * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getEnclosedScopesIterCount(long)
         */
        public long getEnclosedScopesIterCount(long scopeId) {
        	//mContext.getEnclosedScopesIterCount(scopeId);
        	// there can be only one child scope of the OnEvent child and it 
        	//cannot be repeated.
        	return 1;
        }
        
        public String getContextScopeId() {
        	// there are no variables on this context hence this method should not 
        	// be called on this context.
        	throw new UnsupportedOperationException();
        }

        public long getIteration() {
        	throw new UnsupportedOperationException();
        }
        
        public boolean isInserted() {
        	throw new UnsupportedOperationException();
        }
        
        public int getCompletionOrder() {
        	throw new UnsupportedOperationException();
        }

        public String getScopeState() {
        	throw new UnsupportedOperationException();
        }            

        public long getCompensateId() {
        	throw new UnsupportedOperationException();
        }
        
    	public String getParentScopeId() {
        	throw new UnsupportedOperationException();
    	}
		public WSMessage getFaultData() {
			throw new UnsupportedOperationException();
		}

		public QName getFaultName() {
			throw new UnsupportedOperationException();
		}

		public void markInserted() {
			throw new UnsupportedOperationException();
		}
		
		public long getFaultActivityId() {
			throw new UnsupportedOperationException();
	    }
	    
	    public void setFault(Fault fault, long faultActivityId) {
			throw new UnsupportedOperationException();
	    }
    }
    
    class StateContextImpl implements StateContext {

        MutableState mState;
        String mEveHdlrStateId;
        StateContextImpl(RActivityHolder onEvent) {
        	mEveHdlrStateId = BPELHelper.getGUID();
            init(onEvent, mEveHdlrStateId);
        }
        private void init(RActivityHolder onEvent, String eveHdlrStateId) {
            if (!mBPELProcMgr.isPersistenceEnabled()) {
                return;
            }
            
            long eventModelId = ((RActivity) onEvent).getUniqueId();
            mState = new EventStateImpl(mContext.getStateContext().getState(), mEng.getId(), eveHdlrStateId, 
            		mBranchId, mAssociatedScopeId, eventModelId);
        }
        
        StateContextImpl(RActivityHolder onEvent, String eveHdlrStateId) {
        	mEveHdlrStateId = eveHdlrStateId;
            init(onEvent, eveHdlrStateId);
        }
        
        /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext#getState()
         */
        public MutableState getState() {
            return mState;
        }
        
        public String getEventHandlerStateId() {
        	return mEveHdlrStateId;
        }
    }
}
