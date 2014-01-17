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

import java.util.List;

import com.sun.bpel.model.OperationReference;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.ROnAlarm;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.impl.RReplyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.PickUnitImpl.PickState;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState;

/**
 * @author Sun Inc
 * Aug 24, 2007
 */
public class PersistenceManager {
    private Engine mEng;
    private BPELProcessInstance mBPInstance;

    public PersistenceManager(Engine eng, BPELProcessInstance bpInstance) {
        mEng = eng;
        mBPInstance = bpInstance;
    }
    
    public void updateState(BPELProcessInstanceImpl unit, boolean isUnitDone, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        } 
        
        if (isUnitDone) {
            exitState(state);
        } else {
            //long scopeId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
        	String scopeId = FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID;
            state.enterScope(scopeId);
        }
    }
    
    /**
     * @param scopeUnit
     * @param state
     * @param isUnitDone
     */
    protected void updateState(ScopeUnitImpl scopeUnit, MutableState state,
            boolean isUnitDone) {
        
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        if (isUnitDone) {
            //state.exitScope(((ScopingElement) scopeUnit.getStaticModelActivity()).getScopeId());
        	state.exitScope(scopeUnit.getScopeGuid());
        } else {
            //state.enterScope(((ScopingElement) scopeUnit.getStaticModelActivity()).getScopeId());
        	state.enterScope(scopeUnit.getScopeGuid());        	
        }
    }
    
    /**
     * This is called for the two-way invoke ME, where the response is a fault.
     * The scope context is updated with the Fault object and the fault activity id
     * and persisted.  
     * @param scopeUnit
     * @param state
     * @param fault
     * @param faultActivityId
     * @param branchId
     */
    protected void updateState(Context context, Fault fault, 
    		long faultActivityId, long branchId) {
    	if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
    		return;
    	}
    	context.getFaultHandlingContext().setFault(fault, faultActivityId);
        //StateManager mgr = mEng.getStateManager();
        MutableState state = context.getStateContext().getState();
        //state.updatePC(branchId, faultActivityId); //update the pc with invoke value.
        state.updateScope(branchId, context.getFaultHandlingContext());
        //mgr.persistState((State) state, TransactionInfo.getLocalTxInfo());
    }
    
    /** This is called only from the doAction of the ScopeUnit. Is not called from
     * ProcessUnit. Is not called from Event handlers after the updateState in event handlers.
     * 
     * @param scopeUnit
     * @param state
     * @param txInfo
     */
    protected void persistState(ScopeUnitImpl scopeUnit, MutableState state) {
        
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        RActivity activity = scopeUnit.getStaticModelActivity();
        Scope scopeAct = (Scope) activity;
        boolean hasOnEvents = (scopeAct.getEventHandlers() != null 
                && scopeAct.getEventHandlers().getOnEvents() != null 
                && scopeAct.getEventHandlers().getOnEvents().length > 0);
        boolean hasOnAlarms = (scopeAct.getEventHandlers() != null
                && scopeAct.getEventHandlers().getOnAlarms() != null
                && scopeAct.getEventHandlers().getOnAlarms().length > 0);

        if (hasOnEvents || hasOnAlarms) {
            state.updatePC(scopeUnit.getBranchId(), activity.getUniqueId());
            StateManager mgr = mEng.getStateManager();
            mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
        }
    }
    
    public void updateState(BPELProcessInstanceImpl unit, boolean isUnitSuspended, boolean isUnitResumed) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }else if (isUnitSuspended) {
        	State suspendState = StateFactory.getStateFactory().createSuspendResumeState(mEng.getId(), unit.getBPELProcessManager().getBPELProcess().getBPELId(), 
        			unit.getId(), State.Mode.SUSPEND);
            StateManager mgr = mEng.getStateManager();
            mgr.persistState(suspendState, TransactionInfo.getLocalTxInfo(), mBPInstance);        	
        }else if (isUnitResumed) {
           	State resumeState = StateFactory.getStateFactory().createSuspendResumeState(mEng.getId(), unit.getBPELProcessManager().getBPELProcess().getBPELId(), 
        			unit.getId(), State.Mode.RESUME);
            StateManager mgr = mEng.getStateManager();
            mgr.persistState(resumeState, TransactionInfo.getLocalTxInfo(), mBPInstance);        	        	
        }
    }

    public void updateState(ExitUnitImpl unit, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        exitState(state);
    }
    
    private void exitState(MutableState state) {
        //long scopeId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    	String scopeId = FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID;
        state.exitScope(scopeId);
        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
    }
    public void updateState(EventHandlersOnEventUnitImpl unit, RequiredObjects rObjs, TransactionInfo txInfo, 
            MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        RActivity activity = unit.getStaticModelActivity();
        
        state.updatePC(unit.getBranchId(), activity.getUniqueId());
        specificUpdateStateForStartElement(rObjs, (RStartElement) activity, txInfo, state);
    } 

    public void updateState(ReceiveUnitImpl unit, RequiredObjects rObjs, TransactionInfo txInfo, 
            MutableState state, long branchInvokeCounter) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        RActivity activity = unit.getStaticModelActivity();
        
        state.updatePCWithBranchInvokeCounter(unit.getBranchId(), activity.getUniqueId(), 
        		branchInvokeCounter);
        specificUpdateStateForStartElement(rObjs, (RStartElement) activity, txInfo, state);
    }

    public void updateState(ReceiveUnitImpl unit, RequiredObjects rObjs, TransactionInfo txInfo, MutableState state, long branchInvokeCounter, String messageExchangeId) {
      RActivity activity = unit.getStaticModelActivity();
      state.updatePCWithBranchInvokeCounter(unit.getBranchId(), activity.getUniqueId(), branchInvokeCounter);
      
       String crmpInvId = (String) rObjs.removeValue(RequiredObjects.CRMP_INVOKE_ID);
        if (crmpInvId != null) {
            RStartElement startElement =  (RStartElement)activity;
            
            String partnerLink = startElement.getRPartner().getName();
            String operation = ((OperationReference) startElement).getOperation();
            
            state.updateCRMPState(crmpInvId, partnerLink, operation, messageExchangeId);
        }

        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, txInfo, mBPInstance);
    }

    private void specificUpdateStateForStartElement(RequiredObjects rObjs, RStartElement startElement, 
            TransactionInfo txInfo, MutableState state) {
        String crmpInvId = (String) rObjs.removeValue(RequiredObjects.CRMP_INVOKE_ID);
        if (crmpInvId != null) {
            String partnerLink = startElement.getRPartner().getName();
            String operation = ((OperationReference) startElement).getOperation();
            //TODO: (BB) It seems that next line contains error, because startElement is a class of bpelmodel project,
            //which contains setMessageExchange method but doesn't provide it through any interfaces and never uses (Make "find usages" setMessageExchange of com.sun.bpel.model.impl.* classes). 
            //so, the messageExchange will be null in any case. Please see where is a bug.
            String bpelMesgExchange = startElement.getMessageExchange();
            
            state.updateCRMPState(crmpInvId, partnerLink, operation, bpelMesgExchange);
        }

        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, txInfo, mBPInstance);
    }

    public void updateState(PickUnitImpl unit, PickState currentPickState, 
            ROnAlarm onAlrm, long waittime, ROnMessage onMesg, RequiredObjects rObjs,
            TransactionInfo txInfo, MutableState state, long branchInvokeCounter) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        if (currentPickState == PickState.WaitingForRequest && onAlrm == null) {
            // nothing to update the state.
            return;
        }
        
        if ((currentPickState == PickState.WaitingForRequest) && (onAlrm != null)) {
            // This means that the pick has to wait for an onAlarm to trigger
            // its execution
            state.updatePCWithPickCompositeId(unit.getBranchId(), 
                    unit.getStaticModelActivity().getUniqueId(), waittime, onAlrm.getUniqueId(), 
                    branchInvokeCounter);
        } else if (currentPickState == PickState.RequestReceived) {
            state.updatePCWithPickCompositeId(unit.getBranchId(), 
                    unit.getStaticModelActivity().getUniqueId(), onMesg.getUniqueId(), 
                    branchInvokeCounter);
        }
        specificUpdateStateForStartElement(rObjs, (RStartElement) onMesg, txInfo, state);
    }

    public void updateState(ReplyUnitImpl unit, RequiredObjects rObjs, 
            boolean updateCRMP, BPELProcessInstanceImpl instImpl, MutableState state,
            long branchInvokeCounter) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        RActivity activity = unit.getStaticModelActivity();
        state.updatePCWithBranchInvokeCounter(unit.getBranchId(), activity.getUniqueId(), 
        			branchInvokeCounter);
        if (updateCRMP) {
            // conditionaly check to see if the update of the CRMPState table with the response object should
            // be added to this persistence point.
            String bpId = instImpl.getId();

            RReplyImpl reply = (RReplyImpl) activity;
            String partnerLink = reply.getRPartner().getName();
            String operation = reply.getOperation();
            String crmpUpdateListValue = bpId + partnerLink + operation;
            boolean updateCRMPFlagFromInstance = instImpl
            .crmpUpdateListContains(crmpUpdateListValue);
            if (updateCRMPFlagFromInstance) {
                String bpelMsgExchange = reply.getMessageExchange();
                long replyVarId = reply.getRVariable().getUniqueId();
                RuntimeVariable runtimeVar = unit.getContext().getRuntimeVariable(reply
                        .getRVariable());
                Object serialVar = runtimeVar.getSerializedValue();

                state.updateCRMPState(partnerLink, operation, bpelMsgExchange,
                        replyVarId, serialVar.toString().toCharArray());
            }
        }
        StateManager mgr = mEng.getStateManager();
        
        mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
        
    }
    
    public void updateState(WaitUnitImpl unit, long waitTime, MutableState state, 
    		long branchInvokeCounter) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        state.updatePC(unit.getBranchId(), unit.getStaticModelActivity().getUniqueId(), 
        		waitTime, branchInvokeCounter);
        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
    }
    
    public void updateState(InvokeUnitImpl unit, long branchInvokeCounter, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }

        // for all the updates the branch invoke counter is set correctly on the MutableState
        // from the correct and updated value on the frame.
        state.updatePCWithBranchInvokeCounter(unit.getBranchId(), unit.getStaticModelActivity().getUniqueId(),
        		branchInvokeCounter);
    }
    
    public void updateState(FlowUnitImpl unit, List branchIds, MutableState state, boolean isRecoveredUnit,
    			long branchInvokeCounter) {

    	if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
    		return;
    	}

    	//Persist only if the start activity is done and some variables or scopes
    	//states have changed.
    	if ((!isRecoveredUnit) && (unit.getContext().getProcessInstance().isStartActivityDone())) {

    		state.updatePCWithBranchInvokeCounter(unit.getBranchId(), unit.getStaticModelActivity().getUniqueId(), 
    					branchInvokeCounter);
    		StateManager mgr = mEng.getStateManager();
    		mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
    	}

    	state.enterFlow(unit.getStaticModelActivity().getUniqueId(), branchIds);
    }
    
    public void cancelXA(TransactionInfo transactionInfo, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        doXA(transactionInfo, state);
    }
    
    public void endXA(TransactionInfo transactionInfo, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        doXA(transactionInfo, state);
    }
    
    public void startXA(TransactionInfo transactionInfo, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        doXA(transactionInfo, state);
    }
    
    private void doXA(TransactionInfo transactionInfo, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, transactionInfo, mBPInstance);
    }
    
    public void finishPersistence(MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        ((State) state).clearPC();
    }
    
    public void updateState(StateContext statCtx, RuntimeVariable variable, long branchId) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        MutableState state = statCtx.getState();
        state.updateVariable(branchId, variable);
    }
    
    public void updateState(StateContext statCtx, RuntimePartnerLink pLink, long branchId) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        MutableState state = statCtx.getState();
        state.updatePartnerLink(branchId, pLink);
    }
    
    public void updateState(EventHandlersOnAlarmUnitImpl unit, long waitTime, MutableState state) {
        updateState(unit, waitTime, null, state);
    }
    
    public void updateState(EventHandlersOnAlarmRepeatEveryUnitImpl unit, long waitTime, 
            long repeatEveryVal, MutableState state) {
        updateState(unit, waitTime, new Long(repeatEveryVal), state);
    }
    
    private void updateState(EventHandlersOnAlarmUnitImpl unit, long waitTime, 
            Long repeatEveryVal, MutableState state) {
        if (!mBPInstance.getBPELProcessManager().isPersistenceEnabled()) {
            return;
        }
        RActivity activity = unit.getStaticModelActivity();
        
        state.updatePC(unit.getBranchId(), activity.getUniqueId());
        
        ((MutableEventState) state).updateTimeVal(waitTime, repeatEveryVal);
        StateManager mgr = mEng.getStateManager();
        mgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), mBPInstance);
    }
}
