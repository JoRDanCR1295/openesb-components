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
 * @(#)EventHandlersOnEventUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.Status;
import javax.transaction.Synchronization;

import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.REventHandlersOnEventImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnEventUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeEventHandlersImpl.StateContextImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * The Impl class for EventHandlersOnEventUnit
 * @author Sun Microsystems
 *
 */
public class EventHandlersOnEventUnitImpl extends StructuredActivityUnitImpl implements
        EventHandlersOnEventUnit {

    private static final Logger LOGGER = Logger.getLogger(EventHandlersOnEventUnitImpl.class.getName());
    
	private InComingEventKeyImpl mWaitingForEvent;
    private RuntimeEventHandlers mEh;
    private enum OnEventState {Initial, Running, WaitingForRequest, WaitingForTxComplete};
    private OnEventState currentOnEventState = OnEventState.Initial;
    private ActivityUnit mEnclosingActivity = null;
    
    private TxSynchronizer txSynchronizer;
    private Unit mVirtualThrowUnit = null;
    private String mUID;
    
    /**
     * Creates a new EventHandlersOnEventUnitImpl object.
     * @param act activity
     * @param branchId branch ID
     * @param eh TODO
     */
    public EventHandlersOnEventUnitImpl(Context context, RActivity act, RuntimeEventHandlers eh) {
        super(context, null, act, act.getUniqueId());
        mEh = eh;
        mUID = ((StateContextImpl)context.getStateContext()).getEventHandlerStateId();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {

    	RStartElement onEvent = (RStartElement) mAct;
        
    	if (currentOnEventState == OnEventState.Initial) {
    		mEnclosingActivity = ActivityUnitFactory.getInstance().createActivityUnit(
    		        mContext, this, getChildActivity(), getBranchId());
    		Context childScope = (ScopeUnitImpl) mEnclosingActivity;
    		childScope.declareDefaultMessageExchange();
    		mContext = childScope;
    		
        	frame.setProgramCounter(this);    		
    		frame.onLineChange(this);
    		
    		if(rObjs.getBPELProcessManager().isPersistenceEnabled()) { 			
        		((MutableEventState)mContext.getStateContext().getState()).setAssociatedScopeGuid(
        				((FaultHandlingContext)childScope).getScopeGuid());

                //Since this is already being called on the associated scope,
                //we won't do the first updateScope in ScopeUnitImpl 
                mContext.getStateContext().getState().updateScope(getBranchId(), (FaultHandlingContext)childScope);  
    		}

   			mWaitingForEvent = Utility.createSAInComingEventKeyImpl(onEvent, frame, rObjs.getBPELProcessManager());

            currentOnEventState = OnEventState.Running;
    	} else if (this.currentOnEventState== OnEventState.WaitingForTxComplete) {

    		frame.getProcessInstance().getPersistenctMgr().finishPersistence(mContext.getStateContext().getState());
    		frame.getProcessInstance().getClusterMgr().decrementActiveMessagingActivitiesCount(frame);

            if (txSynchronizer.txStatus == Status.STATUS_COMMITTED) {

            	// We don't need to update the state here
                mContext.setRuntimeVariable(txSynchronizer.runtimeVariable);

                // calculate the correlations, verify it and set it on the instance and the state obj.
				// the next persistence point will persist it. Any CorrelationViolation exception will 
				// be handled by the BPELInterpreter.execute() and thrown as a VirtualThrow unit.
				// if it had crashed before this point, recovery will recover this StartActivityUnit and
				// recalculate and check the integrity of the calculations
                if (onEvent.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {

            		BPELProcessInstance bpInstance = frame.getProcessInstance();
            		CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
            		CorrelationDefnWrapper corrDefnWrap = onEvent.getCorrelationDefnWrapper();
            		WSMessage wsMesg = txSynchronizer.runtimeVariable.getWSMessage();
            		// calculate the correlation values
            		CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMesg, 
            				corrDefnWrap, bpInstance); 
            		// associate the values with the instance. this method will also check
            		// message integrity and add the values to the state object.
            		corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, bpInstance);        
            	}  

                frame.onActivityComplete(this);
                frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);       
                return executeChildAct(frame, bpit, rObjs);
            } else {
                // For any other status other than STATUS_COMMITTED, we should consider
                // that the Tx has failed
                LOGGER.log(Level.WARNING, 
                		I18n.loc("BPCOR-6049: The XA Transaction failed. The Transaction Manager sent a status {0}", 
                				txSynchronizer.txStatus));
                cancelTransaction(frame, rObjs);
                return false;
            }
    	} else {
			if (!this.equals(frame.getProgramCounter())) {
				throw new RuntimeException(Utility.createIllegalPCChangeMessage(this, mAct, frame));
			}

			if(mContext.getFaultHandlingContext().isBeingTerminated()) {
				return true;
			}
        }
         
    	// Check to see if we have a message. But mark the task as active first and check if we can proceed.
    	Object[] val = getRuntimeEventHandlers().onEventStarts(this, mWaitingForEvent, frame);
    	Boolean terminateOnEvent = (Boolean) val[1];
    	if (terminateOnEvent) {
    		//Either the associated scope completed or the onEvent was terminated. In either case,
    		//the onEvent was prematurely terminated.
    		frame.onActivityTerminate(this);
    		//TODO: This is not a normal completion of onEvent, so we should not call postActivityCompleteEvent() here
    		//frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this, rObjs, bpit.getCallFrame().getBPId());            
    		return true; 
    	}

    	//Message was found
    	MessageContainer request = (MessageContainer) val[0];

		if (request != null) {
    		frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);		
            RStartElement receive = (RStartElement) mAct;
			//Create another eventHandler before we consume the message.
			postVarEvent(((REventHandlersOnEventImpl) mAct).getRVariable(),
					request, rObjs, bpit.getCallFrame());

            if (rObjs.getEngine().isReliabilityEnabled()
                    && rObjs.getBPELProcessManager().getOperationPattern(receive).equals(Engine.IN_ONLY)
                    && rObjs.getBPELProcessManager().isPersistenceEnabled() && request.getTransaction() != null) {

                consumeMesgReliably(receive, frame, rObjs, request);

                /*
                 * If the call is successful we return false. Note that the CallFrame at this point
                 * are not stored in any Map or List. When the afterCompletion method of the
                 * Synchronization object is called, the Callframe will be put in the ReadyToRun
                 * queue. In case of failure, we reset the Receive and return false, so that no
                 * further interpretation takes place.
                 */
                return false;
            } else {
                if (!consumeMesg(onEvent, frame, rObjs, request)) {
                    return false;
                }
            }
            return executeChildAct(frame, bpit, rObjs);
		}    	
		return false;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    @Override
    public boolean doActionOnRecovery(RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        
    	frame.convert();
    	
		if (frame.getPC() != null) {
			LOGGER.info("Recovery started from activity : " + frame.getPC().getName());
		}
		
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
            frame.onActivityTerminate(this);
            getRuntimeEventHandlers().taskCompletes(this, rObjs);
            return true;
		} else {
	        return executeChildAct(frame, bpit, rObjs);
		}
    }

    private boolean executeChildAct(ICallFrame frame, BusinessProcessInstanceThread bpit, 
            RequiredObjects rObjs) throws Exception {
        boolean childCompleted = executeChildActivities(frame, bpit, rObjs,
                (ScopeUnitImpl) mContext);
        if (childCompleted) {
            frame.onActivityComplete(this);
            getRuntimeEventHandlers().taskCompletes(this, rObjs);
        }
        return childCompleted;
    }
    
    /**
     * 
     * @param onEvent
     * @param frame
     * @param rObjs
     * @param request
     * @throws Exception
     */
    private final boolean consumeMesg(RStartElement onEvent, ICallFrame frame, RequiredObjects rObjs,
            MessageContainer request) throws Exception {
        
        BPELProcessManager procMgr = rObjs.getBPELProcessManager();
        // Add the runtime variable to the associated scope.
        // DEVNOTE: Right now we handle only the variable attribute of the OnEvent only if it exists
        // Support for 'fromParts' as an alternate to 'variable' will be added later on.
    	if (onEvent.getRVariable() != null) {
    		RuntimeVariableImpl runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(onEvent.getRVariable()); 
            WSMessage message = (WSMessage) request.getContent();
            runtimeVariable.setWSMessage(message);
            
            Utility.setReferencesForExternalMessage(message, onEvent.getRVariable());
            mContext.setRuntimeVariable(runtimeVariable);
            StateContext stateCtx = mContext.getStateContext();
            mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                    runtimeVariable, getBranchId());
    	}
    	String operPattern = procMgr.getOperationPattern(onEvent);
        
        // TODO associate the business process with the rest of the correlations
        // to be initialised on the consumption of this message, by this receive.
        Object crmpInvId = request.getCRMPInvokeId();
        if (crmpInvId != null) {
            /*
             * create the crmpUpdateListValue and add it to the CRMPUpdateList on the ProcessInstance.
             * the corresponding reply activity will look up the value in this list to determine 
             * to update the entry with the replyActivityId and the responseObject.
             * the update call is synchronized in the IMAScopeBridge class..
             */
            String partnerLink = 
                ((REventHandlersOnEventImpl) getStaticModelActivity()).getRPartner().getName();
            String operation = 
                ((REventHandlersOnEventImpl) getStaticModelActivity()).getOperation();
            String crmpUpdatListValue = frame.getProcessInstance().getId() + partnerLink + operation;
            Context inst = (BPELProcessInstanceImpl) frame.getProcessInstance();
            inst.addToCRMPUpdateList(crmpUpdatListValue);  
            rObjs.setValue(RequiredObjects.CRMP_INVOKE_ID, crmpInvId);
        } 
        
        try {
        	// persist the receive, and then calculate the correlation values.
        	frame.getProcessInstance().getPersistenctMgr().updateState(this, rObjs, 
        			TransactionInfo.getLocalTxInfo(), mContext.getStateContext().getState());
        	// process correlations
        	if (onEvent.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {

        		BPELProcessInstance bpInstance = frame.getProcessInstance();
        		CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
        		CorrelationDefnWrapper corrDefnWrap = onEvent.getCorrelationDefnWrapper();
        		WSMessage wsMesg = (WSMessage) request.getContent();
        		// calculate the correlation values
        		CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMesg, 
        				corrDefnWrap, bpInstance); 
        		// associate the values with the instance. this method will also check
        		// message integrety and add the values to the state object.
        		corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, bpInstance);        
        	}  
        } catch (Exception e) {
        	// CHANGE: 10/18/07 
        	// In this case, we will not persist the fault immediately, the virtual Throw
        	// activity is created and its doAction() is called, which will mark the scope
        	// as faulted. In case of a crash in between these states, the recovery logic 
        	// will have to replay the act of calculating the correlation for this completed
        	// persisted receive.

        	// send a status message for receives if it is an IN_ONLY operation pattern..
        	if (operPattern.equals(Engine.IN_ONLY)) {
        		rObjs.getBPELProcessManager().sendInOnlyRequestDoneStatus(request.getId());
        	} else {
        		mContext.addRequest(onEvent, request);
        	}

        	throw e;
        }

        // send a status message for receives if it is an IN_ONLY operation pattern..
        if (operPattern.equals(Engine.IN_ONLY)) {
        	rObjs.getBPELProcessManager().sendInOnlyRequestDoneStatus(request.getId());
        } else {
            mContext.addRequest(onEvent, request);
        }
        
        return true;
    }

    /**
     * 
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
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doPassControlToParent(ICallFrame, BusinessProcessInstanceThread, RequiredObjects)
     */
    protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
    		BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

    	//Set the program counter, so that when BPELInterpreter checks the current 
    	//program counter when the doAction completes.
    	frame.setProgramCounter(this);
      	getRuntimeEventHandlers().taskCompletes(this, rObjs);
    	frame.onActivityComplete(this);
    	frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
    	
    	return true;
    }
	
    private void postVarEvent(RVariable variable, MessageContainer container, RequiredObjects objs, ICallFrame callFrame) {
    	if (callFrame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
    	//if (objs.getEngine().isMonitorEnabled() && objs.getEngine().isVariableMonitorEnabled()) {
			Variable var = new VariableImpl(variable, container.getContent(), mContext);
			Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
			variableMap.put(VariableEvent.VariableType.INPUT,
					new ArrayList<Variable>());
			variableMap.get(VariableEvent.VariableType.INPUT).add(var);
			callFrame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, null, null);
		}
	}	
    

    /*
     * 
     */
    private final void consumeMesgReliably(RStartElement receive,
            ICallFrame frame,
            RequiredObjects rObjs,
            MessageContainer request) throws Exception {

        txSynchronizer = new TxSynchronizer();
        txSynchronizer.frame = frame;
        txSynchronizer.rObjs = rObjs;
        try {

            MutableState state = mContext.getStateContext().getState();

            txSynchronizer.runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(receive.getRVariable());
            txSynchronizer.runtimeVariable.setWSMessage((WSMessage) request.getContent());
            // Update the variable
            state.updateVariable(getBranchId(), txSynchronizer.runtimeVariable);

            TransactionInfo txInfo = new TransactionInfo(StateManager.TransactionType.XAParticipate,
                    request.getTransaction(), txSynchronizer, false);

            // persist the receive activity (PC) as part of the TX and then calculate the
            // correlations.
            frame.getProcessInstance().getPersistenctMgr().updateState(this, rObjs, txInfo, 
            		mContext.getStateContext().getState());

            /*
             * need to set the activity to ExecutionState.WaitingForTxComplete.
             * TxSynchronizer.afterCompletion() would enqueue the instance and would re-execute the
             * receive(doAction()), where the logic to handle the condition in this case
             * Status.STATUS_COMMITTED is executed.
             */
            this.currentOnEventState = OnEventState.WaitingForTxComplete;

            // success, send the DONE status for the InOnly ME
            rObjs.getBPELProcessManager().sendInOnlyRequestDoneStatus(request.getId());

        } catch (Exception excp) {
            LOGGER.log(
                    Level.WARNING,
                    I18n.loc("BPCOR-6050: Exception occured while processing a reliable message exchange."),
                    excp);

            /*
             * need to set the activity to ExecutionState.WaitingForTxComplete.
             * TxSynchronizer.afterCompletion() would enqueue the instance and would re-execute the
             * Receive(doAction()), where the logic to handle the condition of failure is executed.
             */
            this.currentOnEventState = OnEventState.WaitingForTxComplete;

            // failure, send the ERROR status for the InOnly ME
            rObjs.getBPELProcessManager().sendRequestError(request.getId(), excp);
        }
    }

    private void cancelTransaction(ICallFrame frame, RequiredObjects rObjs) {

        // For OnEvents we don't need to do anything in cancelTransaction. Because if we are 
        // here, it already means that before we attempted to consume the message we 
        // put another onEvent as waiting for the next message. So letting this onEvent die 
        // seems to be a reasonable option.
        getRuntimeEventHandlers().taskCompletes(this, rObjs); 
    }
    
    private class TxSynchronizer implements Synchronization {
        private ICallFrame frame;

        private RequiredObjects rObjs;
        private RuntimeVariableImpl runtimeVariable;
        //private CorrelationDefnValues corrDefnVals;

        private int txStatus;

        /**
         * @see javax.transaction.Synchronization#afterCompletion(int)
         */
        public void afterCompletion(int status) {
            txStatus = status;
            BPELProcessManager processManager = rObjs.getBPELProcessManager();
            processManager.addToReadyToRunQueue(frame);
        }

        /**
         * @see javax.transaction.Synchronization#beforeCompletion()
         */
        public void beforeCompletion() {
            // Does nothing
        }
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#getEnclosingActivityUnit()
     */
    public ActivityUnit getEnclosingActivityUnit() {
        return mEnclosingActivity;
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnEventUnit#setChildScope(com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ScopeUnitImpl)
	 */
	public void setChildScope(ScopeUnitImpl childScope) {
		mContext = childScope;
	}

	
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct#getUID()
	 */
	public String getUID() {
		return mUID;
	}
	
}
