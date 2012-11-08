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
 * @(#)ReceiveUnitImpl.java 
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

import com.sun.bpel.model.Receive;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RReceive;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
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
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Receive activity unit implementation
 * 
 * @author Sun Microsystems
 */
public class ReceiveUnitImpl extends ActivityUnitImpl {

	private static final Logger LOGGER = Logger.getLogger(ReceiveUnitImpl.class.getName());

	private TxSynchronizer txSynchronizer;

	private enum ReceiveState {Initial, WaitingForRequest, WaitingForTxComplete};
	private ReceiveState currentReceiveState = ReceiveState.Initial;
	
	/* Indicates if this receive activity unit instantiated the bpel process instance */
	private boolean mCreatesInstance;

	/**
	 * DOCUMENT ME!
	 */
	InComingEventKeyImpl mWaitingForEvent = null;

	/**
	 * Creates a new ReceiveUnitImpl object.
	 * 
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public ReceiveUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
		Receive recAct = (Receive) act;
		mCreatesInstance = (Receive.YES.equals(recAct.getCreateInstance())) ? true : false;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
	 *      BusinessProcessInstanceThread, RequiredObjects)
	 */
	public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
			RequiredObjects rObjs) throws Exception {

		RStartElement receive = (RStartElement) mAct;
		// TODO the following needs to be modelled right.
		BPELProcessManager procMgr = rObjs.getBPELProcessManager();
		BPELProcessInstance procInstance = frame.getProcessInstance(); 

		if (currentReceiveState == ReceiveState.WaitingForRequest) {

			if (!this.equals(frame.getProgramCounter())) {
				throw new RuntimeException(Utility.createIllegalPCChangeMessage(this, mAct, frame));
			}

			if(mContext.getFaultHandlingContext().isBeingTerminated()) {
				frame.onActivityTerminate(this);
				return doPassControlToParent(frame, bpit, rObjs);
			}

			//TODO: Do we need this here? It was already called once when the control
			//came in the first time into receive.
            // -- ksorokin: no, we don't, as opposed to onEvent handler, of 
            //              which an instance of is always running
			//frame.onLineChange(this);
			//BPELTraceManager.getInstance().doTraceOnStart(mAct.getTrace(), mContext);

			//TODO: This time we assume that the message will be found. Is that a valid assumption?
			Object[] result = procMgr.receiveRequestOrPutInPendingQueue(mWaitingForEvent, frame);
			MessageContainer request = (MessageContainer) result[0];
			if(request != null)
				mContext.addRequest(receive, request);
			//Post event
			postVarEvent (receive.getRVariable(), request, rObjs, bpit.getCallFrame());

			if (rObjs.getEngine().isReliabilityEnabled()
					&& procMgr.getOperationPattern(receive).equals(Engine.IN_ONLY)
					&& procMgr.isPersistenceEnabled() && request.getTransaction() != null) {

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
				if (!consumeMesg(receive, frame, rObjs, request)) {
					return false;
				}
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this); 
				return doPassControlToParent(frame, bpit, rObjs);
			}
		} else if (this.currentReceiveState == ReceiveState.WaitingForTxComplete) {

			procInstance.getPersistenctMgr().finishPersistence(mContext.getStateContext().getState());
			procInstance.getClusterMgr().decrementActiveMessagingActivitiesCount(frame);

			if (txSynchronizer.txStatus == Status.STATUS_COMMITTED) {
				/*
				 * Check if the tx was successful, if yes, commit the changes to the memory e.g.,
				 * Context, etc. and let the next activity execute
				 */

				// We don't need to update the state here
				mContext.setRuntimeVariable(txSynchronizer.runtimeVariable);

                // calculate the correlations, verify it and set it on the instance and the state obj.
				// the next persistence point will persist it. Any CorrelationViolation exception will 
				// be handled by the BPELInterpreter.execute() and thrown as a VirtualThrow unit.
				// if it had crashed before this point, recovery will recover this StartActivityUnit and
				// reclaculate and check the integrety of the calculations
				if (receive.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {
					
					CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
					WSMessage wsMsg = txSynchronizer.runtimeVariable.getWSMessage();
					// calculate the correlation values
					CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMsg, 
							receive.getCorrelationDefnWrapper(), frame.getProcessInstance()); 
					//set the correlation values on the instance object. 
					//this method would check for integrity of the correlation values and also
					// set the values on the state object, refer to the method java docs for info
					corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, frame.getProcessInstance());
				}
				// If receive happens, it is always true that the first start activity completes
				// TODO: This will be called even when the Receive is not the first activity, we
				// probably should check if the Receive was indeed the first activity was was
				// executed
				frame.getProcessInstance().startActivityCompletes();
				BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
				frame.onActivityComplete(this);
     			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
	
     			return doPassControlToParent(frame, bpit, rObjs);
				
			} else {
				// For any other status other than STATUS_COMMITTED, we should consider
				// that the Tx has failed
				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6049: The XA Transaction failed. " + 
						"The Transaction Manager sent a status {0}", txSynchronizer.txStatus));
				cancelTransaction(frame, rObjs);
				return false;
			}
		}

		frame.setProgramCounter(this);
				//Modified by Logicoy - To fix Receive Activity not in monitorbpelactivity table if a bpel has more than one receive -- Fix start
                //ADDON LMA the two following lines are added up here instead of later to correct the issue with ReceiveActivity event not send
                BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
		frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);
		//Modified by Logicoy - To fix Receive Activity not in monitorbpelactivity table if a bpel has more than one receive -- Fix end
		InComingEventKeyImpl event = Utility.createSAInComingEventKeyImpl(receive,
				frame, rObjs.getBPELProcessManager());

		currentReceiveState = ReceiveState.WaitingForRequest;
		mWaitingForEvent = event;

		Object[] result = procMgr.receiveRequestOrPutInPendingQueue(event, frame);
		MessageContainer request = (MessageContainer) result[0];
		if (request == null) {
			if ((Boolean)result[1]) {
				frame.onActivityTerminate(this);
				return true;
			} else {
				//TODO: Does this need to be in the getRequestOrPutInPendingQueue() method?
				procInstance.getClusterMgr().passivateInstance(this, frame, event);
				return false;
			}
		}
			mContext.addRequest(receive, request);

		frame.onLineChange(this);
		//Modified by Logicoy - To fix Receive Activity not in monitorbpelactivity table if a bpel has more than one receive -- Fix start
                //ADDON LMA comment the two following lines
		//BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
		//frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);
		//Modified by Logicoy - To fix Receive Activity not in monitorbpelactivity table if a bpel has more than one receive -- Fix end
		currentReceiveState = ReceiveState.Initial;
		mWaitingForEvent = null;
		
		if (mCreatesInstance) {
			procInstance.setParentMExArtifacts(request);
		}

		boolean incrementDone = procInstance.getClusterMgr().incrementActiveMessagingActivitiesCount(
				frame, event, request);

		if (!incrementDone) {
			// this indicates some other branch of the flow
			// marked the instance as passivated and the request was put back on 
			// correlated waiting event map. Return false as cannot execute further. 
			return false;
		}
		
		//Post event
		postVarEvent (receive.getRVariable(), request, rObjs, bpit.getCallFrame());        

		if (rObjs.getEngine().isReliabilityEnabled()
				&& procMgr.getOperationPattern(receive).equals(Engine.IN_ONLY)
				&& rObjs.getBPELProcessManager().isPersistenceEnabled() && request.getTransaction() != null) {

			consumeMesgReliably(receive, frame, rObjs, request);

			/*
			 * If the call is successful we return false. Note that the CallFrame at this point are
			 * not stored in any Map or List. When the afterCompletion method of the Synchronization
			 * object is called, the Callframe will be put in the ReadyToRun queue. In case of
			 * failure, we reset the Receive and return false, so that no further interpretation
			 * takes place.
			 */
			return false;
		} else {
			if (!consumeMesg(receive, frame, rObjs, request)) {
				return false;
			}
			BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
			frame.onActivityComplete(this);
			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
			procInstance.getClusterMgr().decrementActiveMessagingActivitiesCount(frame);
			return true;
		}
	}

	private void postVarEvent(RVariable variable, MessageContainer container, RequiredObjects objs, ICallFrame frame) {
		if (frame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
			Variable var = new VariableImpl(variable, container.getContent(), mContext);
			Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
			List<Variable> vars = new ArrayList<Variable>();
			vars.add(var);
			variableMap.put(VariableEvent.VariableType.INPUT, vars);
			frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, 
					container.getCRMPInvokeId(), null);
		}
	}    
	
	private boolean isInOnlyDoneStatus(BPELProcessManager procMgr, ICallFrame frame, RStartElement receive) {
		// send request done status for an IN_ONLY request if BP atomic is not atomic or transaction status is not active or unavailable.
		String operPattern = procMgr.getOperationPattern(receive);
		TxPropagationObject txPropObj = frame.getProcessInstance().getTxPropagationObject();
		if (!operPattern.equals(Engine.IN_ONLY))  
			return false;
		if ((procMgr.isBPAtomic()) && 
				((txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.active)) || 
						((txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.unavailable)))))
			return false;
		return true;
	}
	

	private final boolean consumeMesg(RStartElement receive, ICallFrame frame, RequiredObjects rObjs, 
			MessageContainer request) 
	throws Exception {

		BPELProcessManager procMgr = rObjs.getBPELProcessManager();

        RuntimeVariableImpl runtimeVariable = (RuntimeVariableImpl) mContext.getRuntimeVariable(receive.getRVariable()); 
        if (runtimeVariable == null) {
            runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(receive.getRVariable());
        }
        WSMessage message = (WSMessage) request.getContent();
        runtimeVariable.setWSMessage(message);
        Utility.setReferencesForExternalMessage(message, receive.getRVariable());
		mContext.setRuntimeVariable(runtimeVariable);
        StateContext stateCtx = mContext.getStateContext();
        mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                runtimeVariable, getBranchId());
		String operPattern = procMgr.getOperationPattern(receive);
		TxPropagationObject txPropObj = frame.getProcessInstance().getTxPropagationObject();
		
		// TODO associate the business process with the rest of the correlations
		// to be initialized on the consumption of this message, by this receive.
		Object crmpInvId = request.getCRMPInvokeId();
		if (crmpInvId != null) {

			/*
			 * create the crmpUpdateListValue and add it to the CRMPUpdateList on the
			 * ProcessInstance. the corresponding reply activity will look up the value in this list
			 * to determine to update the entry with the replyActivityId and the responseObject. the
			 * update call is synchronized in the IMAScopeBridge class..
			 */
			String partnerLink = ((RReceive) getStaticModelActivity()).getRPartner().getName();
			String operation = ((Receive) getStaticModelActivity()).getOperation();
			String crmpUpdatListValue = frame.getProcessInstance().getId() + partnerLink
			+ operation;
			Context inst = (BPELProcessInstanceImpl) frame.getProcessInstance();
			inst.addToCRMPUpdateList(crmpUpdatListValue);
			rObjs.setValue(RequiredObjects.CRMP_INVOKE_ID, crmpInvId);
		}

		try {
			// persist the receive, and then calculate the correlation values.
			frame.getProcessInstance().getPersistenctMgr().updateState(this, rObjs, 
					TransactionInfo.getLocalTxInfo(), mContext.getStateContext().getState(),
					frame.getBranchInvokeCounter());

			// process correlations
			if (receive.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {

				CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
				// calculate the correlation values
				CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues((WSMessage) request.getContent(), 
						receive.getCorrelationDefnWrapper(), frame.getProcessInstance()); 
				//set the correlation values on the instance object. 
				//this method would check for integrity of the correlation values and also
				// set the values on the state object, refer to the method java docs for info
				corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, frame.getProcessInstance());
			}
		} catch (Exception e) {
			// CHANGE: 10/18/07 
			// In this case, we will not persist the fault immediately, the virtual Throw
			// activity is created and its doAction() is called, which will mark the scope
			// as faulted. In case of a crash in between these states, the recovery logic 
			// will have to replay the act of calculating the correlation for this completed
			// persisted receive.

			// send a status message for receives if it is an IN_ONLY operation pattern..
			// send request done status for an IN_ONLY request if BP atomic is not atomic or transaction status is not active or unavailable.
			if(isInOnlyDoneStatus(procMgr, frame, receive)){
				mContext.removeRequest(receive, request);
				procMgr.sendInOnlyRequestDoneStatus(request.getId());
			} 

			throw e;
		}

		// send request done status for an IN_ONLY request if BP atomic is not atomic or transaction status is not active or unavailable.
		if(isInOnlyDoneStatus(procMgr, frame, receive)){
			mContext.removeRequest(receive, request);
			procMgr.sendInOnlyRequestDoneStatus(request.getId());
		}

		// If receive happens, it is always true that the first start activity completes
		frame.getProcessInstance().startActivityCompletes();

		return true;
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

            txSynchronizer.runtimeVariable = (RuntimeVariableImpl) mContext.getRuntimeVariable(receive.getRVariable());
            if (txSynchronizer.runtimeVariable == null) {
                txSynchronizer.runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(receive.getRVariable());
            }
            WSMessage message = (WSMessage) request.getContent();
        	txSynchronizer.runtimeVariable.setWSMessage(message);
            Utility.setReferencesForExternalMessage(message, receive.getRVariable());
            
			// Update the variable
            StateContext stateCtx = mContext.getStateContext();
            mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                    txSynchronizer.runtimeVariable, getBranchId());

			TransactionInfo txInfo = new TransactionInfo(StateManager.TransactionType.XAParticipate,
					request.getTransaction(), txSynchronizer, false);

			// persist the receive activity (PC) as part of the TX
			frame.getProcessInstance().getPersistenctMgr().updateState(this, rObjs, txInfo, 
					mContext.getStateContext().getState(), frame.getBranchInvokeCounter());

			/*
			 * need to set the activity to ExecutionState.WaitingForTxComplete.
			 * TxSynchronizer.afterCompletion() would enqueue the instance and would re-execute the
			 * receive(doAction()), where the logic to handle the condition in this case
			 * Status.STATUS_COMMITTED is executed.
			 */
			this.currentReceiveState = ReceiveState.WaitingForTxComplete;

			// success, send the DONE status for the InOnly ME
			mContext.removeRequest(receive, request);
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
			this.currentReceiveState = ReceiveState.WaitingForTxComplete;

			// failure, send the ERROR status for the InOnly ME
			if(receive!= null && request != null)
				mContext.removeRequest(receive, request);
			rObjs.getBPELProcessManager().sendRequestError(request.getId(), excp);
		}
	}

	private void cancelTransaction(ICallFrame frame, RequiredObjects rObjs) {

		// TODO: If the Receive with createInstance=yes was in a flow with other
		// start activities then this cleanup is not sufficient. We need to address this
		// along with other problems related to handling correlations

		if (((RStartElement) mAct).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY) {
			/*
			 * If the Receive has createInstance=yes, we need to remove the process instance from
			 * the memory. Note that no correlations have been associated yet.
			 */
			rObjs.getBPELProcessManager().instanceComplete(frame.getProcessInstance());
		} else {
			// The Receive did not create a new instance, so we need to restore
			// the memory

			// Clear the Tx Synchronization Object
			txSynchronizer = null;

			// Clear the flags and the state of the Receive
			this.currentReceiveState = ReceiveState.Initial;
			this.mWaitingForEvent = null;
			this.mAct = null;

			// Put in ReadyToRun Queue - this will have an effect of restarting
			// the Receive activity
			BPELProcessManager processManager = rObjs.getBPELProcessManager();
			processManager.addToReadyToRunQueue(frame);
		}
	}
	
	public boolean doActionOnRecovery(
			RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
	) throws Exception {
		
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
		
		return doPassControlToParent(frame, bpit, rObjs); 
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
}
