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
 * @(#)ReplyUnitImpl.java 
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

import javax.xml.namespace.QName;

import com.sun.bpel.model.Reply;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.RReplyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CRMPLookup;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.SystemException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Reply activity unit implementation
 *
 * @author Sun Microsystems
 */
public class ReplyUnitImpl extends ActivityUnitImpl {
	
	private enum ReplyState {
		initial,
		waitingForStatus,
		waitingForTransactionCompletion
	}

	/**
	 * toggling of this field is required to support while loop. in a while loop, an activity will
	 * be called many times, and as many times, execution enters and exits the particular
	 * activity.
	 */
	private ReplyState mState;
	
	/*
	 * True if the request for which the reply is being done is associated with the transaction to be propagated.
	 */
	private boolean mRequestAssociatesWithTx = false;
	private boolean mUpdateCRMP = false;
	private SystemException mErrorStatusException = null;

	/**
	 * Creates a new ReplyUnitImpl object.
	 *
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public ReplyUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
		mState = ReplyState.initial;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
	 *      BusinessProcessInstanceThread, RequiredObjects)
	 */
	public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit,RequiredObjects rObjs) 
	throws Exception {

		if (mState.equals(ReplyState.waitingForTransactionCompletion)) {
			if (mErrorStatusException != null) {
				throw mErrorStatusException;
			}
			completeReply(frame, bpit, rObjs);
			return doPassControlToParent(frame, bpit, rObjs);	
		} else if (mState.equals(ReplyState.waitingForStatus)) {

			if(!this.equals(frame.getProgramCounter())) {
				throw new RuntimeException(Utility.createIllegalPCChangeMessage(this, mAct, frame));
			}

			if(mContext.getFaultHandlingContext().isBeingTerminated()) {
				frame.onActivityTerminate(this);
				return doPassControlToParent(frame, bpit, rObjs);
			}
			
			mState = ReplyState.initial;

			InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(frame.getProcess(), 
					Event.DONE, bpit.getMessageExchangeKey());
			MessageContainer obj = rObjs.getBPELProcessManager().receiveDone(event);
			boolean retVal = processStatus(frame, obj);
			if (retVal) {
				completeReply(frame, bpit, rObjs);
				return doPassControlToParent(frame, bpit, rObjs);
			} else {
				return retVal; // retVal will always be false here.
			}
		} else {
			frame.setProgramCounter(this);
			frame.onLineChange(this);
			BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
			frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	
			return sendReply(frame, bpit, rObjs);
		}
	}

	/**
	 * The design aspects for CRMP based recovery are as follows.
	 * when engine comes to executing reply:
	 * 1) if we can find a live messageExchange waiting for response
	 *    a) we send the response
	 *    b) wait for done
	 *    c) then persist the reply activity as done
	 * 2) if there is no active message Exchange waiting for response (happens in the case of recovery 
	 * 	  or similar situations like failover, clustering)
	 * 	  a) we persist the response in CRMP table
	 * 	  b) we persist the reply activity as done
	 * 	  c) we don't wait for done and continue with the BPEL execution      * @param frame
	 * @param bpit
	 * @param rObjs
	 * @return
	 * @throws Exception
	 */
	private boolean sendReply(
			ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
	) throws Exception {

		RReplyImpl reply = (RReplyImpl) mAct;

		BPELProcessManager procMgr = rObjs.getBPELProcessManager();
		RVariable variable = reply.getRVariable();
		RuntimeVariable runtimeVar = mContext.getRuntimeVariable(variable);
		if ((runtimeVar == null) && (variable.getWSDLMessageType().getParts().size() == 0)) {
			runtimeVar = mContext.createRuntimeVariable(variable); 
			Utility.initializeVariableValue(runtimeVar);
			mContext.setRuntimeVariable(runtimeVar);
            StateContext stateCtx = mContext.getStateContext();
            mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                    runtimeVar, getBranchId());
		} else {        
			Utility.verifyValue(variable.getName(), runtimeVar, true);
		}
		WSMessage output = runtimeVar.getWSMessage();
		if (output == null) {
			throw Utility.uninitializedVariableError(runtimeVar.getVariableDef().getName());
		}
		postVarEvent(reply.getRVariable(), output, rObjs, frame);
		// verify and associate correlations.
		CorrelationManager corrMgr = procMgr.getCorrMgr();
		BPELProcessInstance bpInstance = frame.getProcessInstance();
		CorrelationDefnWrapper corrDefnWrap = reply.getCorrelationDefnWrapper();
		// do correlation value assertion on the output message
		corrMgr.doCorrMessageAssertion(corrDefnWrap, output, bpInstance);
		// calculate the correlation values and set on the instance
		CorrelationDefnValues corrDefnVals = corrMgr.calculateCorrValues(output, 
				corrDefnWrap, bpInstance);
		// this method would check for integrity of the correlation values and also
		// set the values on the state object, refer to the method java docs for info
		corrMgr.checkAndAssociateCorrValuesWithInstance(corrDefnVals, bpInstance);
		Utility.setReferencesForExternalMessage(output, variable);

		MessageContainer request = null;
		Object crmpMonitor = procMgr.getCRMPMonitor();
		synchronized(crmpMonitor) {
			String key = frame.getProcessInstance().getId() + reply.getRPartner().getName() + reply.getOperation();
			request = mContext.removeRequest(reply);
			if (request == null) {
				request = frame.getProcessInstance().getBPELProcessManager().removeCRMPReqForRecoveringInsts(key);
			}

			if (request == null) {
				//since the request is null update crmp table with response, persist reply.
				mUpdateCRMP = true;
				frame.getProcessInstance().getPersistenctMgr().updateState(this, 
						rObjs, mUpdateCRMP, (BPELProcessInstanceImpl) frame.getProcessInstance(), 
                        mContext.getStateContext().getState(), frame.getBranchInvokeCounter());
				CRMPLookup lookup = new CRMPLookup(frame, runtimeVar);
				rObjs.getBPELProcessManager().putReplyInCRMPReplyMap(key, lookup);
				mUpdateCRMP = false;
			} 
		}        

		if (request != null) {
			// prepare for calling sendReply
			String meId = request.getId();
			ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
					frame.getProcess(), Event.DONE, meId);
			MessageContainer con = MessageContainerFactory.createMessage(meId, output, request);
			QName faultName = reply.getFaultName();
			
			if (procMgr.isBPAtomic() && (!frame.isEventHandlerCF())) {
				TxPropagationObject txPropObj = frame.getProcessInstance().getTxPropagationObject();
				if ((!txPropObj.getAtomicTransactionType().equals(TxPropagationObject.TransactionType.started)) && 
						txPropObj.getParentContainer().equals(request)) {
					mRequestAssociatesWithTx = true;
					if (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.active)) {
						synchronized (txPropObj) {
							txPropObj.setReplyFrame(frame);
							txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.waitingOnReply);
						}
					}
				}
			}
			// sends a reply
			procMgr.sendReply(con, faultName);

			// find if status available, otherwise add the frame to the pending
			// queue to wait for the done message.
			MessageContainer content = null;
			boolean isTerminated = false;
			
			Object lock = procMgr.getProcessLevelLock();
			
			synchronized (lock) {
				Map eventDoneMap = procMgr.getEventDoneMap();
				content = (MessageContainer) eventDoneMap.remove(event);
				if (content == null) {
					if(mContext.getFaultHandlingContext().isBeingTerminated()) {
						isTerminated = true;
						//Don't add the callframe to the pending queue
					} else {
						procMgr.addToPendingQueueWaitingForDone(event, frame);
						mState = ReplyState.waitingForStatus;
						return false;
					}
				}
			}

			if(isTerminated) {
				frame.onActivityTerminate(this);
				return true;
			}

			boolean retVal = processStatus(frame, content);
			if (retVal) {
				completeReply(frame, bpit, rObjs);
				BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
			}
			return retVal;
		}

		//The request object is null, the CRMP table was persisted with Response obj
		//the reply was persisted as complete and hence continue to process after reply.
		BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
		frame.onActivityComplete(this);
		frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	

		return true;
	}

	private void completeReply(ICallFrame frame,
			BusinessProcessInstanceThread bpit,
			RequiredObjects rObjs) throws Exception {
		frame.getProcessInstance().getPersistenctMgr().updateState(this, 
				rObjs, mUpdateCRMP, (BPELProcessInstanceImpl) frame.getProcessInstance(), 
                mContext.getStateContext().getState(), frame.getBranchInvokeCounter());
		frame.onActivityComplete(this);
		frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
	}

	private boolean processStatus(ICallFrame frame, MessageContainer obj) {
		BPELProcessManager procMgr = frame.getProcessInstance().getBPELProcessManager();
		if (obj.isStatusError()) {
			// Do not throw the exception right away. If the BP is atomic and this request is associated with 
			// the transaction then this will be deferred until the transaction completes if not already completed.
			
			String message = Utility.appendLineNumberAndActName(I18n.loc("BPCOR-6132: An Error status was received instead of Done (partnerLink={0}, portType={1}, operation={2})", 
					((Reply)mAct).getPartnerLink(), ((Reply)mAct).getPortType(), ((Reply)mAct).getOperation()), this);
			mErrorStatusException = new SystemException(message, (Exception)obj.getContent());
		}

		if (procMgr.isBPAtomic() &&	(!frame.isEventHandlerCF())) {
			TxPropagationObject txPropObj = frame.getProcessInstance().getTxPropagationObject();
			if (!txPropObj.getAtomicTransactionType().equals(TxPropagationObject.TransactionType.started)) {
				if (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.uninitialized)) {
					txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.replyComplete);
				} 
				if (mRequestAssociatesWithTx) {
					synchronized (txPropObj) {
						if (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.waitingOnReply)) {
							// The transaction has not yet been committed. So set the waiting for TxCompletion flag
							// to true and return false;
							mState = ReplyState.waitingForTransactionCompletion;
							txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.replyComplete);
							return false;
						} else if (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.unavailable)) {
							txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.replyComplete);
						}
					}
				}
			}
		}
		
		if (mErrorStatusException != null) {
			throw mErrorStatusException;
		}
		return true;
	}

	private void postVarEvent(RVariable variable, Object content, RequiredObjects objs, ICallFrame frame) {
		if (frame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
			Variable var = new VariableImpl(variable, content, mContext);
			Map<VariableEvent.VariableType, List<Variable>> variableMap = 
                new HashMap<VariableType, List<Variable>>();
			List<Variable> vars = new ArrayList<Variable>();
			vars.add(var);
			variableMap.put(VariableEvent.VariableType.OUTPUT, vars);
			frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, null, null);
		}
	}    
}
