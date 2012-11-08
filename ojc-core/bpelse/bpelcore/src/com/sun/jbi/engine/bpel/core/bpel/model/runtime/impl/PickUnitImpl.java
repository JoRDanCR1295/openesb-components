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
 * @(#)PickUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.Status;
import javax.transaction.Synchronization;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.ROnAlarm;
import com.sun.bpel.model.meta.ROnMessage;
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
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager.MessageContainerForPick;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Pick activity unit implementation
 *
 * @author Sun Microsystems
 */
public class PickUnitImpl extends StructuredActivityUnitImpl {
	
	public static final String YES = "yes";
    private static final Logger LOGGER = Logger.getLogger(PickUnitImpl.class.getName()); 
    private TxSynchronizer txSynchronizer;

    protected enum PickState {Initial, WaitingForRequest, RequestReceived, 
    	WaitingForTxComplete};
    private PickState currentPickState = PickState.Initial;
    private boolean consumedMsgWithoutWaiting = true;
    
    //private static final String ON_MESG_PROP_NAME = "OnMessage Id"; //$NON-NLS-1$
    private long mWaitTime = Long.MIN_VALUE;
    private ROnAlarm mOnAlarm = null;
    private Object mLockObj = new Object();
    private boolean mAcquired = false;
        
    /** flag used in recovery to determine if the onMessage was persisted before crash  */
    private boolean mIsOnMesg = false;
    /** during recovery when one of the paths is identified, this variable keeps track of
     * the path to take (onMessage or onAlarm path) */
    private RActivityHolder mChildActHolder = null;
    
    /** child activity */
    protected Activity mChildAct = null;
    
    private RActivityHolder selectedChildAct;
    
    private InComingEventKeyImpl[] mOnMesgEvents = null;
    
    /* Indicates if this receive activity unit instantiated the bpel process instance */
	private boolean mCreatesInstance;

    /**
     * constructor
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public PickUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
        Pick pickAct = (Pick) act;
        mCreatesInstance = (YES.equals(pickAct.getCreateInstance())) ? true : false;
    }

    /**
     * Creates a new PickUnitImpl object.
     */
//    public PickUnitImpl() {
//        super();
//    }
    
    /** pickUnit that is constructed during recovery with information available for recovery
     * @param context
     * @param unit pick activity unit 
     * @param pickCompositeActId pick composite activity ID
     * @param timerVal timer value
     */
    public PickUnitImpl(Context context, PickUnitImpl unit, long pickCompositeActId, long timerVal) {
        this(context, unit.getEnclosingActivityUnit(), unit.getStaticModelActivity(), unit.getBranchId());
        mWaitTime = timerVal;
        init(pickCompositeActId);
    }

    private void init(long pickCompositeActId) {
        Pick pick = (Pick) getStaticModelActivity();

        for (int j = 0, onMesgSize = pick.getOnMessageSize(); j < onMesgSize; j++) {
            ROnMessage onMesg = (ROnMessage) pick.getOnMessage(j);

            if (onMesg.getUniqueId() == pickCompositeActId) {
                mChildActHolder = onMesg;
                break;
            }
        }

        mIsOnMesg = (mChildActHolder != null);

        if (!mIsOnMesg) {
            // for OnAlarm
            for (int i = 0, size = pick.getOnAlarmSize(); i < size; i++) {
                ROnAlarm onAlrm = (ROnAlarm) pick.getOnAlarm(i);

                if (onAlrm.getUniqueId() == pickCompositeActId) {
                    mOnAlarm = onAlrm;
                    mChildActHolder = onAlrm;
                    break;
                }
            }
        }

        if (mChildActHolder == null) {
            throw new BPELRuntimeException(
                BPELRuntimeException.INVALID_BPEL,
                I18n.loc("BPCOR-6057: failed to recover the instance.") +
                I18n.loc("BPCOR-6058: Couldn't find a matching OnMessae or OnAlarm for the recovered pick")
            );
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doAction(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        
        if (currentPickState == PickState.WaitingForRequest) {
            
            if(!this.equals(frame.getProgramCounter())) {
                throw new RuntimeException(Utility.createIllegalPCChangeMessage(this, mAct, frame));
            }

            synchronized (mLockObj) {
                if (mAcquired) {
                    return false;
                }
                mAcquired = true;
            }
            
            if(mContext.getFaultHandlingContext().isBeingTerminated()) {
        		frame.onActivityTerminate(this);
                return doPassControlToParent(frame, bpit, rObjs);
        	}
            
            if (bpit.getType() ==  BusinessProcessInstanceThread.TIMEOUT) {
                selectedChildAct = mOnAlarm;
                frame.onLineChange(this, selectedChildAct);
                
                mChildAct = mOnAlarm.getChildActivity();
                return doContinuePick(frame, bpit, rObjs);
            }

            BPELProcessManager procMgr = rObjs.getBPELProcessManager();

            //this time request is bound to be found.
            MessageContainerForPick cont = procMgr.pickRequestWithOutWaiting(mOnMesgEvents, frame);

            if (cont == null) {
                throw new RuntimeException(I18n.loc("BPCOR-6059: request can't be null"));
            }
            
            currentPickState = PickState.RequestReceived;
            MessageContainer request = cont.getMessageContainer();
            ROnMessage se = (ROnMessage) cont.getInComingEventKey().getEventModel().getStartElement();
        	if(request != null)
        		mContext.addRequest(se, request);
        	
            selectedChildAct = se;
            frame.onLineChange(this, selectedChildAct);
            
            mChildAct = ((RActivityHolder) se).getChildActivity();
            
            postVarEvent (se.getRVariable(), request, rObjs, bpit.getCallFrame());                  
            
            if (rObjs.getEngine().isReliabilityEnabled() 
                    && procMgr.getOperationPattern(se).equals(Engine.IN_ONLY) 
                    && rObjs.getBPELProcessManager().isPersistenceEnabled() 
                    && request.getTransaction() != null) {

            	doPickOnMesgReliably(se, cont, frame, rObjs, procMgr);
                //Post event             
                
                /*
                 * If the call is successful we return false. Note that the CallFrame at 
                 * this point are not stored in any Map or List. When the afterCompletion 
                 * method of the Synchronization object is called, the Callframe will be put 
                 * in the ReadyToRun queue. In case of failure, we reset the Pick to the 
                 * Waiting state and return false, so that no further interpretation takes place. 
                 */
                return false;
            } else {
            	if (!doPickOnMesg(se, request, frame, rObjs, procMgr)) {
                	return false;
                }
                //If pick happens, it is always true that the first start activity completes
                frame.getProcessInstance().startActivityCompletes();

                boolean done = doContinuePick(frame, bpit, rObjs);
                if (done) {
//                    frame.onActivityComplete(this);
//          	        postActivityCompleteEvent(rObjs, bpit.getCallFrame().getBPId());    	
                    
                }
                return done;            
            }
        } else if (currentPickState == PickState.WaitingForTxComplete) {

            BPELProcessInstance procInstance = frame.getProcessInstance();
            procInstance.getPersistenctMgr().finishPersistence(mContext.getStateContext().getState());
            procInstance.getClusterMgr().decrementActiveMessagingActivitiesCount(frame);
            
            ROnMessage onMesg = (ROnMessage) txSynchronizer.request.getInComingEventKey()
                                                        .getEventModel().getStartElement();
            
            if(txSynchronizer.txStatus == Status.STATUS_COMMITTED) {
                /*
                 * Check if the tx was successful, if yes, commit the changes to the memory
                 * e.g., Context, etc. and let the next activity execute 
                 */
                
                //We don't need to update the state here
                mContext.setRuntimeVariable(txSynchronizer.runtimeVariable);        

                //update child activity so that execute continues with OnMessage branch corresponding to request
                mChildAct = ((RActivityHolder) onMesg).getChildActivity();

                // calculate the correlations, verify it and set it on the instance and the state obj.
				// the next persistence point will persist it. Any CorrelationViolation exception will 
				// be handled by the BPELInterpreter.execute() and thrown as a VirtualThrow unit.
				// if it had crashed before this point, recovery will recover this StartActivityUnit and
				// reclaculate and check the integrety of the calculations
                if (onMesg.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {
                	
                	CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
                	WSMessage wsMsg = txSynchronizer.runtimeVariable.getWSMessage();
                	// calculate the correlation values
                	CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMsg, 
                			onMesg.getCorrelationDefnWrapper(), frame.getProcessInstance()); 
                	//set the correlation values on the instance object. 
                	//this method would check for integrity of the correlation values and also
                	// set the values on the state object, refer to the method java docs for info
                	corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, frame.getProcessInstance());
                }
                // If pick happens, it is always true that the first start activity completes
                //TODO: This will be called even when the Receive is not the first activity, we
                //probably should check if the Receive was indeed the first activity that was executed
                frame.getProcessInstance().startActivityCompletes();
                frame.onActivityComplete(this);
				return doContinuePick(frame, bpit, rObjs);

            } else {
                //For any other status other than STATUS_COMMITTED, we should consider
                //that the Tx has failed
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6049: The XA Transaction failed. The Transaction " + 
                		"Manager sent a status {0}", txSynchronizer.txStatus));
                cancelTransaction(frame, onMesg, rObjs);
                return false;
            }
        }
        
        frame.setProgramCounter(this);
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	
        
        //calculate wait time/deadline - earliest OnAlarm and update member value
        long deadline = computeWaitTimeAndSetOnAlarm();

        boolean shouldContinueExec = doPick(frame, bpit, rObjs, deadline, false);

        if (shouldContinueExec) {
            if(mContext.getFaultHandlingContext().isBeingTerminated()) {
        		//frame.onActivityTerminate(this); was already done in doPick
            	return true;
        	}
        	
            //If pick happens, it is always true that the first start activity completes
            frame.getProcessInstance().startActivityCompletes();            
            boolean done =  super.doAction(frame, bpit, rObjs);
            if (done) {
                frame.onSubActivityComplete(this, selectedChildAct);
                
            	//TODO: This was already done in the super.doAction().
                frame.onActivityComplete(this);
      	        frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
            }
            return done;              
        } else {
            //Value should be false since we are waiting for a message to come in
            //or an onAlarm to go off
            return false; 
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        return (RActivity) mChildAct;
    }

	private void setOnMesgEvents(Pick pick, ICallFrame frame, RequiredObjects rObjs) {
        int onMsgSize = pick.getOnMessageSize();
        
        InComingEventKeyImpl[] onMesgEvents = new InComingEventKeyImpl[onMsgSize];
        ROnMessage se = null;

        for (int i = 0; i < onMsgSize; i++) {
            se = (ROnMessage) pick.getOnMessage(i);
            onMesgEvents[i] = Utility.createSAInComingEventKeyImpl(se, frame, rObjs.getBPELProcessManager());
        }
        this.mOnMesgEvents = onMesgEvents;
    }

    private long computeWaitTimeAndSetOnAlarm() {
		Pick pick = (Pick) mAct;
		ROnAlarm onAlrm = null;
		long waitTime;
		long deadline = 0;
		int onAlrmSize = pick.getOnAlarmSize();

		for (int i = 0; i < onAlrmSize; i++) {
			onAlrm = (ROnAlarm) pick.getOnAlarm(i);
			// calculate waitTime
			boolean isFor = true;
			OnAlarm alarm = (OnAlarm) onAlrm;
			String expr = alarm.getFor();
			if (expr == null) { // <until>, not <for>
				expr = alarm.getUntil();
				isFor = false;
			}

			Date date = Utility.getDateFromExpr(alarm, mContext, expr, isFor);
			waitTime = date.getTime();
			if ((waitTime < deadline) || (deadline == 0)) {
				mOnAlarm = onAlrm;
				deadline = waitTime;
			}
		}
		return deadline;
	}
    
    /**
     * @param frame
     * @param bpit
     * @param rObjs
     * @param deadline
     * @param inRecovery
     * @return
     * @throws Exception
     */
    protected boolean doPick(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, long deadline, boolean inRecovery) throws Exception {
        
    	Pick pick = (Pick) mAct;
    	
		mWaitTime = deadline;
		// did deadline expire, yes execute OnAlarm
		if ((mOnAlarm != null) && (mWaitTime <= System.currentTimeMillis())) {
            selectedChildAct = mOnAlarm;
            frame.onLineChange(this, selectedChildAct);
            
			mChildAct = mOnAlarm.getChildActivity();
			return true;
		}

        // Initialize OnMessage events
        setOnMesgEvents(pick, frame, rObjs);

        BPELProcessManager procMgr = rObjs.getBPELProcessManager();
        BPELProcessInstance procInstance = frame.getProcessInstance();
        
        // check for request, if not available queue events
        Object[] result = procMgr.pickRequestOrPutInPendingQueue(pick, mOnMesgEvents, mWaitTime, frame);
        MessageContainerForPick cont = (MessageContainerForPick) result[0];
        if (cont == null) {
            //The thread is put on the pending queue waiting for an incoming request or 
            //for an onAlarm to go off, so return without further interpretation.
        	
        	if ((Boolean)result[1]) {
        		frame.onActivityTerminate(this);
        		return true;
        	} else {
                this.consumedMsgWithoutWaiting = false;
                currentPickState = PickState.WaitingForRequest;
                
                //Since we did not find a message to consume, we will persist the state if
                //an onAlarm with <for> is present
                if (!inRecovery) {
                    procInstance.getPersistenctMgr().updateState(this, 
                            currentPickState, mOnAlarm, mWaitTime, null, rObjs, 
                            TransactionInfo.getLocalTxInfo(), mContext.getStateContext().getState(),
                            frame.getBranchInvokeCounter());
                }

                // TO handle the correlation project (with pick) in cluster, passivate the instance
                // and clean up the instance in the memory
                procInstance.getClusterMgr().passivateInstance(this, mOnMesgEvents, frame, mWaitTime);
                return false;
        	}
        }
        
        currentPickState = PickState.RequestReceived;

        //Found an incoming request for this pick
        MessageContainer request = cont.getMessageContainer();
        ROnMessage sElem = (ROnMessage) cont.getInComingEventKey().getEventModel().getStartElement();
        if(request != null)
        	mContext.addRequest(sElem, request);
		
		if (mCreatesInstance) {
			procInstance.setParentMExArtifacts(request);
		}
        
        selectedChildAct = sElem;
        frame.onLineChange(this, selectedChildAct);
        
        postVarEvent (sElem.getRVariable(), request, rObjs, bpit.getCallFrame()); 
        
        /*
         * For clustering, to keep track of branches under active IMA, instance
         * should not be passivated if there is active IMA happening.
         */
        boolean incrementDone = procInstance.getClusterMgr().incrementActiveMessagingActivitiesCount(frame, cont.getInComingEventKey(), 
        		request);
        if (!incrementDone) {
            // this indicates some other branch of the flow
            // marked the instance as passivated and the requeset was put back on 
            // correlated waiting event map. 
            return true;
        }
        
        if (rObjs.getEngine().isReliabilityEnabled() 
                && procMgr.getOperationPattern(sElem).equals(Engine.IN_ONLY) 
                && rObjs.getBPELProcessManager().isPersistenceEnabled() 
                && request.getTransaction() != null) {

        	doPickOnMesgReliably(sElem, cont, frame, rObjs, procMgr);
            
        	/* If the call is successful we return false. Note that the CallFrame at 
             * this point are not stored in any Map or List. When the afterCompletion 
             * method of the Synchronization object is called, the Callframe will be put 
             * in the ReadyToRun queue. 
             */
            return false;
        } else {
            //consume message/execute corresponding OnMessage
        	if (!doPickOnMesg(sElem, request, frame, rObjs, procMgr)) {
            	return false;
            }
            
            //update child activity so that execute continues with OnMessage branch corresponding to request
            mChildAct = ((RActivityHolder) sElem).getChildActivity();
            
            // If pick happens, it is always true that the first start activity completes
            procInstance.startActivityCompletes();
            procInstance.getClusterMgr().decrementActiveMessagingActivitiesCount(frame);
            return true;
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
	
    private boolean doPickOnMesg(ROnMessage onMesg, 
            MessageContainer request, 
            ICallFrame frame,
            RequiredObjects rObjs, 
            BPELProcessManager procMgr) throws Exception {
    	
        mOnAlarm = null;
        RuntimeVariableImpl runtimeVariable = (RuntimeVariableImpl) mContext.getRuntimeVariable(onMesg.getRVariable()); 
        if (runtimeVariable == null) {
            runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(onMesg.getRVariable());
        }
        WSMessage message = (WSMessage)request.getContent();
        runtimeVariable.setWSMessage(message);
        Utility.setReferencesForExternalMessage(message, onMesg.getRVariable());
        mContext.setRuntimeVariable(runtimeVariable);
        StateContext stateCtx = mContext.getStateContext();
        mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                runtimeVariable, getBranchId());
        
        //TODO associate the business process with the rest of the correlations
        //to be initialised on the consumption of this message, by this onMessage.
        Object crmpInvId = request.getCRMPInvokeId();
        if (crmpInvId != null) {
            
            /* Create the crmpUpdateListValue and add it to the CRMPUpdateList on the ProcessInstance.
             * the corresponding reply activity will look up the value in this list to determine 
             * to update the entry with the replyActivityId and the responseObject.
             * the update call is synchronized in the IMAScopeBridge class..
             */
            String partnerLink = onMesg.getRPartner().getName();
            String operation = ((OnMessage) onMesg).getOperation();
            String crmpUpdatListValue = frame.getProcessInstance().getId() + partnerLink + operation;
            Context inst = (BPELProcessInstanceImpl) frame.getProcessInstance();
            inst.addToCRMPUpdateList(crmpUpdatListValue);  
            rObjs.setValue(RequiredObjects.CRMP_INVOKE_ID, crmpInvId);
        }
        //String mesgIdKey = frame.getId() + getStaticModelActivity().getUniqueId();
        //rObjs.setValue(mesgIdKey + ON_MESG_PROP_NAME, new Long(onMesg.getUniqueId()));
        TxPropagationObject txPropObj = frame.getProcessInstance().getTxPropagationObject();

        try {
        	// persist the receive, and then calculate the correlation values.
        	frame.getProcessInstance().getPersistenctMgr().updateState(this, 
        			currentPickState, mOnAlarm, mWaitTime, onMesg, rObjs, 
        			TransactionInfo.getLocalTxInfo(), mContext.getStateContext().getState(),
        			frame.getBranchInvokeCounter());
        	// process correlations
        	if (onMesg.getStartType() != Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {

        		BPELProcessInstance bpInstance = frame.getProcessInstance();
        		CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
        		CorrelationDefnWrapper corrDefnWrap = onMesg.getCorrelationDefnWrapper();
        		WSMessage wsMesg = (WSMessage) request.getContent();
        		// calculate the correlation values
        		CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMesg, 
        				corrDefnWrap, bpInstance); 
        		//set the correlation values on the instance object. 
        		//this method would check for integrety of the correlation values and also
        		// set the values on the state object, refer to the method java docs for info
        		corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, bpInstance); 
        	}
    	} catch (Exception e) {
    		// CHANGE: 10/18/07 
    		// In this case, we will not persist the fault immediately, the virtual Throw
    		// activity is created and its doAction() is called, which will mark the scope
    		// as faulted. In case of a crash in between these states, the recovery logic 
    		// will have to replay the act of calculating the correlation for this completed
    		// persisted receive.

    		// Send a status message for receives if it is an IN_ONLY operation pattern..
    		if(isInOnlyDoneStatus(procMgr, frame, onMesg)) {
    			mContext.removeRequest(onMesg, request);
    			procMgr.sendInOnlyRequestDoneStatus(request.getId());
    		}

    		throw e;
    	}

        //Send a status message for receives if it is an IN_ONLY operation pattern..
    	if(isInOnlyDoneStatus(procMgr, frame, onMesg)) {
    		mContext.removeRequest(onMesg, request);
			procMgr.sendInOnlyRequestDoneStatus(request.getId());
    	}

        return true;
    }  
    
    private void doPickOnMesgReliably(ROnMessage onMesg, 
                              MessageContainerForPick request, 
                              ICallFrame frame,
                              RequiredObjects rObjs, 
                              BPELProcessManager procMgr) throws Exception {
        
        txSynchronizer = new TxSynchronizer();
        txSynchronizer.frame = frame;
        txSynchronizer.request = request;
        txSynchronizer.rObjs = rObjs;
        
        try {
        	
        	MutableState state = mContext.getStateContext().getState();

        	//TODO: We need to reset the onAlarm if the transaction fails
        	txSynchronizer.onAlarm = mOnAlarm; 
        	mOnAlarm = null;

        	//String mesgIdKey = frame.getId() + getStaticModelActivity().getUniqueId();
        	//rObjs.setValue(mesgIdKey + ON_MESG_PROP_NAME, new Long(onMesg.getUniqueId()));
            
            txSynchronizer.runtimeVariable = (RuntimeVariableImpl) mContext.getRuntimeVariable(onMesg.getRVariable());
            if (txSynchronizer.runtimeVariable == null) {
                txSynchronizer.runtimeVariable = (RuntimeVariableImpl) mContext.createRuntimeVariable(onMesg.getRVariable());
            }
            WSMessage message = (WSMessage)request.getMessageContainer().getContent();
        	txSynchronizer.runtimeVariable.setWSMessage(message);
            Utility.setReferencesForExternalMessage(message, onMesg.getRVariable());

        	//Update the variable
        	state.updateVariable(getBranchId(), txSynchronizer.runtimeVariable);

        	// TODO associate the business process with the rest of the correlations
        	// to be initialised on the consumption of this message, by this onMessage.

        	TransactionInfo txInfo = new TransactionInfo (StateManager.TransactionType.XAParticipate,
        			request.getMessageContainer().getTransaction(), txSynchronizer, false);

        	// persist the receive activity (PC) as part of the TX
        	frame.getProcessInstance().getPersistenctMgr().updateState(this, 
        			currentPickState, mOnAlarm, mWaitTime, onMesg, rObjs, 
        			txInfo, mContext.getStateContext().getState(), 
        			frame.getBranchInvokeCounter());


        	txSynchronizer.xaConnection = txInfo.getConnection();

        	/* need to set the activity to ExecutionState.WaitingForTxComplete. TxSynchronizer.afterCompletion()
        	 * would enqueue the instance and would re-execute the receive(doAction()), where the logic to 
        	 * handle the condition in this case Status.STATUS_COMMITTED is executed.
        	 */
        	currentPickState = PickState.WaitingForTxComplete;

    		// success, send the DONE status for the InOnly ME
        	mContext.removeRequest(onMesg, request.getMessageContainer());
        	procMgr.sendInOnlyRequestDoneStatus(request.getMessageContainer().getId());
        	
        } catch (Exception excp) {
        	LOGGER.log(Level.WARNING, 
        			I18n.loc("BPCOR-6050: Exception occured while processing a reliable message exchange."), excp);

        	/* need to set the activity to ExecutionState.WaitingForTxComplete. TxSynchronizer.afterCompletion()
        	 * would enqueue the instance and would re-execute the activity, where the logic to 
        	 * handle the condition of failure is executed.
        	 */
        	this.currentPickState = PickState.WaitingForTxComplete;

			if(onMesg!= null && request != null && request.getMessageContainer() != null)
				mContext.removeRequest(onMesg, request.getMessageContainer());
            // failure, send the ERROR status for the InOnly ME
            rObjs.getBPELProcessManager().sendRequestError(
                    request.getMessageContainer().getId(), excp);
        }
    }
    
    private void cancelTransaction(ICallFrame frame, ROnMessage onMesg, RequiredObjects rObjs) {
     
        if (((RStartElement)onMesg).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY) {
            /*
             *If the Pick has createInstace=yes, we need to remove the process
             *instance from the memory. Note that no correlations have been associated,
             *no entries have been registered with the PickManager. Also, onAlarm
             *will not be present when createInstance is yes - so no entry in the database.  
             */ 
            rObjs.getBPELProcessManager().instanceComplete(frame.getProcessInstance());
            
        } else if (((RStartElement)onMesg).getStartType() != Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE 
                && ((RStartElement)onMesg).getStartType() != Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE_DUP_OPER) {
            
            if(consumedMsgWithoutWaiting) {
                resetPick(frame, rObjs);
            } else {
                resetToWaitingState(frame, rObjs);                        
            }
        } else {
            //TODO: If the Pick with createInstance=yes was in a flow with other
            //start activities then this cleanup is not sufficient. We need to address this
            //along with other problems related to handling correlations
        }
    }
    
    private void resetPick(ICallFrame frame, RequiredObjects rObjs) {

        /*
         *The pick tried to do an onMessage and it had found a corresponding request
         *the first time doAction was called, but the transaction failed. 
         *So we put the pick back in a state where it starts again afresh. 
         */
        
        frame.setProgramCounter(null);
        mOnAlarm = null;
        mWaitTime = 0;
        mChildAct = null;
        
        mOnMesgEvents = null;
        txSynchronizer = null;
        currentPickState = PickState.Initial;
        
        //Put in ReadyToRun Queue - this will have an effect of restarting 
        //the Pick activity
        BPELProcessManager processManager = rObjs.getBPELProcessManager();
        processManager.addToReadyToRunQueue(frame);
    }    
    
    private void resetToWaitingState(ICallFrame frame, RequiredObjects rObjs) {

        //The Pick did not create a new instance, so we need to restore the memory so the 
        //WaitingForRequest state

        //Set the onAlarm which was removed when the Transaction had started
        mOnAlarm = txSynchronizer.onAlarm;

        //Clear the Tx Synchronization Object
        txSynchronizer = null;

        rObjs.getBPELProcessManager().addToPickRequestPendingInstances(mOnMesgEvents, mWaitTime, frame);
        //TODO: The WaitingForRequest state needs to be synchronized with the above operation
        currentPickState = PickState.WaitingForRequest;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param frame DOCUMENT ME!
     * @param bpit DOCUMENT ME!
     * @param rObjs DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    protected boolean doContinuePick(ICallFrame frame, 
    								 BusinessProcessInstanceThread bpit, 
    								 RequiredObjects rObjs) throws Exception {
        boolean amIDone = super.doAction(frame, bpit, rObjs);
        if (amIDone) {
            frame.onSubActivityComplete(this, selectedChildAct);
            frame.onActivityComplete(this);
			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
        }
		return (amIDone) ? super.doPassControlToParent(frame, bpit, rObjs) : false;
    }
    
    /**
     * @see ActivityUnitImpl#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doActionOnRecovery(RecoveredCallFrame frame, 
                                      BusinessProcessInstanceThread bpit, 
                                      RequiredObjects rObjs) throws Exception {
        frame.convert();
        
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.fine(I18n.loc("BPCOR-3029: Recovery started after activity :") + frame.getPC().getName());
        }
        
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return doPassControlToParent(frame, bpit, rObjs);
		}
		
        if (mIsOnMesg || (mWaitTime < System.currentTimeMillis())) {
            // if an OnMessage was already recorded then start continuing the 
            // the path of the OnMessage.
            // Similarly if an OnAlarm timer value is satisfied, continue the path
            // of the OnAlarm.
            mChildAct = mChildActHolder.getChildActivity();
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.fine(I18n.loc("BPCOR-3030: Recovery started from") + mChildAct);
            }
            return doContinuePick(frame, bpit, rObjs);
        }

        boolean shouldContinueExec = doPick(frame, bpit, rObjs, mWaitTime, true);
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.fine(I18n.loc("BPCOR-3031: Is pick continuing execution :") + shouldContinueExec);
        }

        if (shouldContinueExec) {
            return doContinuePick(frame, bpit, rObjs);
        }

        return shouldContinueExec; // value is false, means pick is waiting
    }
    
    private class TxSynchronizer implements Synchronization {
        private ICallFrame frame;
        private RequiredObjects rObjs;
        private MessageContainerForPick request;
        private RuntimeVariableImpl runtimeVariable;
        //private CorrelationDefnValues corrDefnVals;
        //This is a temporary fix. When we move to automatic enlistment
        //we would not need to keep a reference to the XAConnection because
        //the appserver could be managing those.
        private Connection xaConnection;        
        private int txStatus;
        private ROnAlarm onAlarm = null;
        
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
            //Does nothing
        }
    }
	
    private void postVarEvent(RVariable variable, MessageContainer container, RequiredObjects objs, ICallFrame callFrame) {
    	if (callFrame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
			Variable var = new VariableImpl(variable, container.getContent(), mContext);
			Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
			List<Variable> vars = new ArrayList<Variable>();
			vars.add(var);
			variableMap.put(VariableEvent.VariableType.INPUT, vars);
			callFrame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, 
					container.getCRMPInvokeId(), null);
		}
	}

    public RActivityHolder getChildActHolder() {
        return mChildActHolder;
    }	
}
