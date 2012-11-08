/*

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
 * @(#)InvokeUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Operation;
import javax.xml.namespace.QName;

import net.sf.hulp.measure.Measurement;

import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RMessagingElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBUnavailableException;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPITForClusteredInvoke;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPITForThrottling;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardFaultException;
import com.sun.jbi.engine.bpel.core.bpel.exception.SystemException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;

/**
 * Invoke activity unit implementation
 *
 * 1.Change Date: June 19th 2007 Comment: Invoke activity both for InOnly and
 * InOut message exchange will by default be XA enabled. if reliability(at
 * present always true) and persistence is enabled then the invoke activity will
 * begin a transaction and associate the transaction with the outbound message
 * exchange. In this mode the status/error(InOnly ME) and (resopnse/error) will
 * also be assumed to be associated with a transaction that the request had
 * initiated. Invoke activity in an engine with persistence enabled will
 * initiate a transaction and the message exchange will be done within the
 * transaction boundaries.
 *
 * @author Sun Microsystems
 */
public class InvokeUnitImpl extends ActivityUnitImpl {

    private static final Logger LOGGER = Logger.getLogger(InvokeUnitImpl.class.getName());
    private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.InvokeUnitImpl";
    private Measurement responseMesasure = null;
    private Fault mFault;
    /**
     * toggling of this field is required to support while loop. in a while
     * loop, an activity will be called many times, and as many times, execution
     * enters and exits the particular activity.
     */
    private boolean mWaiting = false;
    /**
     * crminvokeid*
     */
    private String mCRMInvokeId = null;

    /**
     * the throttling config value configured for this invoke activity
     */
    //private int mThrottleCount = 0;
    /**
     * Creates a new InvokeUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
//	private Map<VariableType, List<Variable>> mVarMap = new HashMap<VariableType, List<Variable>> ();
    public InvokeUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
        // initialize the throttling settings.
        /**
         * Remove this stuff - it's BUS-level ThrottlingConfig tConfig =
         * ((RInvoke) mAct).getServiceQuality(ThrottlingConfig.class); if
         * (tConfig != null) { mThrottleCount =
         * tConfig.getMaxConcurrencyLimit(); }
         */
    }

    /**
     * @see
     * com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     * BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit,
            RequiredObjects rObjs) throws Exception {

        if (mWaiting) {
            //if response is not received by same thread that put message on the DC.
            if (Measurement.isInstalled()) {
                responseMesasure.end();
            }


            if (!this.equals(frame.getProgramCounter())) {
                throw new RuntimeException(Utility.createIllegalPCChangeMessage(this, mAct, frame));
            }

            if (mContext.getFaultHandlingContext().isBeingTerminated()) {
                frame.onActivityTerminate(this);
                return doPassControlToParent(frame, bpit, rObjs);
            }

            mWaiting = false;
            MessageContainer msgC = consumeEvent(frame, bpit, rObjs);

            boolean isInvokeDone = false;

            isInvokeDone = process(frame, (RInvoke) mAct, msgC, rObjs);

            BPELProcessInstance procInstance = frame.getProcessInstance();
            // cluster manager decrement called irrespective of isInvokeDoneValue.
            procInstance.getClusterMgr().decrementActiveMessagingActivitiesCount(frame);

            if (isInvokeDone) {
                /**
                 * Remove this stuff - it's BUS-level
                 * decrementThrottlingCount(rObjs.getBPELProcessManager());
                 */
                if (!msgC.isStatus()) {
                    // do this for two way invokes only. one way invokes don't have an output variable.
                    postVarEvent((RVariable) ((RInvoke) mAct).getOutputBPELVariable(), msgC, rObjs, bpit.getCallFrame(), VariableType.OUTPUT);
                }
                frame.onActivityComplete(this);
                frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);

                return doPassControlToParent(frame, bpit, rObjs);
            }
            return false;
        }

        if (mContext.getFaultHandlingContext().isBeingTerminated()) {
            frame.onActivityTerminate(this);
            return doPassControlToParent(frame, bpit, rObjs);
        }

        frame.setProgramCounter(this);

        String crmpId = Utility.getCRMPInvokeId(frame, this);
        /*
         * Bug 483:
         * https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=483
         * The condition handled is the case of an instance with an invoke
         * activity that is waiting on a response/status, which is
         * passivated(memory phase 2 passivation). When the response/status is
         * received for such an invoke, ScalabilityManager schedules the
         * instance for recovery, as part of this invoke is replayed, with the
         * difference that the ScalabilityManager would have stored the
         * response/status object. The apis call isScabilityRecovery() returning
         * true indicates the above scenario and hence if throttling is enabled
         * it should not create an instance of BPITForThrottling.
         */

        /**
         * Remove this stuff - it's BUS-level capability if
         * (!(rObjs.getBPELProcessManager().isPersistenceEnabled() &&
         * rObjs.getBPELProcessManager().getMemoryMgr().isScalabilityRecovery(crmpId)))
         * { // check for throttling if ((mThrottleCount > 0) && !(bpit
         * instanceof BPITForThrottling)) {
         *
         * EndpointInfo endpointInfo = ((RInvoke)
         * mAct).getServiceQualityEndpointInfo(); BPELProcessManager manager =
         * rObjs.getBPELProcessManager(); manager.addThrottlingBPIT(frame,
         * endpointInfo, mThrottleCount);
         *
         * return false; } }
         *
         */
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);

        boolean invokeCompleted;

        boolean result = frame.getProcessInstance().getClusterMgr().incrementActiveMessagingActivitiesCount(frame, null, null);
        if (!result) {
            // this indicates some other branch of the flow marked the instance as passivated;
            // cannot execute further
            return false;
        }


        try {
            invokeCompleted = invoke(frame, rObjs);
        } catch (Exception Execp) {
            /**
             * Remove this stuff - it's BUS-level
             * decrementThrottlingCount(rObjs.getBPELProcessManager());
             */
            throw Execp;
        }

        // marks that this invoke activity has completed so that if the instance is marked for
        // passivation, it can be passivated
        // TODO still need to handle the case wherein if some exception is thrown while
        // executing this invoke (after incrementing the active messaging activities counter).

        // cluster manager decrement called irrespective of invokeCompleted value.
        frame.getProcessInstance().getClusterMgr().decrementActiveMessagingActivitiesCount(frame);

        if (invokeCompleted) {

            /**
             * Remove this stuff - it's BUS-level
             * decrementThrottlingCount(rObjs.getBPELProcessManager());
             */
            if (mContext.getFaultHandlingContext().isBeingTerminated()) {
                frame.onActivityTerminate(this);
                return true;
            } else {
                frame.onActivityComplete(this);
                frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
                return checkForSuspend(frame, bpit, rObjs);
            }
        }
        //Invoke not completed yet
        return false;
    }

    /**
     * This method is added to handle invokeFailSuspend case. When invoke failed
     * and the instance is suspended, a new InvokeUnit will be created to enable
     * replaying invoke at the time instance is resumed. When the instance is
     * resumed, the control should be passed back to parent and not return true,
     * if returns true, the instance will not proceed further since the thread
     * of control completes Subclass of this class will be created at the time
     * suspend is called on invoke failure
     *
     * @return
     */
    protected boolean checkForSuspend(ICallFrame frame, BusinessProcessInstanceThread bpit,
            RequiredObjects rObjs) throws Exception {
        // DEVNOTE: In the invokeUnitImplForSuspend, which overrides this method, the bpel trace is
        // done in the doPassControlToParent method. Here since we do not invoke that method, we need
        // to explicitly do a bpel trace on complete.
        BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
        return true;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        return mContext;
    }

    private boolean invoke(ICallFrame frame, RequiredObjects rObjs) throws Exception {

        RInvoke invoke = (RInvoke) mAct;
        MessageContainer msgContainer = null;
        RBPELProcess process = frame.getProcess();
        BPELProcessManager procMgr = rObjs.getBPELProcessManager();
        BPELProcessInstance procInstance = frame.getProcessInstance();

        // TODO the following should happen in a transaction like context.
        // associating the instance with the correlations
        // and invoking out.
        boolean hasErred = false;

        try {
            // determine type of Invoke
            boolean oneWay = isOneWay();
            // get input from either inputVariable or toPart
            WSMessage input = getInput();
            if (input == null) {
                throw new StandardFaultException(I18n.loc("BPCOR-3027: Cannot invoke using uninitialized variable!"));
            }

            mCRMInvokeId = Utility.getCRMPInvokeId(frame, this);

            // Set the CRMPInvokeId on the MessageContainer so that the invoke
            // could set it on the message exchange.
            MessageContainer con = MessageContainerFactory.createMessage(null, input, mCRMInvokeId,
                    null);

            postVarEvent((RVariable) invoke.getInputBPELVariable(), con, rObjs, frame, VariableType.INPUT);


            // do internal message assertion for the request message
            // before making the outbound request.
            CorrelationManager corrMgr = procMgr.getCorrMgr();
            BPELProcessInstance bpInstance = frame.getProcessInstance();

            // do correlation message assertion for initiate='no'&& 'join' for the request message
            CorrelationDefnWrapper defnWrap = invoke.getRequestCorrelationDefnWrapper();
            corrMgr.doCorrMessageAssertion(defnWrap, input, bpInstance);

            // do correlation message assertion for correlations with pattern='request-response'
            // on the request message for the case initiate='no' && 'join'
            defnWrap = invoke.getReqRespCorrelationDefnWrapperForRequest();
            corrMgr.doCorrMessageAssertion(defnWrap, input, bpInstance);

            // The transaction info returned will be null if the bp is atomic and/or persistence is disabled.
            TransactionInfo txInfo = setTxAndSecurityParameters(frame, rObjs, con);

            RuntimePartnerLink rPLink = mContext.getRuntimePartnerLink(invoke.getBPELPartnerLink());
            if (rPLink == null) {
                rPLink = mContext.createRuntimePartnerLink(invoke.getBPELPartnerLink());
            }

            if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
                /*
                 * we need to check here that no response exists for this crmp
                 * invoke id because the scalability might have passivated the
                 * instance that was pending on invoke response/status. When
                 * that happens, upon response/status for this instance,
                 * recovery will be called and the execution will hit this unit
                 * again, and for this case, we do not want to replay the
                 * invoke, because response already exist.
                 */
                msgContainer = procMgr.getMemoryMgr().getResponseForCRMPId(mCRMInvokeId);
            }

            if (msgContainer == null) {
                Measurement m1 = null;
                if (Measurement.isInstalled()) {
                    String measure = "Invoke Send : [partner-> " + invoke.getPartnerLink() + ", porttype-> " + invoke.getPortType() + ", opeation-> " + invoke.getOperation() + "]";
                    m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
                }
                //As per JBI spec operation name is QName and it inherits name space from PT. 
                //On inokve PT is optional, so we need to get from PLT.

                PartnerLink pl = invoke.getBPELPartnerLink();
                String nsOperation = pl.getPartnerRoleWSDLPortType().getQName().getNamespaceURI();
                QName qualifiedOperation = new QName(nsOperation, invoke.getOperation());

                // Actual invoke done here.
                Object meId = procMgr.invoke(rPLink, qualifiedOperation, con, oneWay, process);

                Utility.setReferencesForExternalMessage(input, (RVariable) invoke.getInputBPELVariable());

                if (Measurement.isInstalled()) {
                    m1.end();

                    String measure = "Waiting For Response : [partner-> " + invoke.getPartnerLink() + ", opeation-> " + invoke.getOperation() + "]";
                    responseMesasure = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
                }

                Object processLock = procMgr.getProcessLevelLock();

                if (oneWay) {
                    // check for status. if found continue execution, else enqueue callframe and return
//                    frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, rObjs, frame.getBPId(), 
//                            mVarMap, null, mCRMInvokeId); 
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process,
                            Event.DONE, meId);

                    synchronized (processLock) {
                        Map eventDoneMap = procMgr.getEventDoneMap();
                        msgContainer = (MessageContainer) eventDoneMap.remove(event);
                        if (msgContainer == null) {
                            if (mContext.getFaultHandlingContext().isBeingTerminated()) {
                                //Don't add the callframe to the pending queue.
                                //If a done is received it will be ignored
                                return true;
                            } else {

                                procMgr.addToPendingQueueWaitingForDone(event, frame);
                                mWaiting = true;
                                return false;
                            }
                        }
                    }
                } else {
                    //Check for response, if found continue execution else enqueue callframe and return
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process,
                            Event.REPLY_FAULT, meId);

                    Map eventResponseMap = procMgr.getEventResponseMap();

                    synchronized (processLock) {
                        msgContainer = (MessageContainer) eventResponseMap.remove(event);
                        if (msgContainer == null) {

                            if (mContext.getFaultHandlingContext().isBeingTerminated()) {
                                procMgr.addExpiredInvokeResponse(event);
                                return true;
                            } else {
                                BusinessProcessInstanceThread bpitForPendingResponse = null;

                                if (rObjs.getBPELProcessManager().isPersistenceEnabled() && rObjs.getEngine().isClustered()) {
                                    /*
                                     * The two way invoke to a sub business
                                     * process with correlated receive in
                                     * cluster might cause the sub bp instance
                                     * to be passivated and hence the invoked ME
                                     * will be lost for such cases. For such
                                     * cases we need to rely on the crmp
                                     * mechanism for finding the response
                                     * object. The logic is to periodically poll
                                     * the database for invoked process response
                                     * object and continue execution when either
                                     * of the following happen: a. the response
                                     * comes back b. response is found to be
                                     * persisted in the crmp table.
                                     *
                                     * Create BPIT and save this frame and crmp
                                     * id on it. The failover thread will
                                     * periodically poll the database to see if
                                     * the response is ready, in which case we
                                     * get the response container and execute
                                     * the frame.
                                     */
                                    bpitForPendingResponse = new BPITForClusteredInvoke(rObjs, event, mCRMInvokeId,
                                            meId, txInfo.getTransaction(), frame);
                                } else {
                                    bpitForPendingResponse = new BusinessProcessInstanceThread(rObjs.getBPELProcessManager(),
                                            rObjs.getEngine(), frame);
                                }

                                bpitForPendingResponse.setType(BusinessProcessInstanceThread.WAITING_FOR_REPLY);
                                bpitForPendingResponse.setMessageExchangeKey(meId);
                                procMgr.addToPendingQueueWaitingForReply(event, bpitForPendingResponse);
                                mWaiting = true;
                                return false;
                            }
                        }
                    }
                }

                if (Measurement.isInstalled()) {
                    responseMesasure.end();
                }
            }

        } catch (Exception ex) {
            hasErred = true;
            throw ex;
        } finally {
            //TODO undo correlation association with the instance
            //if (hasErred) {
            //}
        }

        //The done status or the response was found
        return process(frame, invoke, msgContainer, rObjs);

    }

    private MessageContainer consumeEvent(ICallFrame frame,
            BusinessProcessInstanceThread bpit,
            RequiredObjects rObjs) throws Exception {
        MessageContainer msgC = null;
        RBPELProcess process = frame.getProcess();
        BPELProcessManager procMgr = rObjs.getBPELProcessManager();
        InComingEventKeyImpl event = null;

        if (bpit.getType() == BusinessProcessInstanceThread.WAITING_FOR_DONE) {
            event = new ResponseInComingEventKeyImpl(process, Event.DONE,
                    bpit.getMessageExchangeKey());
            msgC = procMgr.receiveDone(event);
        } else if (bpit.getType() == BusinessProcessInstanceThread.WAITING_FOR_REPLY) {
            event = new ResponseInComingEventKeyImpl(process, Event.REPLY_FAULT,
                    bpit.getMessageExchangeKey());
            msgC = procMgr.receiveResponse(event);
        }

        return msgC;
    }

    private String findFaultName(Operation operation, javax.xml.namespace.QName faultMessageType) {

        Iterator faultsIter = operation.getFaults().values().iterator();
        while (faultsIter.hasNext()) {
            javax.wsdl.Fault fault = (javax.wsdl.Fault) faultsIter.next();
            if (faultMessageType.equals(fault.getMessage().getQName())) {
                return fault.getName();
            }
        }

        return null;
    }

    private boolean process(ICallFrame frame,
            RInvoke invoke,
            MessageContainer msgC,
            RequiredObjects rObjs) throws Exception {

        boolean returnFlag = true;
        Map<String, Object> props = msgC.getNMProperties();
        if (props != null) {
            WSMessage wsMessage = getInput();
            Set<Map.Entry<String, Object>> propEntrySet = props.entrySet();
            for (Map.Entry<String, Object> prop : propEntrySet) {
                wsMessage.setNMProperty(prop.getKey(), prop.getValue());
            }
        }

        if (msgC.isStatus()) {
            return processStatus(frame, invoke, msgC, rObjs);
        }

        try {
            // Two-way invoke has completed sucessfully(returned either
            // fault or response), the branchInvokeCounter is incremented.
            frame.incrementBranchInvokeCounter();

            if (msgC.isFault()) {

                javax.xml.namespace.QName faultMessageType = null;
                JBIMessageImpl message = (JBIMessageImpl) msgC.getContent();
                QName faultName = null;
                if (message != null) {
                    faultMessageType = message.getMessageType();

                    String faultString = message.getMessageName();
                    RMessagingElement mesgElem = (RMessagingElement) getStaticModelActivity();

                    if (faultString == null || faultString.equals("")) {
                        faultString = findFaultName(mesgElem.getWSDLOperation(), faultMessageType);
                    }

                    if (faultString != null && !faultString.equals("")) {
                        String namespaceURI = ((Invoke) mesgElem).getWSDLPortType().getQName().getNamespaceURI();
                        faultName = new QName(namespaceURI, faultString);
                    }
                }
                mFault = new FaultImpl(faultName, message);

                if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {
                    frame.getProcessInstance().getPersistenctMgr().updateState(this,
                            frame.getBranchInvokeCounter(), mContext.getStateContext().getState());

                    // persitence requirement for scope level fault handling and recovery.
                    // the fault data will be set on the scope and will have to be persisted prior to
                    // the virtual throw.
                    frame.getProcessInstance().getPersistenctMgr().updateState(mContext, mFault, getStaticModelActivity().getUniqueId(), getBranchId());

                    TransactionInfo txInfo = new TransactionInfo(StateManager.TransactionType.XAEnd,
                            msgC.getTransaction(), null, false);
                    try {
                        frame.getProcessInstance().getPersistenctMgr().endXA(
                                txInfo, mContext.getStateContext().getState());
                    } catch (DBUnavailableException due) {
                        // The call to the persistent manager failed because the DB was unavailable. Hence
                        // the DBUnavailableException. However the exception is thrown only after the retry logic
                        // ensures that the DB is back up. So the invoke should be tried again.
                        TransactionInfo txInfoCancel = new TransactionInfo(
                                StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                        frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfoCancel,
                                mContext.getStateContext().getState());

                        // put on the appropriate ready to run queue
                        addToAppropriateReadyToRunQueue(rObjs, frame);
                        return false;
                    }
                }

                // Persistence is done in the virtual throw unit for faults.
                Unit unit = ActivityUnitFactory.getInstance().createVirtualThrowUnit(mContext,
                        getEnclosingActivityUnit(), getStaticModelActivity(), getBranchId(), this,
                        false, mFault);
                // the doAction() of the synthetic throw unit always returns false the first anyways.
                returnFlag = unit.doAction(frame, null, rObjs);
                // Returning the status.
                rObjs.getBPELProcessManager().sendResponseDoneStatus(msgC.getId());

            } else if (msgC.isMessage()) {

                // The output\response message
                WSMessage result = (WSMessage) msgC.getContent();
                CorrelationManager corrMgr = rObjs.getBPELProcessManager().getCorrMgr();
                BPELProcessInstance bpInstance = frame.getProcessInstance();

                // do correlation message assertion for initiate='no'&& 'join' for the response message
                CorrelationDefnWrapper corrDefnWrap = invoke.getResponseCorrelationDefnWrapper();
                corrMgr.doCorrMessageAssertion(corrDefnWrap, result, bpInstance);

                // do correlation message assertion for correlations with pattern='request-response'
                // on the response message for the case initiate='no' && 'join'
                corrDefnWrap = invoke.getReqRespCorrelationDefnWrapperForResponse();
                corrMgr.doCorrMessageAssertion(corrDefnWrap, result, bpInstance);

                // get the input message
                WSMessage input = getInput();

                corrDefnWrap = invoke.getRequestCorrelationDefnWrapper();
                CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(input, corrDefnWrap, bpInstance);

                corrDefnWrap = invoke.getResponseCorrelationDefnWrapper();
                CorrelationDefnValues responseCorrVals = corrMgr.calculateCorrValues(result, corrDefnWrap, bpInstance);

                corrDefnWrap = invoke.getReqRespCorrelationDefnWrapperForRequest();
                CorrelationDefnValues reqRespPatternCorrValsForReqMsg = corrMgr.calculateCorrValues(input, corrDefnWrap, bpInstance);

                corrDefnWrap = invoke.getReqRespCorrelationDefnWrapperForResponse();
                CorrelationDefnValues reqRespPatternCorrValsForRespMsg = corrMgr.calculateCorrValues(result, corrDefnWrap, bpInstance);

//				if (rObjs.getEngine().isMonitorEnabled()) {
//					Variable var = new VariableImpl((RVariable) invoke
//							.getInputBPELVariable(), msgC.getContent(), mContext);
//					List<Variable> vars = new ArrayList<Variable>();
//					vars.add(var);
//					mVarMap.put(VariableEvent.VariableType.OUTPUT, vars);
//				}

                if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {

                    // check the integerty of the correlation values computed.
                    requestCorrVals = corrMgr.checkCorrValues(requestCorrVals,
                            bpInstance);
                    responseCorrVals = corrMgr.checkCorrValues(
                            responseCorrVals, bpInstance);
                    corrMgr.checkEquivalenceOfCorrValuesForReqRespPattern(
                            reqRespPatternCorrValsForReqMsg,
                            reqRespPatternCorrValsForRespMsg, bpInstance);
                    reqRespPatternCorrValsForReqMsg = corrMgr.checkCorrValues(
                            reqRespPatternCorrValsForReqMsg, bpInstance);

                    // since its in TX mode the correlation values for input and output message
                    // have to be calculated and set on the state object, but not on the instance.

                    List corrIDs = new ArrayList();
                    corrIDs.addAll(requestCorrVals.getCorrIDs());
                    corrIDs.addAll(responseCorrVals.getCorrIDs());
                    corrIDs.addAll(reqRespPatternCorrValsForReqMsg.getCorrIDs());

                    // Update the correlations in the state object
                    MutableState state = mContext.getStateContext().getState();
                    if (!corrIDs.isEmpty()) {
                        state.addCorrelation(corrIDs);
                    }

                    // set/update the output variable on the state.
                    RVariable outputVar = (RVariable) ((RInvoke) mAct).getOutputBPELVariable();
                    // check if the runtime variable exits on the context.
                    RuntimeVariable runtimeVar = mContext.getRuntimeVariable(outputVar);
                    if (runtimeVar == null) {
                        // create a new RuntimeVariable if one does not exist.
                        runtimeVar = mContext.createRuntimeVariable(outputVar);
                    }
                    // set the WSMessage(content) of the response on the RuntimeVariable
                    WSMessage message = (WSMessage) msgC.getContent();
                    runtimeVar.setWSMessage(message);
                    Utility.setReferencesForExternalMessage(message, outputVar);

                    // update the variable on the state
                    state.updateVariable(getBranchId(), runtimeVar);

                    frame.getProcessInstance().getPersistenctMgr().updateState(this,
                            frame.getBranchInvokeCounter(), mContext.getStateContext().getState());

                    TransactionInfo txInfo = new TransactionInfo(StateManager.TransactionType.XAEnd,
                            msgC.getTransaction(), null, false);
                    try {
                        frame.getProcessInstance().getPersistenctMgr().endXA(
                                txInfo, mContext.getStateContext().getState());
                    } catch (DBUnavailableException due) {
                        // The call to the persistent manager failed because the DB was unavailable. Hence
                        // the DBUnavailableException. However the exception is thrown only after the retry logic
                        // ensures that the DB is back up. So the invoke should be tried again.
                        TransactionInfo txInfoCancel = new TransactionInfo(
                                StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                        frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfoCancel,
                                mContext.getStateContext().getState());

                        // put on the appropriate ready to run queue
                        addToAppropriateReadyToRunQueue(rObjs, frame);
                        return false;
                    }


                    // Update the correlations in the memory, now that the transaction has been
                    // successfull.
                    corrMgr.associateCorrValuesWithInstance(requestCorrVals, bpInstance);
                    corrMgr.associateCorrValuesWithInstance(responseCorrVals, bpInstance);
                    corrMgr.associateCorrValuesWithInstance(reqRespPatternCorrValsForReqMsg, bpInstance);

                    //set the output variable on the context only(not on the state)
                    mContext.setRuntimeVariable(runtimeVar);
                } else {
                    // the following should happen in a transaction like context.
                    // associating the instance with the correlations
                    // and assigning the new value to the variable.

                    //check for the equivalence of the corr values for the request-response pattern
                    corrMgr.checkEquivalenceOfCorrValuesForReqRespPattern(reqRespPatternCorrValsForReqMsg,
                            reqRespPatternCorrValsForRespMsg, bpInstance);
                    // update the correlations for the input/request message
                    corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals, bpInstance);
                    // update the correlations for the output/response message
                    corrMgr.checkAndAssociateCorrValuesWithInstance(responseCorrVals, bpInstance);
                    // update the correlations for the 'request-response' pattern
                    corrMgr.checkAndAssociateCorrValuesWithInstance(reqRespPatternCorrValsForReqMsg,
                            bpInstance);
                    //set the output variable on the context
                    setOutput(frame, result);
                }

                // send the DONE status for the InOut ME
                rObjs.getBPELProcessManager().sendResponseDoneStatus(msgC.getId());

                returnFlag = true;
            }

        } catch (Exception ex) {
            if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {
                // if there is an error in calculating the correlations and since this message is
                // in the bounds of a transaction, it has to be cancelled.

                TransactionInfo txInfo = new TransactionInfo(
                        StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfo, mContext.getStateContext().getState());
            }

            // send the ERROR status for the InOut ME
            rObjs.getBPELProcessManager().sendResponseErrorStatus(msgC.getId(), ex);
            throw ex;
        }

        return returnFlag;
    }

    /**
     * Processes the follwoing status/errors. a. One-Way invoke (InOnly ME) The
     * provider can return a DONE of ERROR status. b. Two-Way invoke (InOut ME)
     * The provider can return an ERROR instead of the Response.
     *
     * @param frame
     * @param invoke
     * @param msgC
     * @param rObjs
     * @return
     */
    private boolean processStatus(ICallFrame frame, Invoke invoke,
            MessageContainer msgC, RequiredObjects rObjs) throws Exception {

        try {
            if (!msgC.isStatusError()) {

                // One-way invoke has completed sucessfully(returned with
                // with DONE status), the branchInvokeCounter is incremented.
                frame.incrementBranchInvokeCounter();

                // consume the done status, in case of one way we get a done
                // TODO do persistence.
                frame.getProcessInstance().getPersistenctMgr().updateState(this,
                        frame.getBranchInvokeCounter(),
                        mContext.getStateContext().getState());

                BPELProcessManager procMgr = rObjs.getBPELProcessManager();
                WSMessage input = getInput();

                BPELProcessInstance bpInstance = frame.getProcessInstance();
                CorrelationManager corrMgr = procMgr.getCorrMgr();
                CorrelationDefnWrapper corrDefnWrap = ((RInvoke) invoke).getRequestCorrelationDefnWrapper();
                CorrelationDefnValues requestCorrVals = null;

                if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {

                    // process correlations
                    try {
                        requestCorrVals = corrMgr.calculateCorrValues(input,
                                corrDefnWrap, bpInstance);
                        requestCorrVals = corrMgr.checkCorrValues(requestCorrVals,
                                bpInstance);

                    } catch (Exception excp) {
                        // if there is an error in calculating the correlations and
                        // since this message is within the bounds of a transaction,
                        // it has to be canceled.
                        TransactionInfo txInfo = new TransactionInfo(
                                StateManager.TransactionType.XACancel,
                                msgC.getTransaction(), null, true);
                        frame.getProcessInstance().getPersistenctMgr().cancelXA(
                                txInfo, mContext.getStateContext().getState());

                        throw excp;
                    }

                    // Update the correlations in the state object
                    List corrIDs = requestCorrVals.getCorrIDs();
                    MutableState state = mContext.getStateContext().getState();
                    if (!corrIDs.isEmpty()) {
                        state.addCorrelation(corrIDs);
                    }
                    TransactionInfo txInfo = new TransactionInfo(
                            StateManager.TransactionType.XAEnd,
                            msgC.getTransaction(), null, false);
                    try {
                        frame.getProcessInstance().getPersistenctMgr().endXA(txInfo,
                                mContext.getStateContext().getState());
                    } catch (DBUnavailableException due) {
                        // The call to the persistent manager failed because the DB was unavailable. Hence
                        // the DBUnavailableException. However the exception is thrown only after the retry logic
                        // ensures that the DB is back up. So the invoke should be tried again.
                        TransactionInfo txInfoCancel = new TransactionInfo(
                                StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                        frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfoCancel,
                                mContext.getStateContext().getState());

                        // put on the appropriate ready to run queue
                        addToAppropriateReadyToRunQueue(rObjs, frame);
                        return false;
                    }

                    // Update the correlations in the memory
                    corrMgr.associateCorrValuesWithInstance(requestCorrVals, bpInstance);

                } else {
                    // for non persistence mode, set the correlations on the
                    // input message to the instance this is done here only
                    // after the invoke (InOnly ME) has completed successfully.
                    requestCorrVals = corrMgr.calculateCorrValues(input,
                            corrDefnWrap, bpInstance);
                    corrMgr.checkAndAssociateCorrValuesWithInstance(
                            requestCorrVals, bpInstance);
                }

            } else {
                // process error for InOut (ERROR instead of response) and InOnly (ERROR status)
                if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {
                    TransactionInfo txInfo = new TransactionInfo(
                            StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                    frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfo,
                            mContext.getStateContext().getState());
                }
                /*
                 * Redelivery conditions handled 1) InOnly: a. on-failure =
                 * 'delete', retry till we receive a 'DONE' Status. b.
                 * on-failure = 'Redirect' or 'error' or 'suspend', retry till
                 * we receive 'DONE' or ('ERROR' with the
                 * RedeliveryStatus.hasFailed == true).
                 *
                 * 2) InOut: a. on-failure = 'error' or 'suspend', retry till we
                 * receive 'RESPONSE' or ('ERROR' with the
                 * RedeliveryStatus.hasFailed == true).
                 *
                 */
                RedeliveryConfig rConfig = ((RInvoke) mAct).getServiceQuality(RedeliveryConfig.class);

                if (rConfig != null) {
                    // redelivery is configured for this ME endpoint

                    Failure failure = rConfig.getFailure();

                    RedeliveryStatus redeliverStatus = msgC.getRedeliveryStatus();

                    if (((failure == Failure.redirect && !redeliverStatus.hasFailed()) || failure == Failure.delete) && isOneWay()) {
                        // endpoint and delete is applicable only for InOnly ME

                        // do the invoke again as redelivery.
                        return invoke(frame, rObjs);

                    } else if (failure == Failure.error || failure == Failure.suspend) {
                        // suspend is applicable for both InOnly & Inout

                        if (redeliverStatus != null) {

                            boolean suspend = redeliverStatus.hasFailed();

                            if (!suspend) {
                                // do the invoke again as redelivery.
                                return invoke(frame, rObjs);

                            } else if (frame.getProcessInstance().getMonitorMgr().generateMonitorEventsForProcess() && (failure == Failure.suspend)) {
                                // suspend of an instance is supported only if
                                // monitoring is enabled.
                                ActivityUnit newInvoke = new InvokeUnitImplForSuspend(
                                        getContext(), getEnclosingUnit(), mAct,
                                        getBranchId());
                                frame.setProgramCounter(newInvoke);
                                // suspend the instance.
                                frame.getProcessInstance().getMonitorMgr().suspendInstance();

                                addToAppropriateReadyToRunQueue(rObjs, frame);

                                // return false as we have a new bpit to execute.
                                return false;

                            }
                        }
                    }
                }

                /*
                 * increment the branchInvoke counter before throwing the
                 * exception NOTE: we are incrementing the counter after
                 * processing of the redelivery is exhausted, since redelivery
                 * replays the same invoke activity and hence has to have the
                 * same ServiceQuality.MESSAGE_ID.
                 */
                frame.incrementBranchInvokeCounter();

                // At this point the re-delivery attempts have been made, if
                // warranted. Now this
                // should be converted to a SystemException so that we may attempt
                // FH
                String message = Utility.appendLineNumberAndActName(
                        I18n.loc("BPCOR-6131: An Error status was received while doing an invoke (partnerLink={0}, "
                        + "portType={1}, operation={2})", ((Invoke) mAct).getPartnerLink(),
                        ((Invoke) mAct).getPortType(), ((Invoke) mAct).getOperation()), this);
                throw new SystemException(message, (Exception) msgC.getContent());
            }

            return true;
        } catch (Exception ex) {
            if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {
                // if there is an error in calculating the correlations and since this message is
                // in the bounds of a transaction, it has to be canceled.

                TransactionInfo txInfo = new TransactionInfo(
                        StateManager.TransactionType.XACancel, msgC.getTransaction(), null, true);
                frame.getProcessInstance().getPersistenctMgr().cancelXA(txInfo, mContext.getStateContext().getState());
            }
            throw ex;
        }
    }

    private WSMessage getInput() {
        RInvoke invoke = (RInvoke) mAct;
        RVariable inputVar = (RVariable) invoke.getInputBPELVariable();
        RuntimeVariable runtimeVar = mContext.getRuntimeVariable(inputVar);
        if ((runtimeVar == null) && (inputVar.getWSDLMessageType().getParts().size() == 0)) {
            runtimeVar = mContext.createRuntimeVariable(inputVar);
            Utility.initializeVariableValue(runtimeVar);
            mContext.setRuntimeVariable(runtimeVar);
            StateContext stateCtx = mContext.getStateContext();
            mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx,
                    runtimeVar, getBranchId());
        } else {
            Utility.verifyValue(inputVar.getName(), runtimeVar, true);
        }
        WSMessage input = runtimeVar.getWSMessage();
        return input;
    }

    private boolean isOneWay() {
        RInvoke invoke = (RInvoke) mAct;
        if (invoke.getOutputVariable() != null) {
            return false;
        }
        return true;
    }

    private void setOutput(ICallFrame frame, WSMessage output) {
        RInvoke invoke = (RInvoke) mAct;
        RVariable variable = (RVariable) invoke.getOutputBPELVariable();

        RuntimeVariable runtimeVariable = mContext.getRuntimeVariable(variable);
        if (runtimeVariable == null) {
            runtimeVariable = mContext.createRuntimeVariable(variable);
        }
        runtimeVariable.setWSMessage(output);
        output.addInternalReference(variable);
        mContext.setRuntimeVariable(runtimeVariable);
        StateContext stateCtx = mContext.getStateContext();
        mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx,
                runtimeVariable, getBranchId());
    }

    /**
     * Called during recovery to set the fault retrieved from the database
     *
     * @param fault
     */
    public void setFault(Fault fault) {
        this.mFault = fault;
    }

    public boolean doActionOnRecovery(
            RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

        frame.convert();

        if (frame.getPC() != null) {

            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE,
                        I18n.loc("BPCOR-3070: Recovery started from activity : {0}", frame.getPC().getName()));
            }
        }

        if (mContext.getFaultHandlingContext().isBeingTerminated()) {
            frame.onActivityTerminate(this);
            return doPassControlToParent(frame, bpit, rObjs);
        }

        if (mFault != null) {
            Unit unit = ActivityUnitFactory.getInstance().createVirtualThrowUnit(getContext(),
                    getEnclosingActivityUnit(), getStaticModelActivity(), getBranchId(), this,
                    false, mFault);
            //The doAction of VirtualThrowUnit will return false here
            return unit.doAction(frame, null, rObjs);

        } else {
            return doPassControlToParent(frame, bpit, rObjs);
        }
    }

    /*
     * (non-Javadoc) @see
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
     */
    public void completePendingInOnlyRequests() {
        throw new UnsupportedOperationException();
    }

    /*
     * This method creates a <code>PropertyContext</code> with the appropriate
     * settings. It also starts a new transaction if persistence is on and the
     * bp is not atomic. When a new transaction is started a
     * <code>TransactionInfo</code> object, which contains the transaction and
     * other pertinent information, is returned.
     *
     * @param frame @param rObjs @param container @return TransactionInfo
     */
    private TransactionInfo setTxAndSecurityParameters(ICallFrame frame, RequiredObjects rObjs,
            MessageContainer container) {
        TransactionInfo txInfo = null;
        BPELProcessInstance procInstance = frame.getProcessInstance();
        PropagationContext bpPropContext = procInstance.getPropagationContext();
        PropagationContext propContext;
        if (bpPropContext == null) {
            // The propagation context on the business process instance will be null only in the case of
            // a recovery. This will never happen when the bp is atomic and/or persistence is disabled.
            propContext = new PropagationContext(null);
        } else {
            propContext = new PropagationContext(bpPropContext.getParentMessageExchange());
        }

        if (rObjs.getBPELProcessManager().isBPAtomic() && (!frame.isEventHandlerCF())) {
            TxPropagationObject txPropObj = procInstance.getTxPropagationObject();
            propContext.setSecurityType(ConfigManager.SECURITYTYPE.ALWAYS);
            if (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.active)) {
                propContext.setTransactionType(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT);
            } else {
                propContext.setTransactionType(ConfigManager.TRANSACTIONTYPE.NEVER);
            }
            if (txPropObj.getAtomicTransactionType().equals(TxPropagationObject.TransactionType.started)) {
                container.setTransaction(txPropObj.getTransaction());
                container.setTxAtomicStarted();
            }
        } else if (rObjs.getEngine().isReliabilityEnabled() && rObjs.getBPELProcessManager().isPersistenceEnabled()) {
            txInfo = new TransactionInfo(StateManager.TransactionType.XAStartNew, null, null, true);
            frame.getProcessInstance().getPersistenctMgr().startXA(txInfo, mContext.getStateContext().getState());
            propContext.setSecurityType(ConfigManager.SECURITYTYPE.NEVER);
            propContext.setTransactionType(ConfigManager.TRANSACTIONTYPE.REQUIRES_NEW);
            propContext.setTransaction(txInfo.getTransaction());
            container.setTransaction(txInfo.getTransaction());
        } else {
            propContext.setSecurityType(ConfigManager.SECURITYTYPE.ALWAYS);
            propContext.setTransactionType(ConfigManager.TRANSACTIONTYPE.NEVER);
        }

        container.setPropagationContext(propContext);

        return txInfo;
    }

    private void postVarEvent(RVariable variable, MessageContainer container, RequiredObjects objs, ICallFrame frame, VariableEvent.VariableType type) {
        if (frame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
            Variable var = new VariableImpl(variable, container.getContent(), mContext);
            Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
            List<Variable> vars = new ArrayList<Variable>();
            vars.add(var);
            variableMap.put(type, vars);
            frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, null,
                    container.getCRMPInvokeId());
        }
    }

    @Deprecated
    private void decrementThrottlingCount(BPELProcessManager manager) {
        //if (mThrottleCount > 0) {
        //    manager.decrementThrottlingCount(((RInvoke) mAct).getServiceQualityEndpointInfo());
        //}
    }

    private void addToAppropriateReadyToRunQueue(RequiredObjects rObjs, ICallFrame frame) {
        /**
         * Remove this stuff - it's BUS-level if (mThrottleCount > 0) {
         *
         * decrementThrottlingCount(rObjs.getBPELProcessManager());
         *
         * EndpointInfo info = ((RInvoke) mAct).getServiceQualityEndpointInfo();
         *
         * rObjs.getBPELProcessManager().addThrottlingBPIT( frame, info,
         * mThrottleCount); } else {
         */
        BusinessProcessInstanceThread bpit = null;
        bpit = new BusinessProcessInstanceThread(rObjs.getBPELProcessManager(), rObjs.getEngine(), frame);
        // add this bpit to the RRQ, so that user can
        // resume the instance.
        rObjs.getBPELProcessManager().addToReadyToRunQueue(bpit);
        //}
    }
}
