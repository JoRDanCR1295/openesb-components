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
 * @(#)BPELProcessManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.xml.namespace.QName;
import org.w3c.dom.DocumentFragment;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CRMPLookup;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ScalabilityManager;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsFilter;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.XSDVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.MonitorManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.NonExpiredOnAlarmInstanceInfo;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;

/**
 * bpel process manager
 *
 * @author Sun Microsystems
 */
public interface BPELProcessManager {

    /**
     * handling incoming event key
     *
     * @param event incoming event key
     * @param contents message container
     *
     */
    void handleEvent(InComingEventKeyImpl event, MessageContainer contents);

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getBPELProcess()
     */
    RBPELProcess getBPELProcess();

    /**
     * @see Engine#addStartActivityModel(com.sun.jbi.engine.bpel.core.bpms.bpel.model.BPELProcess,
     *      com.sun.jbi.engine.bpel.core.bpms.common.model.bpel.runtime.RStartElement,
     *      java.lang.String)
     */
    void addStartActivityModel(RStartElement sa, String operPattern);

    /**
     * @see Engine#addStartActivityModel(com.sun.jbi.engine.bpel.core.bpms.bpel.model.BPELProcess,
     *      com.sun.jbi.engine.bpel.core.bpms.common.model.bpel.runtime.RStartElement,
     *      java.lang.String)
     */
    String getOperationPattern(RStartElement sa);

    /**
     * Pick request or put the frame in a pending queue. Also check whether the 
     * activity is being terminated.
     * @param pick the pick activity
     * @param events keys for incoming events
     * @param deadline dealine
     * @param frame callframe
     * @return An array containing the message (MessageContainerForPick) at the position 0 
     * and true or false at position 1. True means the activity is being terminated. 
     */
    Object[] pickRequestOrPutInPendingQueue(
            Pick pick, InComingEventKeyImpl[] events, long deadline, ICallFrame frame);

    /**
     * pickRequest without waiting
     *
     * @param events incoming event key
     * @param frame callframe
     *
     * @return MessageContainerForPick message container for pick
     */
    MessageContainerForPick pickRequestWithOutWaiting(
            InComingEventKeyImpl[] events, ICallFrame frame);

    /**
     * Add to the PickRequestPendingInstance queue in pick manager. Also
     * starts the onAlarm. Does the same thing as puts the pickRequest
     * but does not actually pick a request  
     * @param events incoming event key
     * @param deadline deadline
     * @param frame callframe
     * @return MessageContainerForPick message container for pick
     */
    void addToPickRequestPendingInstances(
            InComingEventKeyImpl[] events, long deadline, ICallFrame frame);

    /**
     * Receive request or put the frame in a pending queue. Also check whether the 
     * activity is being terminated.
     * 
     * @param event incoming event key
     * @param frame callframe
     * @return An array containing the message at the position 0 and true or false
     * at position 1. True means the activity is being terminated. 
     */
    Object[] receiveRequestOrPutInPendingQueue(InComingEventKeyImpl event, ICallFrame frame);

    /**
     * check to see of the request exists.
     * 
     * @param events
     * @return
     */
    boolean checkRequest(InComingEventKeyImpl[] events);

    /**
     * invoke
     *
     * @param pLink Invoke activity
     * @param operation TODO
     * @param con input value
     * @param oneWay when true it is oneway
     * @param process The business process definition to which invoke needs to be done
     * @param callFrame callframe
     * @return message exchange key
     */
    public Object invoke(RuntimePartnerLink pLink, QName operation, MessageContainer con, boolean oneWay, RBPELProcess process);

    /**
     * sends Fault
     *
     * @param faultName fautl defined on WSDL operation
     * @param msgContainer message container
     */
    void sendFault(QName faultName, MessageContainer msgContainer);

    /** send message exchange after setting with error
     * 
     * @param msgExchangeId Message ExchangeId
     * @param error exception
     */
    public void sendRequestError(String msgExchangeId, Exception error);

    /**
     * send done on message exchange after processing response
     * 
     * @param msgExchangeId
     */
    public void sendResponseDoneStatus(String msgExchangeId);

    /**
     * send error on message exchange after processing response

     * @param messageExchangeKey
     * @param ex
     */
    public void sendResponseErrorStatus(String messageExchangeKey, Exception ex);

    /**
     * send done on message exchange after processing request
     * @param msgExchangeId
     */
    public void sendInOnlyRequestDoneStatus(String msgExchangeId);

    /**
     * sends reply
     *
     * @param content is response
     * @param faultName if it is fault response
     */
    public void sendReply(MessageContainer content, QName faultName);

    /**
     * receive response
     *
     * @param event incoming event key
     *
     * @return MessageContainer message container
     */
    MessageContainer receiveResponse(InComingEventKeyImpl event);

    /**
     * receives DONE status
     *
     * @param event incoming event key
     *
     * @return MessageContainer message container
     */
    MessageContainer receiveDone(InComingEventKeyImpl event);

    /**
     * terminates
     * @param BPELProcessInstance
     */
    void terminate(BPELProcessInstance processInstance);

    /**
     * gets next scheduled time
     *
     * @return long next scheduled time
     */
    long getNextScheduledTime();

    /**
     * Adds a callframe to the ReadyToRunQueue with no "TIMEOUT". This is like 
     * addToReadyToRunQueueWithTimeout(0, ICallFrame);
     * @param callFrame 
     */
    public void addToReadyToRunQueue(ICallFrame callFrame);

    /**
     * This is for the Flow based child callframes. puts on ready to run queue all the child
     * callframes of the flow.
     *
     * @param childCallFrames child callframe
     */
    public void addToReadyToRunQueueForFlow(List childCallFrames);

    /**
     * adds to ready-to-run queue. If the timeout is zero it is equivalent of 
     * addToReadyToRunQueue(ICallFrame), although using addToReadyToRunQueue(ICallFrame) 
     * is preferable and slightly beneficial in terms of performance. It avoids an 
     * unnecessary time value check. 
     *
     * @param deadline deadline
     * @param callFrame callframe
     */
    public void addToReadyToRunQueueWithTimeout(long deadline, ICallFrame callFrame);

    public void recoverInstance(String instanceId) throws Exception;

    /**
     * list of instanceIds to recover.
     *
     * @param bpInstanceIds list of BP instance IDs
     *
     * @throws Exception Exception
     */
    void recover(List<String> bpInstanceIds) throws Exception;

    /**
     * process
     */
    void process();

    /**
     * creates webservice message
     *
     * @param messageName message type
     *
     * @return WSMessage webservice message
     */
    WSMessage createMessage(QName messageName);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    CorrelationManager getCorrMgr();

    void addToReadyToRunQueue(BusinessProcessInstanceThread bpit);

    /**
     * Remove business process instance wrapper from ready to run queue
     *
     * @param bpit business process instance wrapper to be removed
     * 
     * @return true if this contains bpit and successfully removed it
     */
    boolean removeFromReadyToRunQueue(BusinessProcessInstanceThread bpit);

    /**
     * API that is called when the instance is complete.
     *
     * @param bp bpel process instance
     */
    void instanceComplete(BPELProcessInstance bp);

    /**
     * Access method 
     * @return Map provides access to reply Map
     */
    public Map getEventResponseMap();

    /**
     * Access method
     * @return Map provides access to status Map
     */
    public Map getEventDoneMap();

    /**
     * Adds call frame to pending instance for Reply on given event.
     *
     * @param event InComingEvent
     */
    public void addToPendingQueueWaitingForReply(InComingEventKeyImpl event,
            BusinessProcessInstanceThread bpit);

    /**
     * Adds call frame to pending instance for status on given event.
     *
     * @param event InComingEvent
     */
    public void addToPendingQueueWaitingForDone(InComingEventKeyImpl event,
            ICallFrame frame);

    /**
     * Create a BP Instance for the given process.
     * @param model RBPELProcess
     * @param eng Engine
     * @param bpInstanceId - null when creating a new BPInstance else 
     * BPInstanceId while recovering.
     * @return BPELProcessInstance
     */
    public BPELProcessInstance createBPInstance(String bpInstanceId);

    /**
     * get all instances id in the memory.
     * @return An array list of all instance-ids in the memory
     */
    public List getInstances();

    /**
     * get all instances in the memory.
     * @return An array list of all instances in the memory
     */
    public List getProcessInstances();

    /**
     * message container for Pick interface
     *
     * @author Sun Microsystems
     * @version $Revision: 1.53 $
     */
    interface MessageContainerForPick {

        /**
         * gets message container
         *
         * @return MessageContainer message container
         */
        MessageContainer getMessageContainer();

        /**
         * gets incoming event key
         *
         * @return InComingEventKeyImpl incoming event key
         */
        InComingEventKeyImpl getInComingEventKey();
    }

    XSDVariable createXSDVariable(QName name);

    /**
     * add the out of order correlated events and also the events that due to race
     * condition in a flow cannot be processed in regular execution. This map 
     * will be vistied by the failover thread periodically to activate the instance.
     * 
     * @param event
     * @param contents
     */
    void addToCorrelatedWaitingEventsMap(InComingEventKeyImpl event, MessageContainer contents);

    /**
     * get the out of order correlated events. i.e the correlated message that 
     * arrives before the instance for the correlated message is not created.
     * This is used only when bpel se is participating in a clustered environment.
     * 
     * @return
     */
    List getCorrelatedWaitingEvents();

    /**
     * returns the object monitor for the CRMPLookup of the receive-reply pair.
     * @return
     */
    Object getCRMPMonitor();

    /**
     * Used to put the CRMPLookup value into wait state for request, used from the 
     * reply activity
     * @param key composite string key of partnerlink name and operation 
     * @param value
     */
    void putReplyInCRMPReplyMap(String key, CRMPLookup value);

    /**
     * Api used by the reply activity to check if there exist a Request to reply to in the 
     * Instance level map, that may have arrived prior to its own instance being created.
     * (Could happen in recovery logic where the MainBP is recovered and executed before
     * SubBP is recovered).
     * @param key composite key of bpId + PartnerLink + Operation
     * @return MessageContainer required to send the response
     */
    MessageContainer removeCRMPReqForRecoveringInsts(String key);

    /**
     * Api used by the CRMP recovery logic to add the request since the SubBP instance
     * has not yet been initialized.
     * @param key composite key of bpId + PartnerLink + Operation
     * @param req MessageContainer associated with the Inbound CRMP request
     */
    void addCRMPReqForRecoveringInsts(String key, MessageContainer req);

    /**
     * For clustered case, activate the instance.
     * 
     * @param instanceId
     * @return
     */
    int activateClusteringPassInstance(String instanceId);

    /**
     * Get non expited on alarms, for clustered case.
     * 
     * @return
     */
    List getNonExpiredOnAlarmRunningInstances();

    /**
     * add non expited on allram running instances, for clustered case.
     *  
     * @param instanceInfo
     */
    void addNonExpiredOnAlarmRunningInstances(NonExpiredOnAlarmInstanceInfo instanceInfo);

    /**
     * Construct the response object directly from the database.
     * 
     * @param crmpInvokeId
     * @param meId
     * @param replyVarId
     * @return
     */
    public MessageContainer getMessageContainerForCRMP(String crmpInvokeId, String meId, long replyVarId);

    /**
     * For clustered Sub Business Process Invoke, get the response object directly from the database
     * for the cases where the sub process instance is completed by another engine (due to passivation
     * and activation mechanism).
     */
    public void getResponsesForClusteredSubBPInvokes();

    /**
     * Called by a faulted scope. This method checks whether any of the callFrames is
     * waiting in any of the queue (request pending queue, response pending queue,
     * waiting for done status queue). It also checks if any of the callFrames is
     * is one corresponding to a timeout. If yes, it sets the timeout value to 0, so
     * that the callFrame is executed without any more wait.
     * @param callFrames
     */
    void activateWaitingCallFrames(Collection callFrames);

    /**
     * If an invoke waiting for a response is terminated, then an error needs to be sent
     * when the response is received. This is called to notify the BPELProcessManager of
     * such a response event.
     * @param event The response event for which an error should be sent
     */
    void addExpiredInvokeResponse(ResponseInComingEventKeyImpl event);

    /**
     * The scalability thread when kicks in would passivate all the dirty variables in the
     * persistence databsae.
     * 
     * @param memRelTimeCriterion
     */
    void doPhase1ScalabilitySolution(long memRelTimeCriterion);

    /**
     * When the Phase 1 does not bring the memory levels down, 
     * Phase 2 will be called that would completely passivate the 
     * instances.
     *  
     * @param instPassivationTimeCriterion
     */
    void doPhase2ScalabilitySolution(long instPassivationTimeCriterion);

    boolean isPersistenceEnabled();

    boolean isBPAtomic();

    boolean persistenceOptOut();

    boolean getGenerateEventsFlag();

    void setGenerateEventsFlag(boolean eventsFlag);

    /**
     * Specifies if missing 'from' data on all 'copy' activities in the process are to be ignored. 
     * 
     * @return true if missing from data is to be ignored for the entire process.
     */
    boolean ignoreMissingFromData();

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getEngineChannel()
     */
    DocumentFragment getExternalEndPoint(PartnerLink partnerLink);

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getEngineChannel()
     */
    DocumentFragment getEndPointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole);

    /**
     * Used to mark the instance as completed with enqueing the 'RUNNING' instances 
     * for excution during recovery. Used for the 'MaxInstances' support.
     *
     */
    void setRecoveryComplete();

    MonitorManager getMonitorManager(String instanceId);

    BPELProcessManager recreateInstance(String bpInstanceId) throws Exception;

    Collection<BPELProcessInstance> getProcessInstances(String string);

    /**
     * set the throttling count for this manager. 
     * the count is derived from the throttling count of the consumers
     * connected to the provisioning endpoint of this Process.
     * @param count
     */
    void setMaxInstances(int count);

    /**
     * Creates a <code>BPITForThrottling</code> instance for the given 
     * <code>EndpointInfo</code> and adds it to the ReadyToRunQueue.
     * It will initalize the <code>ThrottlingConfigInfo</code> for this
     * <code>EndpointInfo</code> if this is the first invocation for that
     * endpoint.
     * @param frame: <code>ICallFrame</code> associated with the endpoint.
     * @param endpointInfo: <code>EndpointInfo</code> 
     * @param mThrottleCount: throttling count configured for the endpoint. 
     */
    @Deprecated
    void addThrottlingBPIT(ICallFrame frame, EndpointInfo endpointInfo, int mThrottleCount);

    /**
     * Creates a <code>BPITForTransactionSync</code> instance for the given 
     * <code>instance</code> and adds it to the ReadyToRunQueue.
     * @param frame: <code>ICallFrame</code> associated with the endpoint.
     * @param processInstance: <code>BPELProcessInstance</code> 
     *  
     */
    void addTransactionSyncBPIT(ICallFrame frame, TxPropagationObject txPropObj, int transactionstatus);

    /**
     * Called to decrement the throttling count in the 
     * <code>ThrottlingConfigInfo</code> for the endpoint.
     * The count is decremented once the outbound activity either
     * completes successfully or has thrown a processing fault.
     * @param info: <code>EndpointInfo</code> 
     */
    void decrementThrottlingCount(EndpointInfo info);

    /**
     * Called by the <code>BPITForThrottling</code> created for an endpoint
     * invocation of the is process, to check if space is available to process
     * this throttling outbound activity.
     * See <code>ThrottlingConfigInfo.isReady()</code> method. 
     * @param info
     * @return
     */
    boolean canConsumeService(EndpointInfo info);

    /**
     * To get process level lock
     * 
     * @return
     */
    public Object getProcessLevelLock();

    /**
     * Removes the onEvent and onAlarm callframes from the pending queue and the ready to run queue.
     * @param eventCallFrames
     * @param alarmCallFrames
     */
    public void cleanUpEventHandlerCallFrames(Set<ICallFrame> eventCallFrames, Set<ICallFrame> alarmCallFrames);

    void cleanUp(BPELProcessInstance procInstance);

    public String getServiceAssemblyName();

    public String getServiceUnitName();

    /**
     * Cleans the waiting request list. An error is sent back for all expired requests
     */
    public void purgeWaitingRequests();

    Map getResponsePendingInstances();

    Map getPickRequestPendingInstances();

    Map getDonePendingInstances();

    Map getRequestPendingInstances();

    ScalabilityManager getMemoryMgr();

    void pickMgrCleanUp(ICallFrame callframe);

    /**
     * Sets cache of application Variables
     * @param appVars A cache of application variables
     */
    public void setApplicationVariables(Map<String, Object> appVars);

    /**
     * Fetches cache of application variables.
     * @return cache of application variables.
     */
    public Map<String, Object> getApplicationVariables();

    /**
     * Fetches cache of compiled XSL stylesheets, keyed by file uri.
     * @return cache of compiled XSL stylesheets, keyed by file uri.
     */
    public XslCache getXslCache();

    /**
     * Sets cache of compiled XSL stylesheets, keyed by file uri.
     * @param xslCache A cache of compiled XSL stylesheets.
     */
    public void setXslCache(XslCache xslCache);

    /**
     * Setter for the 'atomicTxType' attribute.
     * @param mAtomicTxType
     */
    public void setAtomicTxType(String type);

    /**
     * Getter for the 'atomicTxType' attribute.
     * @return String.
     */
    public String getAtomicTxType();

    public PerformanceManager getPerformanceManger();

    /**
     *
     * @return EventsFilter for the BPELProcess;
     */
    public EventsFilter getEventsFilter();

    /**
     *
     * @return List, it is even size in paris. name followed by value.
     */
    public List<String> getExtraNDC();

    /**
     *
     * @return boolean, when true Trace/Logging defined in BP are logged.
     */
    public boolean isLoggingEnabled();

    /**
     * To find the readiness to recover its instances
     * @return true - if associated SU state is started
     */
    public boolean isReadyToRecover();
}
