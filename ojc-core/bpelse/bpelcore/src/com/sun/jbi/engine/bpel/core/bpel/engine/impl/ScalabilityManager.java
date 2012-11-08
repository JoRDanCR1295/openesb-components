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
 * @(#)ScalabilityManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ClusterManager;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactiveCallframeInfo;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactivityReason;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * TO handle the Memory Management for the BPEL Process
 * 
 * @author mbhasin
 */
public class ScalabilityManager {

    private static Logger LOGGER = Logger.getLogger(ScalabilityManager.class.getName());
    BPELProcessManager mProcMgr = null;
    Engine mEngine;
    ReadyToRunQueue mReadyToRunQueue = null;
    private Set mScalabltyPassvtdInstsSet = Collections.synchronizedSet(new HashSet());
    private Set mPendingReceivePassvtdInstsSet = Collections.synchronizedSet(new HashSet());
    /**
     * Map of the crmp id key and message container value. Message containers for the
     * following events are stored. Event.REPLY_FAULT, Event.RESPONSE_ERROR,
     * Event.ERROR, Event.DONE.
     */
    Map mCRMPEventMap = Collections.synchronizedMap(new HashMap());

    public ScalabilityManager(BPELProcessManager bpelProcessManager, Engine engine, ReadyToRunQueue readyToRunQueue) {
        mProcMgr = bpelProcessManager;
        mEngine = engine;
        mReadyToRunQueue = readyToRunQueue;
    }

    /**
     * The idea here is that when any instance is not moving forward either
     * due to wait activity, or a response from a invoke or a status
     * on one way invoke or reply, the instance variables can be passivated
     * and memory released.
     *
     * Since we are dealing with potentially huge number of instances
     * it is not good idea to batch all the variables for all the
     * qualified instances.
     */
    public void doPhase1ScalabilitySolution(long memRelTimeCriterion) {
        Map pendingMap = mProcMgr.getResponsePendingInstances();

        if (pendingMap.size() > 0) {
            dereferencePendingInstanceVars(pendingMap, memRelTimeCriterion);
        }

        pendingMap = mProcMgr.getPickRequestPendingInstances();
        if (pendingMap.size() > 0) {
            dereferencePendingInstanceVars(pendingMap, memRelTimeCriterion);
        }

        pendingMap = mProcMgr.getDonePendingInstances();
        if (pendingMap.size() > 0) {
            dereferencePendingInstanceVars(pendingMap, memRelTimeCriterion);
        }

        pendingMap = mProcMgr.getRequestPendingInstances();
        if (pendingMap.size() > 0) {
            dereferencePendingInstanceVars(pendingMap, memRelTimeCriterion);
        }

        passivateWaitingInstances(memRelTimeCriterion, true);
    }

    /**
     * Dereference the variables for the instances.
     * 
     * @param lock
     * @param memRelTimeCriterion
     */
    private void dereferencePendingInstanceVars(Map pendingInstancesMap, long memRelTimeCriterion) {
        ICallFrame frame = null;
        Map.Entry entry = null;
        InComingEventKeyImpl event = null;
        BPELProcessInstance instance = null;
        Iterator iter = null;

        Object processLevelLock = mProcMgr.getProcessLevelLock();

        synchronized (processLevelLock) {
            iter = pendingInstancesMap.entrySet().iterator();
            while (iter.hasNext()) {
                if (mEngine.getBpelMemoryMonitor().isBelowLowerMemoryThreshold()) {
                    break;
                }
                entry = (Map.Entry) iter.next();
                event = (InComingEventKeyImpl) entry.getKey();
                Object value = entry.getValue();

                if (value instanceof BusinessProcessInstanceThread) {
                    frame = ((BusinessProcessInstanceThread) value).getCallFrame();
                } else {
                    frame = (ICallFrame) value;
                }

                instance = frame.getProcessInstance();

                if (!instance.areVarsPassivated() && instance.passesIdlenssCriterion(memRelTimeCriterion)) {
                    dereferenceInstanceVariables(instance);
                }
            }
        }
    }

    /**
     * NOTE: Ideally, the criterion for waiting instances should be inverted compared to
     * other instances for passivation (instance held in response/request/status pending maps,
     * as for non-wait type, the logic being select the most aged first, but for instances
     * with defined expiry we want to select the last expiring instances first).
     * To keep the code simple, for now same criterion is used for all frames
     * for passivation, i.e how long they have been idle.
     * @param instPassivationTimeCriterion
     * @param isVariablePassivation TODO
     */
    private void passivateWaitingInstances(long instPassivationTimeCriterion, boolean isPhase1) {

        BPELProcessInstance instance = null;

        BusinessProcessInstanceThread bpit = null;
        Iterator<BusinessProcessInstanceThread> iter = null;
        Map idleInstancesMap = new HashMap<String, List>();

        Object processLock = mProcMgr.getProcessLevelLock();
        synchronized (processLock) {
            iter = getWaitingInstances().iterator();

            while (iter.hasNext()) {
                bpit = iter.next();

                // we need to do null check for the call frame a the special bpit for clustered pick
                // BusinessProcessInstanceThreadForClusteredPick does not have callframe set on it.
                if (bpit.getCallFrame() == null) {
                    continue;
                }

                if (bpit.getType() != BusinessProcessInstanceThread.SCALABILITY_PASSIVATED) {

                    instance = bpit.getCallFrame().getProcessInstance();
                    Object[] results = instance.qualifiesForInstancePassivation(instPassivationTimeCriterion);

                    if ((Boolean) results[0]) {
                        if (isPhase1) {
                            dereferenceInstanceVariables(instance);
                        } else {
                            List idleCallframesList = (List) results[1];
                            idleInstancesMap.put(instance.getId(), idleCallframesList);
                        }
                    }
                }
            }
            if (!isPhase1 && idleInstancesMap.size() > 0) {
                passivateInstancesForScalability(idleInstancesMap);
            }
        }
    }

    private Set<BusinessProcessInstanceThread> getWaitingInstances() {
        Set<BusinessProcessInstanceThread> waitingInstances = new HashSet<BusinessProcessInstanceThread>();
        BusinessProcessInstanceThread bpit = null;

        bpit = mReadyToRunQueue.getMostRecentlyExpiringBPIT();
        if (bpit != null) {
            waitingInstances.add(bpit);
            waitingInstances.addAll(mReadyToRunQueue.getWaitingBPIsSet());
        }
        return waitingInstances;
    }

    /**
     * De-reference Instance Variables
     * 
     * @param instance
     */
    private void dereferenceInstanceVariables(BPELProcessInstance instance) {
        Map variables = ((VariableScope) instance).getRuntimeVariables();

        Set entries = variables.entrySet();
        RVariable variableDef = null;
        RuntimeVariable rv = null;

        /*
         * If the instance is the middle of regular persistence the
         * acquireStateLock will return false. Scalability will be skipped
         * for such instances. Such instances can be visited later for
         * scalability passivation.
         */
        if (!instance.areVarsPassivated() && instance.acquirePersistenceLock(true)) {

            try {
                Iterator iter = entries.iterator();

                while (iter.hasNext()) {
                    Map.Entry e = (Map.Entry) iter.next();
                    rv = (RuntimeVariable) e.getValue();

                    /* Following is optimization:
                     * Don't passivate the XSD simple type variables. This wont
                     * help release much memory.
                     */
                    if (rv != null) {
                        if (rv.isSimpleType()) {
                            continue;
                        }

                        rv.passivateVariable();
                    }
                }
            } finally {
                instance.releasePersistenceLock(true);
            }
            instance.markVarsPassivated(true);
        }
    }

    public void logPersistedVariableDereferencing(String stateId, String variableName, long uniqueId, String scopeGuid) {
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.log(Level.FINE, I18n.loc("BPCOR-3023: " +
                    "System Memory Low - Performing PHASE 1 Scalability for inactive instance with id - {0}. Removing memory associated with already PERSISTED Variable - {1} (ID - {2})", stateId, variableName, String.valueOf(uniqueId)));
        }
    }

    /**
     * Passivate Variable 
     * 
     * @param stateId
     * @param varName TODO
     * @param uniqueId
     * @param chars
     * @param scopeId
     */
    public void passivateVariable(String stateId, String varName, long uniqueId, char[] chars, String scopeId) {
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.log(Level.FINE, I18n.loc("BPCOR-3071: " +
                    "System Memory Low - Performing PHASE 1 Scalability for inactive instance with id {0}. Passivating NON-PERSISTED variable - {1} (ID - {2})", stateId, varName, String.valueOf(uniqueId)));
        }
        mEngine.getStateManager().passivateVariable(stateId, uniqueId, chars, scopeId);
    }

    /**
     * Delete Passivated Variable 
     * 
     * @param stateId
     * @param uniqueId
     * @param scopeId
     */
    public void deletePassivatedVariable(String stateId, long uniqueId, String scopeId) {
        mEngine.getStateManager().deletePassivatedVariable(stateId, uniqueId, scopeId);
    }

    /**
     * Load the already Persisted variable from the database. Scalability earlier
     * de-referenced the variable from memory.
     *  
     * @param stateId
     * @param varName TODO
     * @param uniqueId
     * @param scopeId
     * @return
     */
    public Object loadPersistedVariable(String stateId, String varName, long uniqueId, String scopeId) {
        if (LOGGER.isLoggable(Level.INFO)) {
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-3072: " +
                    " Loading Persisted variable (Scalability removed the same from memory earlier) for instance with id {0}. Variable - {1} (ID - {2})", stateId, varName, String.valueOf(uniqueId)));
        }
        return mEngine.getStateManager().loadPersistedVariable(stateId, uniqueId, scopeId);
    }

    /**
     * Load the variables passivated by the scalability thread.
     * The scalability passivated record (variable) will also be
     * deleted in the database.
     * 
     * @param stateId
     * @param varName TODO
     * @param uniqueId
     * @param scopeId
     * @return
     */
    public Object loadPassivatedVariable(String stateId, String varName, long uniqueId, String scopeId) {
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.log(Level.FINE, I18n.loc("BPCOR-3073: " +
                    " Loading Passivated variable (Scalability persisted and removed the same from memory earlier) for instance with id {0}. Variable - {1} (ID - {2})", stateId, varName, String.valueOf(uniqueId)));
        }
        return mEngine.getStateManager().loadPassivatedVariable(stateId, uniqueId, scopeId);
    }

    /**
     * Visiting the Pending queues for instances waiting on correlated request, response,
     * status or for defined wait activity, the qualifying instances will be passivated. 
     * The qualification criterion being:
     * - If the process has no event handler defined
     * - If the execution is in the middle of flow, none of the branch of the 
     *   flow is under active execution
     * - The instance has been idle for a duration longer than passed duration value
     *   instPassivationTimeCriterion
     * - The memory levels are lower than the configured upper memory utilization levels
     *   
     * @param instPassivationTimeCriterion
     */
    public void doPhase2ScalabilitySolution(long instPassivationTimeCriterion) {

        Map pendingMap = mProcMgr.getRequestPendingInstances();

        if (pendingMap.size() > 0) {
            passivateIdleInstances(pendingMap, instPassivationTimeCriterion);
        }

        pendingMap = mProcMgr.getPickRequestPendingInstances();
        if (pendingMap.size() > 0) {
            passivateIdleInstances(pendingMap, instPassivationTimeCriterion);
        }

        pendingMap = mProcMgr.getResponsePendingInstances();
        if (pendingMap.size() > 0) {
            passivateIdleInstances(pendingMap, instPassivationTimeCriterion);
        }

        pendingMap = mProcMgr.getDonePendingInstances();
        if (pendingMap.size() > 0) {
            passivateIdleInstances(pendingMap, instPassivationTimeCriterion);
        }

        passivateWaitingInstances(instPassivationTimeCriterion, false);
    }

    private void passivateIdleInstances(Map pendingInstanceMap, long instPassivationTimeCriterion) {
        ICallFrame frame = null;
        Map.Entry entry = null;
        InComingEventKeyImpl event = null;
        BPELProcessInstance instance = null;

        Map idleInstancesMap = new HashMap<String, List>();

        Object processLevelLock = mProcMgr.getProcessLevelLock();

        synchronized (processLevelLock) {
            Iterator iter = pendingInstanceMap.entrySet().iterator();

            while (iter.hasNext()) {
                if (mEngine.getBpelMemoryMonitor().isBelowUpperMemoryThreshold()) {
                    break;
                }
                entry = (Map.Entry) iter.next();
                event = (InComingEventKeyImpl) entry.getKey();

                Object value = entry.getValue();

                if (value instanceof BusinessProcessInstanceThread) {
                    frame = ((BusinessProcessInstanceThread) value).getCallFrame();
                } else {
                    frame = (ICallFrame) value;
                }

                instance = frame.getProcessInstance();

                Object[] results = instance.qualifiesForInstancePassivation(instPassivationTimeCriterion);
                if ((Boolean) results[0]) {
                    List idleCallframesList = (List) results[1];
                    idleInstancesMap.put(instance.getId(), idleCallframesList);
                }
            }

            if (idleInstancesMap.size() > 0) {
                passivateInstancesForScalability(idleInstancesMap);
            }
        }
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#passivateInstance(java.util.List)
     */
    private void passivateInstancesForScalability(Map idleInstancesMap) {
        InactiveCallframeInfo callframeInfo = null;
        InactivityReason reason = null;
        Map.Entry<String, List> entry = null;
        String instanceId = null;

        Iterator instIter = idleInstancesMap.entrySet().iterator();
        List inactiveCallFrames = null;

        while (instIter.hasNext()) {
            entry = (Map.Entry<String, List>) instIter.next();
            inactiveCallFrames = entry.getValue();

            Iterator iter = inactiveCallFrames.iterator();

            while (iter.hasNext()) {
                callframeInfo = (InactiveCallframeInfo) iter.next();
                instanceId = callframeInfo.getCallframe().getBPId();

                reason = callframeInfo.getInactivityReason();
                if (LOGGER.isLoggable(Level.FINE)) {
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-3074: System Memory Low, Performing PHASE 2 Scalability (Instance Passivation) instance with id : {0} passivated. Reason ({1}) ", callframeInfo.getCallframe().getBPId(), reason.toString()));
                }

                if (reason == InactivityReason.PENDING_REQUEST) {
                    persistForScalabilityForPendingRequest(callframeInfo.getCallframe(), mProcMgr.getRequestPendingInstances());

                } else if (reason == InactivityReason.PENDING_REQUEST_FOR_PICK) {
                    persistForScalabilityForPendingRequest(callframeInfo.getCallframe(), mProcMgr.getPickRequestPendingInstances());
                    mProcMgr.pickMgrCleanUp(callframeInfo.getCallframe());

                } else if (reason == InactivityReason.PENDING_RESPONSE) {
                    persistForScalabilityForPendRespOrStatus(callframeInfo.getCallframe(), mProcMgr.getResponsePendingInstances());

                } else if (reason == InactivityReason.PENDING_STATUS) {
                    persistForScalabilityForPendRespOrStatus(callframeInfo.getCallframe(), mProcMgr.getDonePendingInstances());

                } else if (reason == InactivityReason.WAITING || reason == InactivityReason.ONALARM_FOR_PICK) {
                    passivateWaitingInstances(instanceId);
                    mProcMgr.pickMgrCleanUp(callframeInfo.getCallframe());
                }
            }
            // This needs to be done only once. Using the callframeInfo from the last iteration above.
            mProcMgr.cleanUp(callframeInfo.getCallframe().getProcessInstance());
            if (reason == InactivityReason.PENDING_REQUEST || reason == InactivityReason.PENDING_REQUEST_FOR_PICK) {
                mPendingReceivePassvtdInstsSet.add(entry.getKey());
            }
            mScalabltyPassvtdInstsSet.add(entry.getKey());
        }
    }

    /**
     * Passivate instances waiting for correlate messaging activity. Before the instance
     * is removed from memory, the correlation values are persisted.
     * 
     * @param callframe
     * @param pendingQueue
     */
    private void persistForScalabilityForPendingRequest(ICallFrame callframe, Map pendingQueue) {
        StateManager mgr = mEngine.getStateManager();
        MutableState state = callframe.getProgramCounter().getContext().getStateContext().getState();

        Map.Entry entry = null;
        ICallFrame cf = null;

        Iterator itr = pendingQueue.entrySet().iterator();

        while (itr.hasNext()) {
            entry = (Map.Entry) itr.next();
            cf = (ICallFrame) entry.getValue();

            if (cf.getBPId().equals(callframe.getBPId())) {

                if (mEngine.isClustered()) {
                    // the cluster passivation takes care of the persistence of the correlation
                    // values also.
                    // TODO refactor and move the common code in cluster and used by this call
                    // out and call the api to passivate
                    // directly instead of calling cluster manager.
                    ClusterManager clusterMgr = cf.getProcessInstance().getClusterMgr();
                    clusterMgr.passivateInstance(cf);
                } else {
                    //state = cf.getProgramCounter().getContext().getStateContext().getState();
                    //mgr.persistCorreations(state);
                }

                itr.remove();
            }
        }
    }

    /**
     * Passivate Instances waiting for response or status message. Before the callframe
     * is de-referenced, the outstanding message exchange id is persisted so that when 
     * the response/status comes back, the instance can be identified.
     *  
     * @param callframe
     * @param pendingQueue
     */
    private void persistForScalabilityForPendRespOrStatus(ICallFrame callframe, Map pendingQueue) {

        StateManager mgr = mEngine.getStateManager();
        //MutableState state = callframe.getProgramCounter().getContext().getStateContext().getState();

        Map.Entry entry = null;
        ICallFrame cf = null;
        String msgExId = null;

        Iterator itr = pendingQueue.entrySet().iterator();

        while (itr.hasNext()) {

            entry = (Map.Entry) itr.next();

            Object value = entry.getValue();

            if (value instanceof BusinessProcessInstanceThread) {
                cf = ((BusinessProcessInstanceThread) value).getCallFrame();
            } else {
                cf = (ICallFrame) value;
            }

            if (cf.getBPId().equals(callframe.getBPId())) {
                msgExId = (String) ((ResponseInComingEventKeyImpl) entry.getKey()).getMsgExId();

                // TODO the following two calls should be done in one transaction.
                mgr.persistOutstandingMsgExchID(msgExId, callframe.getBPId(), callframe.getProcess().getBPELId().toString());
                //mgr.persistCorreations(state);

                itr.remove();
            }
        }
    }

    /**
     * Mark the Waiting BPIT as scalability passivated and de-reference the
     * callframe
     * 
     * @param bpInstanceId
     */
    private void passivateWaitingInstances(String bpInstanceId) {

        BusinessProcessInstanceThread bpit = null;
        Object processLock = mProcMgr.getProcessLevelLock();

        synchronized (processLock) {
            ICallFrame callframe = null;
            bpit = mReadyToRunQueue.getMostRecentlyExpiringBPIT();

            if (bpit != null) {

                /*
                 * NOTE: The waitingBPISSet must be iterated first before
                 * checking the mMostRecentlyExpiringBPIT for the callframe
                 * match because if there are callframe(s) in the
                 * waitingBPISSet and also mMostRecentlyExpiringBPIT for the
                 * given instance, you need to get rid of the bpit in
                 * mWaitingBPIsSet first so that when the locally cached bpit
                 * (mMostRecentlyExpiringBPIT) is removed the next one from
                 * the set (waitingBPISSet) can be picked up.
                 */

                Iterator<BusinessProcessInstanceThread> iter = mReadyToRunQueue.getWaitingBPIsSet().iterator();

                while (iter.hasNext()) {
                    bpit = iter.next();
                    callframe = bpit.getCallFrame();
                    if ((bpit.getCallFrame() != null) && (bpInstanceId.equals(callframe.getBPId()))) {
                        bpit.markScalabilityPassivated();
                    }
                }

                callframe = bpit.getCallFrame();
                if ((callframe != null) && (bpInstanceId.equals(callframe.getBPId()))) {
                    bpit.markScalabilityPassivated();
                }
            }
        }
    }

    /**
     * Get the instance id of the scalability passivated instance while waiting for response
     * or status
     *
     * @param event
     * @param contents
     * @return
     */
    public String getScalabilityPassInstanceId(ResponseInComingEventKeyImpl event, MessageContainer contents) {

        String msgExId = (String) ((ResponseInComingEventKeyImpl) event).getMsgExId();
        String instanceId = mEngine.getStateManager().getInstanceForMsgEx(msgExId, event.getBPELProcess().getBPELId().toString());

        if (instanceId != null) {
            String crmpId = contents.getCRMPInvokeId();
            mCRMPEventMap.put(crmpId, contents);
        }

        return instanceId;
    }

    /**
     * Get Scalability passivated instance
     * 
     * @param corrIds
     * @return instance id of the passivated instance
     */
    String getScalabilityPassivatedInstance(List<CorrelationVal> corrIds) {
        return mEngine.getStateManager().getScalabilityPassivatedInstance(corrIds);
    }

    /**
     * Return the message container during re-execution of the Invoke unit
     * as part of activation of instance.
     *  
     * @param crmpId
     * @return
     */
    public MessageContainer getResponseForCRMPId(String crmpId) {
        return (MessageContainer) mCRMPEventMap.remove(crmpId);
    }

    /**
     * Activate (recover) the scalability passivated instance.
     * 
     * @param instanceId
     * @param reason
     * @return
     */
    public boolean activateScalabilityPassInstance(String instanceId, InactivityReason reason) {

        if (instanceId != null && mScalabltyPassvtdInstsSet.remove(instanceId)) {

            // the instance was scalability passivated. Recover the instance.
            try {
                if (LOGGER.isLoggable(Level.FINE)) {
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-3075: Activating Scalability Passivated Instance with id : {0}. Instance was earlier passivated while being inactive due to : {1} ", instanceId, reason.toString()));
                }

                mProcMgr.recoverInstance(instanceId);

            } catch (Exception e) {
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6157: Failed During Recovery Scalability passivated instance id : {0} ", instanceId), e);
            }
            if (reason == InactivityReason.PENDING_REQUEST || reason == InactivityReason.PENDING_REQUEST_FOR_PICK) {
                mPendingReceivePassvtdInstsSet.remove(instanceId);
            }
            return true;
        }

        return false;
    }

    /**
     * Returns of there are instances that are scalability passivated
     * 
     * @return
     */
    public boolean areInstancesScalPassivted() {
        return mScalabltyPassvtdInstsSet.size() > 0 ? true : false;
    }

    /**
     * NOTE: This api need to be revisited. 
     * Should be Optimized - monitor need not to active the instance.
     * 
     * Used by monitor to active the scalability passivated instances
     * @param instanceId
     * @return
     * @throws Exception
     */
    public boolean activateInstanceForMonitor(String instanceId) throws Exception {
        BusinessProcessInstanceThread bpit = null;

        Object processLock = mProcMgr.getProcessLevelLock();

        synchronized (processLock) {
            // NOTE: The waitingBPISSet must be iterated first. See similar comment in other 
            // methods of this class for details.

            Iterator<BusinessProcessInstanceThread> iter = mReadyToRunQueue.getWaitingBPIsSet().iterator();
            while (iter.hasNext()) {
                bpit = iter.next();
                if (bpit.getType() == BusinessProcessInstanceThread.SCALABILITY_PASSIVATED && bpit.getProcessInstanceId().equals(instanceId)) {
                    iter.remove();
                    mProcMgr.recreateInstance(instanceId);
                    return true;
                }
            }

            bpit = mReadyToRunQueue.getMostRecentlyExpiringBPIT();
            if (bpit != null && bpit.getType() == BusinessProcessInstanceThread.SCALABILITY_PASSIVATED && bpit.getProcessInstanceId().equals(instanceId)) {

                mReadyToRunQueue.selectNextBPITForCaching();
                mProcMgr.recreateInstance(instanceId);
                return true;
            }
        }
        return false;
    }

    /**
     * NOTE: This api need to be revisited.
     * Should be Optimized - monitor need not to active the instance.
     *
     * Used by monitor to activate instances.
     *
     * @param bpelId
     */
    public void activateInstancesForMonitor(String bpelId) {
        StateManager mgr = mEngine.getStateManager();
        try {
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPCOR-3076: Activating Scalability Passivated Instances due to monitor call with process id : {0} ", bpelId));
            }

            // TODO the instance list is in memory, the following call should be avoided.
            List<String> list = mgr.getScalabilityPassivatedInstances(bpelId);
            if (mPendingReceivePassvtdInstsSet.size() > 0) {
                list.addAll(mPendingReceivePassvtdInstsSet);
            }

            mProcMgr.recover(list);
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6165: Exception happened while recovering: Scalability Passivated instances for process id : {0} ", bpelId), e);
        }
    }

    /**
     * NOTE: This api need to be revisited. Is this api really needed?
     * Should be Optimized - monitor need not to active the instance.
     *  
     * Used by the monitor to activate scalability passivated instances
     * @return
     */
    public List<String> activateWaitingInstancesForMonitor() {
        BusinessProcessInstanceThread bpit = null;
        List<String> instanceIds = new ArrayList<String>();

        Object processLock = mProcMgr.getProcessLevelLock();

        synchronized (processLock) {
            bpit = mReadyToRunQueue.getMostRecentlyExpiringBPIT();
            if (bpit != null) {

                // NOTE: The waitingBPISSet must be iterated first before checking
                // the mMostRecentlyExpiringBPIT as if there are scalability passivated
                // instance in waitingBPISSet, they will be activated and removed. Hence
                // Subsequently, if the locally cached bpit (mMostRecentlyExpiringBPIT)
                // is also found to be scalability passivated, the next expiring bpit from
                // the set waitingBPISSet can be picked up.

                Iterator<BusinessProcessInstanceThread> iter = mReadyToRunQueue.getWaitingBPIsSet().iterator();
                while (iter.hasNext()) {
                    BusinessProcessInstanceThread bpthread = iter.next();
                    if (bpthread.getType() == BusinessProcessInstanceThread.SCALABILITY_PASSIVATED) {
                        instanceIds.add(bpthread.getProcessInstanceId());
                        iter.remove();
                    }
                }

                if (bpit != null && bpit.getType() == BusinessProcessInstanceThread.SCALABILITY_PASSIVATED) {
                    instanceIds.add(bpit.getProcessInstanceId());
                    mReadyToRunQueue.selectNextBPITForCaching();
                }
            }
        }

        try {
            mProcMgr.recover(instanceIds);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(
                    "Exception happened while recovery the Scalability Passivated instances for process ids : " + instanceIds);

        }

        return instanceIds;
    }

    /**
     * Utility to determine if there is a response/status for this CRMP id key. 
     * This map is populated in the scenario where an invoke activity is waiting
     * for a response/status and the instance is passivated. When the 
     * response/status is received the map is updated with the CRMP id and the 
     * instance is queued for recovery. 
     * 
     * @param crmpId the key to lookup the map
     * @return true, if the map contains the key
     */
    public boolean isScalabilityRecovery(String crmpId) {
        return mCRMPEventMap.containsKey(crmpId);
    }
}
