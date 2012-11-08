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
 * @(#)StateImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;


/** @author Sun Microsystems
 * Mar 3, 2005
 */
public class StateImpl implements MutableState, State {
    protected Mode mUpdateStatus;

    /** map of branch id to new variables referenced in a branch per scope
     * Map<Long, Map<Long, Map<Long, RuntimeVariable>>>
     */
    private Map<Long, Map<String, Map<Long, StateVariableWrapper>>> mBranchNewVarMap =
        Collections.synchronizedMap(new HashMap<Long, Map<String, Map<Long, StateVariableWrapper>>>());

    /** map of branch id to updated variables referenced in a branch per scope
     * Map<Long, Map<Long, Map<Long, RuntimeVariable>>>
     */
    private Map<Long, Map<String, Map<Long, StateVariableWrapper>>> mBranchUpdatedVarMap =
    Collections.synchronizedMap(new HashMap<Long, Map<String, Map<Long, StateVariableWrapper>>>());


    /** map of branch id to new partner links referenced in a branch per scope
     * Map<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>>
     */
    private Map<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>> mBranchNewPLinkMap =
        Collections.synchronizedMap(new HashMap<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>>());

    /** map of branch id to updated partner links referenced in a branch per scope
     * Map<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>>
     */
    private Map<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>> mBranchUpdatedPLinkMap =
        Collections.synchronizedMap(new HashMap<Long, Map<Long, Map<Long, StatePartnerLinkWrapper>>>());

    private Map<Long, Boolean> mPartnerLinkInsertState = new HashMap<Long, Boolean>();

    /** Map<Long, Long> keeps track of the branchId and the current Pc of that branch*/
    private Map<Long, Long> mBranchIdPCMap = new HashMap<Long, Long>();
    private Long mOldPC;
    private Long mNewPC;
    private Long mPickCompositeId;
    private Long mBranchInvokeCounter;
    protected Long mBranchId;

    /** The Set of correlations. Set<CorrelationVal> */
    private Set mCorrIdVals = new HashSet();

    // TODO    Map<Long, List<Long>> mScopedCorrMap = new HashMap<Long, List<Long>>();

    /** Map<Long, List<Long>> */
    private Map<Long, List<Long>> mFlowBranchIdMap = new HashMap<Long, List<Long>>();

    /** ForEach info per branch map, Map<Long, Map<Long, ForEachState>> */
    private Map<Long, Map<Long, ForEachState>> mBranchForEachMap =
        Collections.synchronizedMap(new HashMap<Long, Map<Long, ForEachState>>());

    /** Set<Long> */
    private Set<Long> mInvalidBranches = new HashSet<Long>();
    /** used by wait, Pick-onAlarm  */
    private Timestamp mTimerObject;
    private boolean mIsInFlow = false;
    private String mEngId;
    //private String mBPELId;
    private QName mBPELId;
    private String mId;

    private transient short mSynchFlag = 0;
    private Object mSynchObj = new Object();

    private CRMPState mCRMPState = null;

    //set true when suspend/resume occurs
    private boolean mStatusChanged = false;

    private Map<Long, Map<String, FaultHandlingContext>> mBranchNewScopeMap =
        Collections.synchronizedMap(new HashMap<Long, Map<String, FaultHandlingContext>>());

    private Map<Long, Map<String, FaultHandlingContext>> mBranchUpdatedScopeMap =
        Collections.synchronizedMap(new HashMap<Long, Map<String, FaultHandlingContext>>());
    
    /**
     * constructor
     * @param engId engine ID
     * @param bpelId BPEL ID
     * @param id State ID
     */
    public StateImpl(String engId, QName bpelId, String id) {
        mEngId = engId;
        mBPELId = bpelId;
        mId = id;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#exitScope(long)
     */
    public void exitScope(String scopeGuid) {
        if (scopeGuid.equals(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID)) {
            mUpdateStatus = Mode.DELETE;
            // when execution hits the end of the process, it doesn't make sense to
            // try and persist variable changes that might have happened between the
            // the last persistence check point and this point. Typically processes
            // won't end with assignments.
            cleanUp();
            return;
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#recover(com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState) */
    public void recover(RecoveredState rState, long branchId) {
        // recover foreaches
        List<ForEachDBO> foreachList = rState.getForEaches();
        if (foreachList != null) {
            for (ForEachDBO fedbo : foreachList) {
                addForEach(fedbo.getForEachId(), branchId, fedbo
                        .getStartCount(), fedbo.getFinalCount(), fedbo
                        .getCompletionCount());
                updateForEach(fedbo.getForEachId(), branchId, fedbo
                        .getCounter(), fedbo.getSuccesses());
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.State#finishedPersisting()
     */
    public void clearPC() {

        /*
         * Fix for Bug: 510. Situation where clearPC() is called without
         * updatePC() being called. (case of Invoke ME returning with an ERROR).
         * Not the optimal solution, have to be reconsidered in post sierra
         * release refactoring effort
         */
        synchronized (mSynchObj) {
            if (mSynchFlag == 0) {
                return;
            }
        }

        if (mStatusChanged) {
            mStatusChanged = false;
        }
        if (mUpdateStatus == Mode.INSERT) {
            mUpdateStatus = Mode.UPDATE;
        }
        if (mUpdateStatus == Mode.DELETE) {
            mBPELId = null;
            mId = null;
            return;
        }
        cleanUpVariables(mBranchId, true);
        cleanUpPartnerLinks(mBranchId, true);
        cleanUpScopes(mBranchId);
        cleanUpForEachSates(mBranchId);
        
        mOldPC = null;
        mNewPC = null;
        mBranchId = null;
        mCorrIdVals.clear();

        for (Long invalidBranch : mInvalidBranches) {
            mBranchIdPCMap.remove(invalidBranch);
        }

        mInvalidBranches.clear();
        mTimerObject = null;
        mPickCompositeId = null;

        mCRMPState = null;
        mBranchInvokeCounter = null;

        synchronized (mSynchObj) {
            mSynchFlag--;
            mSynchObj.notify();
        }
    }

    protected void cleanUpScopes(long branchId) {
        Collection<FaultHandlingContext> scopes = getScopeStates(false);
        for (FaultHandlingContext scope : scopes) {
            scope.markInserted();
        }
        //remove scopes from the branch scope map to avoid duplicate
        // persists.
        Map<String, FaultHandlingContext> scopeMap = null;
        scopeMap = mBranchNewScopeMap.get(branchId);
        if (scopeMap != null) {
            scopeMap.clear();
        }
        scopeMap = mBranchUpdatedScopeMap.get(branchId);
        if (scopeMap != null) {
            scopeMap.clear();
        }
    }

    protected void cleanUpVariables(long branchId, boolean finishedPersistence) {
        if (finishedPersistence) {
            for (Iterator<StateVariableWrapper> itr = getVariables(false, branchId).iterator(); itr.hasNext();) {
                RuntimeVariable var = itr.next().getVar();
                var.markInserted();
                var.setPersisted();
            }
        }

        // remove variables from branch variable map to avoid duplicate
        // persists
        Map<String, Map<Long, StateVariableWrapper>> scopedVars = null;
        scopedVars = mBranchUpdatedVarMap.get(branchId);
        if (scopedVars != null) {
            scopedVars.clear();
        }
        scopedVars = mBranchNewVarMap.get(branchId);
        if (scopedVars != null) {
            scopedVars.clear();
        }
    }

    protected void cleanUpPartnerLinks(long branchId, boolean finishedPersistence) {
        for (Iterator<StatePartnerLinkWrapper> itr = getPartnerLinks(false, branchId).iterator(); itr.hasNext();) {
            RuntimePartnerLink pLink = itr.next().getPLink();
            pLink.markInserted();
        }

        // remove variables from branch variable map to avoid duplicate
        // persists
        Map<Long, Map<Long, StatePartnerLinkWrapper>> scopedPLinks = null;
        scopedPLinks = mBranchUpdatedPLinkMap.get(branchId);
        if (scopedPLinks != null) {
            scopedPLinks.clear();
        }
        scopedPLinks = mBranchNewPLinkMap.get(branchId);
        if (scopedPLinks != null) {
            scopedPLinks.clear();
        }
    }

    private void cleanUpForEachSates(long branchId) {
        Map<Long, ForEachState> forEachStateMap = mBranchForEachMap
                .get(new Long(branchId));
        if (forEachStateMap != null && forEachStateMap.size() > 0) {
            Collection<ForEachState> states = forEachStateMap.values();
            for (ForEachState state : states) {
                state.markInserted();
            }
        }
    }


    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#addCorrelation(java.lang.Long,
     *      java.lang.String)
     */
    public void addCorrelation(CorrelationVal corrVal) {
        /*
         * TODO DEFER this, this is needed. if (mScopedCorrMap.get(scopeId) ==
         * null) { mScopedCorrMap.put(scopeId, new ArrayList<Long>()); }
         * mScopedCorrMap.get(scopeId).add(id);
         */
        mCorrIdVals.add(corrVal);
    }

    /** (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#addCorrelation(java.util.Collection)
     */
    public void addCorrelation(Collection corrValSet) {
        mCorrIdVals.addAll(corrValSet);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#removeCorrelations()
     */
    public synchronized void removeCorrelations() {
        mCorrIdVals.clear();
    }

    /**
     * cleans up the state. lets go of all the resources that it has held on
     * to represent the state from the last successful persistent point.
     * Retains the engId of the state object.
     */
    private void cleanUp() {
        // don't clean up the engineId value. Once assigned this is a constant
        // for the life of the engine.
        mBranchIdPCMap.clear();
        mOldPC = null;
        mNewPC = null;
        mCorrIdVals.clear();
        mInvalidBranches.clear();
        mFlowBranchIdMap.clear();
        mTimerObject = null;
        mPickCompositeId = null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#enterFlow(java.lang.Long, java.util.List)
     */
    public void enterFlow(long flowId, List branchIds) {
        mIsInFlow = true;

        if (branchIds == null) return;

        mFlowBranchIdMap.put(new Long(flowId), branchIds);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#enterScope(long)
     */
    public void enterScope(String scopeGuid) {
        if (scopeGuid.equals(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID)) {
            /* by default the mode is insert mode. still over writing the same
             * value just for consistent reasons. */
            mUpdateStatus = Mode.INSERT;
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#exitFlow(long, long)
     */
    public void exitFlow(long flowId, long parentBranchId) {

        Long lFlowId = new Long(flowId);
        List<Long> completedBranches = mFlowBranchIdMap.get(lFlowId);
        mFlowBranchIdMap.remove(lFlowId);

        if (completedBranches != null) {
            mInvalidBranches.addAll(completedBranches);
        }

        if (mFlowBranchIdMap.isEmpty()) {
            mIsInFlow = false;
        }
        updateScopeOnFlowExit(parentBranchId, completedBranches);
        updateVariablesOnFlowExit(parentBranchId, completedBranches);
        updatePartnerLinksOnFlowExit(parentBranchId, completedBranches);
    }

    /*
     * Move all the scopes in the completed flow branches to the parent branch of the flow
     * both for the updatedScope and newScope maps.
     *
     */
    private void updateScopeOnFlowExit(long parentBranchId,
            List<Long> completedBranches) {
        Long pBranchId = new Long(parentBranchId);

        // first for the mBranchNewScopeMap map.
        Map<String, FaultHandlingContext> mapOfRemvoedNewScopes = null;
        Map<String, FaultHandlingContext> newScopeMapOfTheParentBranch = null;
        newScopeMapOfTheParentBranch = mBranchNewScopeMap.get(parentBranchId);
        if (newScopeMapOfTheParentBranch == null) {
            newScopeMapOfTheParentBranch = new HashMap<String, FaultHandlingContext>();
            mBranchNewScopeMap.put(parentBranchId, newScopeMapOfTheParentBranch);
        }
        for (Long cBranchId : completedBranches) {
            mapOfRemvoedNewScopes = mBranchNewScopeMap.remove(cBranchId);
            if (mapOfRemvoedNewScopes != null && !mapOfRemvoedNewScopes.isEmpty()) {
                newScopeMapOfTheParentBranch.putAll(mapOfRemvoedNewScopes);
            }
        }

        // second for the mBranchUpdatedScopeMap map.
        Map<String, FaultHandlingContext> mapOfRemovedUpdatedScopes = null;
        Map<String, FaultHandlingContext> updatedScopeMapOfTheParentBranch = null;
        updatedScopeMapOfTheParentBranch = mBranchUpdatedScopeMap.get(parentBranchId);
        if (updatedScopeMapOfTheParentBranch == null) {
            updatedScopeMapOfTheParentBranch = new HashMap<String, FaultHandlingContext>();
            mBranchUpdatedScopeMap.put(parentBranchId, updatedScopeMapOfTheParentBranch);
        }
        for(Long cBranchId : completedBranches) {
            mapOfRemovedUpdatedScopes = mBranchUpdatedScopeMap.remove(cBranchId);
            if (mapOfRemovedUpdatedScopes != null && !mapOfRemovedUpdatedScopes.isEmpty()) {
                updatedScopeMapOfTheParentBranch.putAll(mapOfRemovedUpdatedScopes);
            }
        }
    }

    private void updateVariablesOnFlowExit(long parentBranchId,
            List<Long> completedBranches) {
        Long lParentBranchId = new Long(parentBranchId);
        Map<String, Map<Long, StateVariableWrapper>> scopeIdToNewVars = new HashMap<String, Map<Long, StateVariableWrapper>>();
        Map<String, Map<Long, StateVariableWrapper>> scopeIdToUpdatedVars = new HashMap<String, Map<Long, StateVariableWrapper>>();
        Map<String, Map<Long, StateVariableWrapper>> flowBranchScopeIdToNewVars = null;
        Map<String, Map<Long, StateVariableWrapper>> flowBranchScopeIdToUpdatedVars = null;
        Map<Long, StateVariableWrapper> fromVars = null;
        Map<Long, StateVariableWrapper> toVars = null;
        for (Long completedBranchId : completedBranches) {
            flowBranchScopeIdToNewVars = mBranchNewVarMap.remove(completedBranchId);
            if (flowBranchScopeIdToNewVars != null && !flowBranchScopeIdToNewVars.isEmpty()) {
                if (scopeIdToNewVars.isEmpty()) {
                    scopeIdToNewVars.putAll(flowBranchScopeIdToNewVars);
                } else {
                    for (String scopeId : flowBranchScopeIdToNewVars.keySet()) {
                        fromVars = flowBranchScopeIdToNewVars.get(scopeId);
                        if (fromVars == null || fromVars.isEmpty()) {
                            continue;
                        }
                        toVars = scopeIdToNewVars.get(scopeId);
                        if (toVars == null || toVars.isEmpty()) {
                            scopeIdToNewVars.put(scopeId, fromVars);
                        } else {
                            toVars.putAll(fromVars);
                        }
                    }
                }
            }
            flowBranchScopeIdToUpdatedVars = mBranchUpdatedVarMap.remove(completedBranchId);
            if (flowBranchScopeIdToUpdatedVars != null && !flowBranchScopeIdToUpdatedVars.isEmpty()) {
                if (scopeIdToUpdatedVars.isEmpty()) {
                    scopeIdToUpdatedVars.putAll(flowBranchScopeIdToUpdatedVars);
                } else {
                    for (String scopeId : flowBranchScopeIdToUpdatedVars.keySet()) {
                        fromVars = flowBranchScopeIdToUpdatedVars.get(scopeId);
                        if (fromVars == null || fromVars.isEmpty()) {
                            continue;
                        }
                        toVars = scopeIdToUpdatedVars.get(scopeId);
                        if (toVars == null || toVars.isEmpty()) {
                            scopeIdToUpdatedVars.put(scopeId, fromVars);
                        } else {
                            toVars.putAll(fromVars);
                        }
                    }
                }
            }
        }
        if (!scopeIdToNewVars.isEmpty()) {
            mBranchNewVarMap.put(lParentBranchId, scopeIdToNewVars);
        }
        if (!scopeIdToUpdatedVars.isEmpty()) {
            mBranchUpdatedVarMap.put(lParentBranchId, scopeIdToUpdatedVars);
        }
    }

    private void updatePartnerLinksOnFlowExit(long parentBranchId,
            List<Long> completedBranches) {
        Long lParentBranchId = new Long(parentBranchId);
        Map<Long, Map<Long, StatePartnerLinkWrapper>> scopeIdToNewPLinks = new HashMap<Long, Map<Long, StatePartnerLinkWrapper>>();
        Map<Long, Map<Long, StatePartnerLinkWrapper>> scopeIdToUpdatedPLinks = new HashMap<Long, Map<Long, StatePartnerLinkWrapper>>();
        Map<Long, Map<Long, StatePartnerLinkWrapper>> flowBranchScopeIdToNewPLinks = null;
        Map<Long, Map<Long, StatePartnerLinkWrapper>> flowBranchScopeIdToUpdatedPLinks = null;
        Map<Long, StatePartnerLinkWrapper> fromPLinks = null;
        Map<Long, StatePartnerLinkWrapper> toPLinks = null;
        for (Long completedBranchId : completedBranches) {
            flowBranchScopeIdToNewPLinks = mBranchNewPLinkMap.remove(completedBranchId);
            if (flowBranchScopeIdToNewPLinks != null && !flowBranchScopeIdToNewPLinks.isEmpty()) {
                if (scopeIdToNewPLinks.isEmpty()) {
                    scopeIdToNewPLinks.putAll(flowBranchScopeIdToNewPLinks);
                } else {
                    for (Long scopeId : flowBranchScopeIdToNewPLinks.keySet()) {
                        fromPLinks = flowBranchScopeIdToNewPLinks.get(scopeId);
                        if (fromPLinks == null || fromPLinks.isEmpty()) {
                            continue;
                        }
                        toPLinks = scopeIdToNewPLinks.get(scopeId);
                        if (toPLinks == null || toPLinks.isEmpty()) {
                            scopeIdToNewPLinks.put(scopeId, fromPLinks);
                        } else {
                            toPLinks.putAll(fromPLinks);
                        }
                    }
                }
            }
            flowBranchScopeIdToUpdatedPLinks = mBranchUpdatedPLinkMap.remove(completedBranchId);
            if (flowBranchScopeIdToUpdatedPLinks != null && !flowBranchScopeIdToUpdatedPLinks.isEmpty()) {
                if (scopeIdToUpdatedPLinks.isEmpty()) {
                    scopeIdToUpdatedPLinks.putAll(flowBranchScopeIdToUpdatedPLinks);
                } else {
                    for (Long scopeId : flowBranchScopeIdToUpdatedPLinks.keySet()) {
                        fromPLinks = flowBranchScopeIdToUpdatedPLinks.get(scopeId);
                        if (fromPLinks == null || fromPLinks.isEmpty()) {
                            continue;
                        }
                        toPLinks = scopeIdToUpdatedPLinks.get(scopeId);
                        if (toPLinks == null || toPLinks.isEmpty()) {
                            scopeIdToUpdatedPLinks.put(scopeId, fromPLinks);
                        } else {
                            toPLinks.putAll(fromPLinks);
                        }
                    }
                }
            }
        }
        if (!scopeIdToNewPLinks.isEmpty()) {
            mBranchNewPLinkMap.put(lParentBranchId, scopeIdToNewPLinks);
        }
        if (!scopeIdToUpdatedPLinks.isEmpty()) {
            mBranchUpdatedPLinkMap.put(lParentBranchId, scopeIdToUpdatedPLinks);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getCorrelations()
     */
    public Set getCorrelations() {
        return mCorrIdVals;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getForEach()
     */
    public Collection getForEach() {
        // At the time of this call to create the ForEachDBO objects, the
        // updatePC() call would have initialized the mBranchId
        return getForEach(mBranchId);
    }

    private Collection<ForEachState> getForEach(long branchId) {
        Collection<ForEachState> states = new ArrayList<ForEachState>();
        Map<Long, ForEachState> forEachSateMap = mBranchForEachMap
                .get(branchId);
        if (forEachSateMap != null && forEachSateMap.size() > 0) {
            states.addAll(forEachSateMap.values());
        }
        return states;
    }

    private boolean shouldPersistForEach(long branchId) {
        Collection<ForEachState> states = getForEach(branchId);
        boolean retVal = false;
        for(ForEachState state: states) {
            retVal = state.isDirty();
            if (retVal) {
                break;
            }
        }
        return retVal;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getUpdateMode()
     */
    public Mode getUpdateMode() {
        return mUpdateStatus;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#shouldPersistBeforeConcurrentExecution(long)
     */
    public boolean shouldPersistBeforeConcurrentExecution(long branchId) {
        return (getVariables(true, branchId).size() > 0
                || getVariables(false, branchId).size() > 0
                || getScopeStates(true, branchId).size() > 0
                || getScopeStates(false, branchId).size() > 0
                || shouldPersistForEach(branchId));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getScopeStates(boolean)
     */
    public Collection<FaultHandlingContext> getScopeStates(boolean for_update) {
        return getScopeStates(for_update, mBranchId);
    }

    protected Collection<FaultHandlingContext> getScopeStates(boolean for_update, long branchId) {
        Map<String, FaultHandlingContext> scopeMap = null;
        if (for_update) {
            scopeMap = mBranchUpdatedScopeMap.get(branchId);
        } else {
            scopeMap = mBranchNewScopeMap.get(branchId);
        }

        Collection<FaultHandlingContext> retVal = new ArrayList<FaultHandlingContext>();

        if (scopeMap == null) {
            return retVal;
        }
        Collection<FaultHandlingContext> scopeValuesFromScopeMap = scopeMap.values();
        if (scopeValuesFromScopeMap != null && !scopeValuesFromScopeMap.isEmpty()) {
            retVal.addAll(scopeValuesFromScopeMap);
        }

        return retVal;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getVariables(boolean)
     */
    public Collection<StateVariableWrapper> getVariables(boolean for_update) {
        return getVariables(for_update, mBranchId);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getPartnerLinks(boolean)
     */
    public Collection<StatePartnerLinkWrapper> getPartnerLinks(boolean for_update) {
        return getPartnerLinks(for_update, mBranchId);
    }

    protected Collection<StateVariableWrapper> getVariables(boolean for_update, long branchId) {
        Map<String, Map<Long, StateVariableWrapper>> scopeIdToVars = null;
        if (for_update) {
            scopeIdToVars = mBranchUpdatedVarMap.get(branchId);
        } else {
            scopeIdToVars = mBranchNewVarMap.get(branchId);
        }
        if (scopeIdToVars == null) {
            return new ArrayList<StateVariableWrapper>();
        }
        Collection<Map<Long, StateVariableWrapper>> scopeIdVars = scopeIdToVars.values();
        Collection<StateVariableWrapper> retVal = new ArrayList<StateVariableWrapper>();
        for (Map<Long, StateVariableWrapper> item : scopeIdVars) {
            retVal.addAll(item.values());
        }
        return retVal;
    }

    protected Collection<StatePartnerLinkWrapper> getPartnerLinks(boolean for_update, long branchId) {
        Map<Long, Map<Long, StatePartnerLinkWrapper>> scopeIdToPLinks = null;
        if (for_update) {
            scopeIdToPLinks = mBranchUpdatedPLinkMap.get(branchId);
        } else {
            scopeIdToPLinks = mBranchNewPLinkMap.get(branchId);
        }
        if (scopeIdToPLinks == null) {
            return new ArrayList<StatePartnerLinkWrapper>();
        }
        Collection<Map<Long, StatePartnerLinkWrapper>> scopeIdPLinks = scopeIdToPLinks.values();
        Collection<StatePartnerLinkWrapper> retVal = new ArrayList<StatePartnerLinkWrapper>();
        for (Map<Long, StatePartnerLinkWrapper> item : scopeIdPLinks) {
            retVal.addAll(item.values());
        }
        return retVal;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#markInserted()
     */
    public void markInserted() {
        mUpdateStatus = Mode.UPDATE;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updatePC(long, long, long)
     */
    public void updatePC(long branchId, long pcId, long nanoSeconds, 
    		long branchInvokeCounter) {
    	updatePCWithBranchInvokeCounter(branchId, pcId, branchInvokeCounter);
        mTimerObject = new Timestamp(nanoSeconds);
    }

    /** @see MutableState#updatePCWithPickCompositeId(long, long, long)
     */
    public void updatePCWithPickCompositeId(long branchId, long activityId,
        long compositeId, long branchInvokeCounter) {
    	updatePCWithBranchInvokeCounter(branchId, activityId, branchInvokeCounter);
        mPickCompositeId = new Long(compositeId);
    }

    /** @see MutableState#updatePCWithPickCompositeId(long, long, long, long)
     */
    public void updatePCWithPickCompositeId(long branchId, long activityId,
        long nanoSeconds, long compositeId, long branchInvokeCounter) {
        updatePC(branchId, activityId, nanoSeconds, branchInvokeCounter);
        mPickCompositeId = new Long(compositeId);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updatePC(long, long)
     */
    public void updatePC(long branchId, long pcId) {

        synchronized(mSynchObj) {
            if (mSynchFlag != 0) {
                try {
                    mSynchObj.wait();
                } catch (InterruptedException ex) {
                    throw new RuntimeException(ex);
                }
            }
            mSynchFlag++;
        }

        Long lBranchId = new Long(branchId);
        Long lPCId = new Long(pcId);

        if (mBranchIdPCMap.get(lBranchId) == null) {
            mOldPC = null;
            mNewPC = lPCId;
            mBranchIdPCMap.put(lBranchId, lPCId);
        } else {
            mOldPC = mBranchIdPCMap.get(lBranchId);
            mNewPC = lPCId;
            mBranchIdPCMap.put(lBranchId, lPCId);
        }
        mBranchId = lBranchId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updatePCWithBranchInvokeCounter(long, long, long)
     */
    public void updatePCWithBranchInvokeCounter(long branchId, long activityId, long branchInvokeCounter) {
        updatePC(branchId, activityId);
        mBranchInvokeCounter = branchInvokeCounter;
    }


    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updateForEach(long,
     *      long, int, int)
     */
    public void updateForEach(long pcId, long branchId, int counter,
            int successes) {
        Map<Long, ForEachState> forEachStateMap = mBranchForEachMap
                .get(new Long(branchId));
        if (forEachStateMap != null && forEachStateMap.size() > 0) {
            ForEachState fes = forEachStateMap.get(new Long(pcId));
            boolean isRecovering = false;
            if (!fes.isStarted()) {
                if (fes.isInserted()) {
                    // recovered, and still running...so mark ForEachState
                    // started
                    fes.start();
                } else {
                    // recovering now...mark inserted since we're initializing
                    // from DB
                    fes.markInserted();
                    isRecovering = true;
                }
            }
            // don't need to reset activity id
            fes.updateState(counter, successes);
            // if recovering, cannot be dirty
            if (isRecovering) {
                fes.markClean();
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#addForEach(long,
     *      long, int, int, int)
     */
    public void addForEach(long pcId, long branchId, int startCount,
            int finalCount, int completionCount) {
        Map<Long, ForEachState> forEachStateMap = mBranchForEachMap
                .get(new Long(branchId));


        if (forEachStateMap == null) {
            // create for the first time.
            forEachStateMap = new HashMap<Long, ForEachState>();
            mBranchForEachMap.put(new Long(branchId), forEachStateMap);
        }
        ForEachState fes = forEachStateMap.get(new Long(pcId));
        if (fes == null) {
            fes = new ForEachState(pcId);
            forEachStateMap.put(new Long(pcId), fes);
        }
        fes.reset(startCount, finalCount, completionCount);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#startForEach(long,
     *      long)
     */
    public void startForEach(long pcId, long branchId) {
        Map<Long, ForEachState> forEachStateMap = mBranchForEachMap
                .get(new Long(branchId));
        if (forEachStateMap != null && forEachStateMap.size() > 0) {
            ForEachState fes = forEachStateMap.get(new Long(pcId));
            fes.start();
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#endForEach(long,
     *      long)
     */
    public void endForEach(long pcId, long branchId) {
        // don't remove, nested forEach should not be reinserted into DB
        Map<Long, ForEachState> forEachStateMap = mBranchForEachMap
                .get(new Long(branchId));
        if (forEachStateMap != null && forEachStateMap.size() > 0) {
            ForEachState fes = forEachStateMap.get(new Long(pcId));
            fes.end();
        }
    }

    public void updateScope(long branchId, FaultHandlingContext scope) {
        Long lBranchId = new Long(branchId);
        String scopeId = scope.getScopeGuid();
        Map<String, FaultHandlingContext>scopeMap = null;
        if (scope.isInserted()) {
            scopeMap = mBranchUpdatedScopeMap.get(lBranchId);
            if (scopeMap == null) {
                scopeMap = new HashMap<String, FaultHandlingContext>();
                mBranchUpdatedScopeMap.put(branchId, scopeMap);
            }
        } else {
            scopeMap = mBranchNewScopeMap.get(lBranchId);
            if (scopeMap == null) {
                scopeMap = new HashMap<String, FaultHandlingContext>();
                mBranchNewScopeMap.put(branchId, scopeMap);
            }
        }
        scopeMap.put(scopeId, scope);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updateVariable(long, RuntimeVariable)
     */
    public void updateVariable(long branchId, RuntimeVariable variable) {
        Long varID = new Long(variable.getVariableDef().getUniqueId());

        // register variable copies by branch
        Long lBranchID = new Long(branchId);

        String varScopeId = variable.getScopeGuid();

        Map<String, Map<Long, StateVariableWrapper>> scopeMap = null;
        Map<Long, StateVariableWrapper> branchVars = null;
        if (variable.isInserted()) {
            scopeMap = mBranchUpdatedVarMap.get(lBranchID);
            if (scopeMap == null) {
                scopeMap = new HashMap<String, Map<Long, StateVariableWrapper>>();
                mBranchUpdatedVarMap.put(lBranchID, scopeMap);
                branchVars = new HashMap<Long, StateVariableWrapper>();
                scopeMap.put(varScopeId, branchVars);
            } else {
                branchVars = scopeMap.get(varScopeId);
                if (branchVars == null) {
                    branchVars = new HashMap<Long, StateVariableWrapper>();
                    scopeMap.put(varScopeId, branchVars);
                }
            }
        } else {
            scopeMap = mBranchNewVarMap.get(lBranchID);
            if (scopeMap == null) {
                scopeMap = new HashMap<String, Map<Long, StateVariableWrapper>>();
                mBranchNewVarMap.put(lBranchID, scopeMap);
                branchVars = new HashMap<Long, StateVariableWrapper>();
                scopeMap.put(varScopeId, branchVars);
            } else {
                branchVars = scopeMap.get(varScopeId);
                if (branchVars == null) {
                    branchVars = new HashMap<Long, StateVariableWrapper>();
                    scopeMap.put(varScopeId, branchVars);
                }
            }
        }
        branchVars.put(varID, new StateVariableWrapper(getId(), variable));
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updatePartnerLink(long, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink)
     */
    public void updatePartnerLink(long branchId, RuntimePartnerLink pLink) {
        RPartnerLink rPLink = (RPartnerLink) pLink.getStaticModel();
        Long pLinkID = new Long(rPLink.getUniqueId());

        // register variable copies by branch
        Long lBranchID = new Long(branchId);
        Long pLinkScopeId = ((ScopingElement) rPLink.getAssociatedScope()).getScopeId();

        Map<Long, Map<Long, StatePartnerLinkWrapper>> scopeMap = null;
        Map<Long, StatePartnerLinkWrapper> branchPLinks = null;
        if (pLink.isInserted()) {
            // If this partnerLink has been inserted already
            scopeMap = mBranchUpdatedPLinkMap.get(lBranchID);
            if (scopeMap == null) {
                scopeMap = new HashMap<Long, Map<Long, StatePartnerLinkWrapper>>();
                mBranchUpdatedPLinkMap.put(lBranchID, scopeMap);
                branchPLinks = new HashMap<Long, StatePartnerLinkWrapper>();
                scopeMap.put(pLinkScopeId, branchPLinks);
            } else {
                branchPLinks = scopeMap.get(pLinkScopeId);
                if (branchPLinks == null) {
                    branchPLinks = new HashMap<Long, StatePartnerLinkWrapper>();
                    scopeMap.put(pLinkScopeId, branchPLinks);
                }
            }
        } else {
            scopeMap = mBranchNewPLinkMap.get(lBranchID);
            if (scopeMap == null) {
                scopeMap = new HashMap<Long, Map<Long, StatePartnerLinkWrapper>>();
                mBranchNewPLinkMap.put(lBranchID, scopeMap);
                branchPLinks = new HashMap<Long, StatePartnerLinkWrapper>();
                scopeMap.put(pLinkScopeId, branchPLinks);
            } else {
                branchPLinks = scopeMap.get(pLinkScopeId);
                if (branchPLinks == null) {
                    branchPLinks = new HashMap<Long, StatePartnerLinkWrapper>();
                    scopeMap.put(pLinkScopeId, branchPLinks);
                }
            }
        }
        branchPLinks.put(pLinkID, new StatePartnerLinkWrapper(getId(), pLink));
    }

    /**
     * Creates a deep copy of this variable, such that changes to the copy
     * do not affect this instance.
     *
     * @return a deep copy of this variable.
     */
    /*private RuntimeVariable copy(RuntimeVariable var) {

         *      do not remove this method, it will be used later

        RuntimeVariable copy = new RuntimeVariableImpl(var.getVariableDef());
        if (var.getVariableDef().getMessageType() != null) { // same check as constructor
            if (var.getWSMessage() == null) {
                copy.setWSMessage(null, true);
            }
            else {
                copy.setWSMessage(var.getWSMessage().copy(), true);
            }
        } else {
            Object objVal = var.getXSDVariableData();
            if (objVal instanceof Node) {
                Element elem = (Element) objVal;
                Document document = constructDocument();
                Node tmpNode = document.importNode(elem, true);
                document.appendChild(tmpNode);
                copy.setXSDVariableData(document.getDocumentElement());
            } else {
                copy.setXSDVariableData(objVal);
            }
        }
        return copy;
    }*/
    private Document constructDocument() {
        XmlResourceProviderPool xmlResProviderpool = (XmlResourceProviderPool) BPELSERegistry.getInstance().lookup(
                XmlResourceProviderPool.class.getName());
        XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
        Document doc = xmlResourceProvider.getDocumentBuilder().newDocument();
        xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
        xmlResourceProvider = null;
        return doc;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getBPELId()
     */
    public QName getBPELId() {
        return mBPELId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getEngineId()
     */
    public String getEngineId() {
        return mEngId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getId()
     */
    public String getId() {
        return mId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getInvalidPCs()
     */
    public Long[] getInvalidPCs() {
        Set<Long> retVals = new HashSet<Long>();
        Long invalidPC = null;

        for (Long invalidBranch : mInvalidBranches) {
            invalidPC = mBranchIdPCMap.get(invalidBranch);

            if (invalidPC != null) {
                retVals.add(invalidPC);
            }
        }

//        if (mIsInFlow) {
//            // we are persisting an activity in a flow branch
//            // invalidate last persisted checkpoint in main branch
//            Long lastMainPC = mBranchIdPCMap.get(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
//            if (lastMainPC != null) retVals.add(lastMainPC);
//        }
        return (Long[]) retVals.toArray(new Long[] {});
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getPC()
     */
    public Long[] getPC() {
        return new Long[] {mOldPC, mNewPC};
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getTimerValue()
     */
    public Timestamp getTimerValue() {
        return mTimerObject;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getPickCompositeActId()
     */
    public Long getPickCompositeActId() {
        return mPickCompositeId;
    }

    public Long getBranchInvokeCounter() {
        return mBranchInvokeCounter;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updateCRMPState(java.lang.String, java.lang.String, java.lang.String, long, char[])
     */
    public void updateCRMPState(String partnerLink, String operation, String bpelMsgExchange, long replyVarId, char[] responseObj) {
        mCRMPState = new CRMPStateImpl(partnerLink, operation, bpelMsgExchange,
                replyVarId, responseObj);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState#updateCRMPState(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
     */
    public void updateCRMPState(String crmpInvokeId, String partnerLink, String operation, String bpelMesgExchange) {
        mCRMPState = new CRMPStateImpl(crmpInvokeId, partnerLink, operation,
                bpelMesgExchange);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getCRMPState()
     */
    public CRMPState getCRMPState() {
        return mCRMPState;
    }
    
}
