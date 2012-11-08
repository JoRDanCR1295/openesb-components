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
 * @(#)MutableState.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.util.Collection;
import java.util.List;

import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;


/**
 * MutableState represents the state of a BP Instance that is alive and incomplete.
 * Once a state is marked delete, no thread should have a reference to the object.
 *
 * branchId for the BPELProcess level is -1
 * ScopeId for the BPELProcess level is -2
 *
 * @author Sun Microsystems
 */
public interface MutableState { 
    
    public interface MutableEventState extends MutableState {
        /**
         * @param nanoSecs
         * @param repeatEvery (a null is expected) @see {@link com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getRepeatEveryVal()}
         */
        void updateTimeVal(long nanoSecs, Long repeatEvery);
        
        void setAssociatedScopeGuid(String scopeguid);
    }
    
    String getId();
    
    /**
     * Updates process execution state with recovered state.
     * @param recoveredState The recovered state of a process instance.
     *@param branchId, the branchId of the ForEach activity.
     */
    void recover(RecoveredState recoveredState, long branchId);
    
    /** Initializes the Data Structures required to associate state information with the
     * current scope. If the scope is BPELProcess, marks the MutableState as insert.
     * Scope Id for the processlevel is -2.
     * @param scopeGuid scope ID
     */
    void enterScope(String scopeGuid);

    /** Deletes all variables associated with the scope that was exited.
     * If the Scope is the BPELProcess, marks the State as delete. Scope Id for
     * the processlevel is -2
     * @param scopeGuid scope ID
     */
    void exitScope(String scopeGuid);

    /**
     * Updates a ForEach activity.
     * @param pcId The id of the activity.
     * @param branchId the branch id of the ForEach activity. 
     * @param counter The current iteration.
     * @param successes The number of successful (non-faulted) iterations.
     */
    void updateForEach(long pcId, long branchid, int counter, int successes);
    
    /**
     * @param pcId ForEach Id.
     * @param branchId the branch id of the ForEach activity.
     * @param startCount
     * @param finalCount
     * @param completionCount
     */
    void addForEach(long pcId, long branchid, int startCount, int finalCount, int completionCount);
    
    /**
     * Indicates a serial ForEach has started.
     * @param pcId The activity id of the ForEach.
     * @param branchId the branch id of the ForEach activity.
     */
    void startForEach(long pcId, long branchid);
    
    /**
     * Indicates a serial ForEach has ended.
     * @param pcId The activity id of the ForEach.
     * @param branchId the branch id of the ForEach activity.
     */
    void endForEach(long pcId, long branchid);
    
    /**
     * branchId for the BPELProcess level is '-1'
     * @param branchId branch in which the execution was at
     * @param activityId activity at which the execution persisted.
     */
    void updatePC(long branchId, long activityId);

    /**
     * for time based activities.
     * @param branchId branch in which the execution was at
     * @param activityId activity at which the execution persisted.
     * @param nanoSeconds duration to be persisted
     * @param branchInvokeCounter the branchInvokeCounter value of the CallFrame at
     * this instance of persistence.
     */
    void updatePC(long branchId, long activityId, long nanoSeconds, long branchInvokeCounter);

    /**
     * called by pick-OnAlarm.
     *
     * @param branchId branch in which the execution was at
     * @param activityId activity at which the execution persisted.
     * @param nanoSeconds duration to be persisted
     * @param compositeId OnAlarm Id.
     * @param branchInvokeCounter the branchInvokeCounter value of the CallFrame at
     * this instance of persistence.
     */
    void updatePCWithPickCompositeId(long branchId, long activityId,
        long nanoSeconds, long compositeId, long branchInvokeCounter);

    /**
     * for Pick OnAlarm and OnMessage after the path has been selected.
     * @param branchId branch in which the execution was at
     * @param activityId activity at which the execution persisted.
     * @param compositeId OnMessage or OnAlarm id, whose path pick executes
     * @param branchInvokeCounter the branchInvokeCounter value of the CallFrame at
     * this instance of persistence.
     */
    void updatePCWithPickCompositeId(long branchId, long activityId,
        long compositeId, long branchInvokeCounter);

    /**
     * branchId for the BPELProcess level is '-1'
     * @param branchId branch in which the execution was at
     * @param activityId activity at which the execution persisted.
     * @param branchInvokeCounter invokeId on that branch, used by CRMP. 
     *        update the branch invoke counter for this branch used for the 
     *        custom reliable messaging InOut message exchange (two-way) invokes
     */
    void updatePCWithBranchInvokeCounter(long branchId, long activityId, long branchInvokeCounter);
    
    /**
     * update variable in database
     * @param branchId The branch id where variable is updated.
     * @param variable runtime variable object
     */
    void updateVariable(long branchId, RuntimeVariable variable);

    /**
     * Update scope-defined variables.
     * @param variable The current value of the runtime variable.
     */
    //public void updateScopedVariable(RuntimeVariable variable);

    /**
     * add correlation set value
     * @param corrVal CorrelationVal
     */
    void addCorrelation(CorrelationVal corrVal);
    
    /**
     * add a set of correlation values
     * @param corrVal CorrelationVal
     */
    void addCorrelation(Collection corrValSet);

    void removeCorrelations();
    
    /**
     * marks the entrance of a flow.
     * @param flowId flow activity ID that uniquely identifes the flow in the BPEL
     * @param branchIds all the branches that are associated with the flow. List<Long>
     */
    void enterFlow(long flowId, List branchIds);

    /**
     * marks the exit of a flow
     * @param flowID flow activity ID that uniquely identifes the flow in the BPEL
     * @param parentBranchId TODO
     */
    void exitFlow(long flowID, long parentBranchId); // delete or mark for delete of PCIds from the table

    /** This API is used by concurrent execution units (Flow or event handler), to see if 
     * there are any(variable) changes to the bpel instance (or event handler). If there 
     * are no changes, the concurrent execution units would consider that it doesn't
     * need to persist and continue with execution without any persistence. 
     * If there are variable, scope changes, ForEach State (or in future partner link or message exchange changes)
     * then engine needs to update the last recoverable point before engine starts executing the concurrent 
     * execution unit.
     * 
     * @param branchId
     * @return
     */
    boolean shouldPersistBeforeConcurrentExecution(long branchId);
    
    /**
     * This is called to persist the reply activity variable id and the repsonse 
     * runtime variable to the CRMPState table as part of updating the completion of the 
     * reply activity
     * @param partnerLink
     * @param operation
     * @param bpelMsgExchange
     * @param replyVarId 
     * @param responseObj char array of the string representation of the RuntimeVariable
     * @param state
     * @param bpId
     * @return
     */
    void updateCRMPState(String partnerLink, String operation, String bpelMsgExchange, 
            long replyVarId, char[] responseObj);


    /**
     * This API is used when customReliablemessaging for the InOut message exchange is used. 
     * This will be called such that both the state and the reliableMessage(crmpInvokeId) 
     * are saved to the persistent store. calling the following two APIs together 
     * persistState(MutableState) and persistReliableState(), may result in the same
     * behavior as this API, but it differs because this API guarantees all in one transaction.
     *
     * @param state MutableState
     * @param crmpInvokeId reliable messaging ID 
     * @param bpId the process instance id
     * @param partnerLink the partnerLink name attribute of the receiveing activity
     * @param operation the operation name attribute of the receiving activity 
     * @param bpelMesgExchange the bpel messageexchange attribute of the receiving activity 
     *
     * @return boolean: if it's executed successful, returns true; otherwise, return false
     */
    void updateCRMPState(String crmpInvokeId, String partnerLink, 
                String operation, String bpelMesgExchange);
    
    /** update partnerlink in DB
     * @param branchId
     * @param pLink
     */
    void updatePartnerLink(long branchId, RuntimePartnerLink pLink);
    
    /**
     * 
     * @param branchId
     * @param scope
     */
    void updateScope(long branchId, FaultHandlingContext scope);
    
}
