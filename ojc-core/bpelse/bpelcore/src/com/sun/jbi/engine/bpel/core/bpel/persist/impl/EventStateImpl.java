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
package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState;

/**
 * @author Sun Inc
 * Aug 30, 2007
 */
public class EventStateImpl extends StateImpl implements EventState, MutableEventState {

    MutableState mParentState;
    String mEventScopeGuid;
    long mAssocScopeBranchId;
    Set<String> mEnteredScopes = Collections.synchronizedSet(new HashSet<String>());
    String mAssociatedScopeId;
    long mEventModelId;
    Timestamp mOnAlarmTimerObject;
    Long mRepeatEveryVal;
    boolean mIsUpdated = false;
    
    /**
     * @param engId
     * @param id
     * @param assocScopeId 
     * @param eventModelId
     */
    public EventStateImpl(MutableState parentState, String engId, String id, long assocScopeBranchId, 
    		String assocScopeId, long eventModelId) {
    	
        super(engId, ((State) parentState).getBPELId(), id);
        init(parentState, engId, assocScopeBranchId, assocScopeId, eventModelId);
        mIsUpdated = false;
    }
    
    void init(MutableState parentState, String engId, long assocScopeBranchId,String assocScopeId, 
    		long eventModelId) {
        mParentState = parentState;
        mAssocScopeBranchId = assocScopeBranchId;
        mAssociatedScopeId = assocScopeId;
        mEventModelId = eventModelId;
        mUpdateStatus = State.Mode.INSERT;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#exitScope(long)
     */
    public void exitScope(String scopeGuid) {
        if (mEventScopeGuid == scopeGuid) {
            // TODO when the event handler is to be exited, any state changes that
            // were not persisted but that need to be persisted at a later check point
            // need to be transferred to the parentState.

            // mEventModelId is also the branchId of the event. pull out all the variables
            // registered with this branchId from the parentStates and assign them properly with 
            // the scope branchId.
            Collection<StateVariableWrapper> varsForUpdate = getVariablesRecursively(true, mEventModelId);
            Collection<StateVariableWrapper> varsForInsert = getVariablesRecursively(false, mEventModelId);

            for (StateVariableWrapper varForUpdate : varsForUpdate) {
                mParentState.updateVariable(mAssocScopeBranchId, varForUpdate.getVar());
            } 
            for (StateVariableWrapper varForInsert : varsForInsert) {
                mParentState.updateVariable(mAssocScopeBranchId, varForInsert.getVar());
            }
            // once the variables are merged onto the parent branchId then they will be cleaned
            // from the current eventBranchId association. 
            // We don't need to worry about synchronization
            // since concurrent modification of these variables is not valid and allowed. We 
            // are making a loose attempt for such kind of BPELs that tend to write but which 
            // may work because of the nature of the requirement and because concurrency is not
            // common for them. The support till this far itself is a bit far fetched.
            cleanUpVariablesRecursively(mEventModelId, false);
            
            // if this event scope as enclosed scope, update these to to the parent branch.
            Collection<FaultHandlingContext> ownScopesForUpdate = this.getScopeStates(true, mEventModelId);
            Collection<FaultHandlingContext> ownScopesForInsert = this.getScopeStates(false, mEventModelId);
            
            for (FaultHandlingContext scopeForUpdate: ownScopesForUpdate) {
            	mParentState.updateScope(mAssocScopeBranchId, scopeForUpdate);
            }
            
            for (FaultHandlingContext scopeForInsert: ownScopesForInsert) {
            	mParentState.updateScope(mAssocScopeBranchId, scopeForInsert);
            }
            
        } else {
            mEnteredScopes.remove(scopeGuid);
            super.exitScope(scopeGuid);
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#updateScope(long, com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
     */
    @Override
    public void updateScope(long branchId, FaultHandlingContext scope) {
        // This method is over ridden just to make a point that it was thought over and 
        // that the parent class implementation is good enough.
        super.updateScope(branchId, scope);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#updateVariable(long, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
     */
    public void updateVariable(long branchId, RuntimeVariable variable) {
        String varScopeId = variable.getScopeGuid();
        if (doIOwnScope(varScopeId)) {
            super.updateVariable(branchId, variable);
        } else {
            mParentState.updateVariable(branchId, variable);
        }
    }    

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#getVariables(boolean)
     */
    @Override
    public Collection<StateVariableWrapper> getVariables(boolean for_update) {
        Collection<StateVariableWrapper> retVal = super.getVariables(for_update);
        Collection<StateVariableWrapper> parentStateVal = ((StateImpl) mParentState).getVariables(for_update, mBranchId);
        retVal.addAll(parentStateVal);
        return retVal;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#shouldPersistBeforeConcurrentExecution(long)
     */
    @Override
    public boolean shouldPersistBeforeConcurrentExecution(long branchId) {
        return (super.shouldPersistBeforeConcurrentExecution(branchId)
                || mParentState.shouldPersistBeforeConcurrentExecution(branchId));
    }

    private boolean doIOwnScope(String scopeGuid) {
        return mEnteredScopes.contains(scopeGuid);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getAncestorScopeId()
     */
    public String getAncestorScopeId() {
        return mAssociatedScopeId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getEventModelId()
     */
    public long getEventModelId() {
        return mEventModelId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getRepeatEveryVal()
     */
    public Long getRepeatEveryVal() {
        return mRepeatEveryVal;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#isUpdatedInDB()
     */
    public boolean isUpdatedInDB() {
        return mIsUpdated;
    }

    public void setIsUpdatedInDB(boolean flag) {
        // This is to be called only from the recovery code. And has to be called last, in order to counter the
        // effect of calling finishedPersistence.
        mIsUpdated = flag;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getBPStateId()
     */
    public String getBPStateId() {
        if (mParentState instanceof EventState) {
            return ((EventState) mParentState).getBPStateId();
        } else {
            return ((State) mParentState).getId();
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl#finishedPersisting()
     */
    @Override
    public void clearPC() {
        if (mUpdateStatus == Mode.UPDATE && !mIsUpdated) {
            mIsUpdated = true;
        }
        cleanUpVariablesRecursively(mBranchId, true);
        super.clearPC();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState#updateTimeVal(long, Long)
     */
    public void updateTimeVal(long nanoSeconds, Long repeatEvery) {
        mOnAlarmTimerObject = new Timestamp(nanoSeconds);
        mRepeatEveryVal = repeatEvery;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState#getOnEventTimerValue()
     */
    public Timestamp getOnEventTimerValue() {
        return mOnAlarmTimerObject;
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.persist.MutableState.MutableEventState#setAssociatedScopeGuid(java.lang.String)
	 */
	public void setAssociatedScopeGuid(String scopeguid) {
        mEnteredScopes.add(scopeguid);
		mEventScopeGuid = scopeguid;
	}
    
    protected void cleanUpVariablesRecursively(long branchId, boolean finishedPersistence) {
        ((StateImpl) mParentState).cleanUpVariables(branchId, finishedPersistence);
        if (mParentState instanceof EventStateImpl) {
            ((EventStateImpl) mParentState).cleanUpVariablesRecursively(branchId, finishedPersistence);
        }
    }
    
	protected Collection<StateVariableWrapper> getVariablesRecursively(boolean for_update, long branchId) {
	    Collection<StateVariableWrapper> retVal = getVariables(true, branchId);
        Collection<StateVariableWrapper> parentStateVal;
        if (mParentState instanceof EventStateImpl) {
            parentStateVal = ((EventStateImpl) mParentState).getVariablesRecursively(for_update, branchId);
        } else {
            parentStateVal = ((StateImpl) mParentState).getVariables(for_update, branchId);
        }
	    retVal.addAll(parentStateVal);
	    return retVal;
	}
}
