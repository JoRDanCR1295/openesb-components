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
 * @(#)Context.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;


import java.util.Stack;

import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;

/**
 * This interface provides support for notifying and handling faults. It is 
 * implemented by the ScopeUnitImpl and BPELProcessInstanceImpl. 
 * Also provides support for compensation of enclosed completed scopes, in
 * the event of a compensation handler being invoked by either a <compensate>
 * or <compensateScope> activity.
 *
 * @author pbhagat
 */
public interface FaultHandlingContext {

    String SCOPE_ID_ITER_SEPARATOR = ","; //$NON-NLS-1$
	String PROCESS_INSTANCE_SCOPE_ID = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.toString(); //$NON-NLS-1$
    
    // The different states that the scope is in.
    String COMPLETED = "C"; //$NON-NLS-1$
    String RUNNING = "R"; //$NON-NLS-1$
    String EXE_FAULT_HANDLER = "EFH"; //$NON-NLS-1$
    String EXE_TERMINATION_HANDLER = "ETH"; //$NON-NLS-1$
    String EXE_COMPENSATION_HANDLER = "ECH"; //$NON-NLS-1$
    String FAULTED = "F"; //$NON-NLS-1$
    String FAULTED_IN_FH = "FFH"; //$NON-NLS-1$
    String FAULTED_IN_TH = "FTH"; //$NON-NLS-1$
    String FAULTED_IN_CH = "FCH"; //$NON-NLS-1$
    String DONE = "D"; //$NON-NLS-1$
    //TODO: have to see if the terminated state is needed.
    String TERMINATED = "T"; //$NON-NLS-1$
    
	/**
	 * Used by a Unit which is a source of fault, to notify the enclosing 
	 * scope that a fault has occurred. This method will change the state of
	 * the FaultHandlingContext to Faulted. It also calls activateWaitingCallFrames().
	 *  
	 * @param fault The fault that occurred
	 * @param frame Associated CallFrame
	 * @param bpit Associated BusinessProcessInstanceThread 
	 * @param rObjs An instance of RequiredObjects
	 */
	void notifyFault(Fault fault, ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs);

	/**
	 * The method will return true if the scope has faulted or its parent scope 
	 * has faulted. If the scope is executing any of its FCT handlers the method 
	 * will return false. However, if there is a fault in the FCT handler itself, 
	 * then this method will return true.
	 * 
	 * This method is called by an Unit in its doAction() to find out if it is being 
	 * terminated. A structured unit will also do this check in its doResumeAction(). 
	 * If it returns true, the activity will terminate itself. It will either return 
	 * true or call doPassControlToParent() depending upon the the path of its 
	 * execution. Typically those situations where the doAction() of the Unit has 
	 * been called by its parent unit, the Unit will simply return false. Some examples 
	 * are assign, sequence, invoke (when the doAction is called the first time).
	 * doPassControlToParent() is called in cases where the execution was 'broken' 
	 * because the Unit had to wait for a event or timer, etc. For example,
	 * invoke (when it enters doAction() to handler the response that comes back),
	 * VirtualThrowUnit (when it enters doAction() the second time), doRsumeAction()
	 * of structured activity, etc. 
	 *   
	 * @return
	 */
	boolean isBeingTerminated();

	/**
	 * Used by a FaultHandlingContext to register itself with its parent 
	 * FaultHandlingContext. See activateWaitingCallFrames() on how these
	 * child FaultHandlingContext are used. 
	 * @param enclosedScope
	 */
	void registerEnclosedScope(FaultHandlingContext enclosedScope);

	/**
	 * When a callframe is created it will be registered with the FaultHandlingContext.
	 * This is used by the FaultHandlingContext to remove any callframe that is waiting 
	 * for an event or a timer and put it in the ReadyToRun queue. The FaultHandlingContext
	 * does this when a Unit causing fault calls notifyFault. See activateWaitingCallFrames()
	 * for more details.
	 * @param callframe
	 */
	void registerCallFrame(ICallFrame callframe);


	/**
	 * A FaultHandlingContext uses this API to activate all the waiting callframes that
	 * have been registered with it using registerCallFrame(). It then call this method
	 * on its child FaultHandlingContexts which were earlier registered with it using
	 * registerEnclosedScope(). Note that when a Unit calls notifyFault() on the 
	 * FaultHandlingContext, the FaultHandlingContext will call this method among other
	 * things.
	 */
	void activateWaitingCallFrames();

	/**
	 * Returns the original fault that occurred
	 * @return
	 */
    Fault getFault();
	
	/**
	 * When a contained scope of an instance completes successfully(without faults) 
	 * it would be added to that instance's completed scopes stack in the order of 
	 * completion  
	 * 
	 * @param completedScope: the contained scope instance which has completed
	 * successfully.
	 */
	int pushCompletedScope(Context completedScope);
	
    /**
     * Returns the Stack of all the completed contained instance scopes of this
     * Scope. The stack has been populated in the order of completion of the contained
     * instances.
     * @return the Stack of the completed contained instances of this scope.
     */
	Stack<Context> getCompletedScopes();
	
	/**
	 * Returns the Scope activity that has the same name as the target attribute of 
	 * the <compensateScope> activity.
	 * @param targetScopeName: the target Scope name.
	 * @return Context: the Context object of the target scope
	 */
	Context getTargetScopeForCompensation(String targetScopeName);
	
	/**
	 * Returns the last completed Scope activity of this scope context.
	 * The Scope activities are returned in the strict reverse order of 
	 * their completion. Used for compensation of Scope's induced 
	 * by the <compensate> activity. 
	 * @return the Context object for a Scope
	 */
	Context getScopeForCompensation();
	
	/**
	 * Once a completed scope instance has been invoked for compensation by a 
	 * <compensate> or <compensateScope> and if the compensation handler invoked
	 * has a Messaging activity(receive, invoke, etc) then the resumtion logic of
	 * those activities after the Message exchange scenarios will have to return 
	 * to the calling/invoking <compensate> or <compensateScope> activity. 
	 * This ActivityUnit would be the return value of the getEnclosingUnit() method
	 * call on a scope in such a state.  
	 * @param unit: the calling/invoking <compensate> or <compensateScope> activity.
	 */
	void setCompensateContinuation(ActivityUnit unit);

	String getStateId();
	
	long getScopeId();
	
    /**
     * return the string value indicating the unique runtime scope id associated
     * with this scope context instance.
     * Ex: '100001,1' : its a combination of the scope static activity id and the 
     * iteration count.
     * @return String 
     */
	String getScopeGuid();

	/**
	 * The unique runtime scope id associated with the parent context of this scope
	 * context instance.
	 * For the process instance this would return null. 
	 * @return
	 */
	String getParentScopeGuid();
    
	/**
	 * return a string value indicating the current state of the scope context instance.
	 * Could have any of the String values indicating the state defined in this interface.
	 * @return
	 */
	String getScopeState();
    
    /**
     * Called from the two-way invoke when the response received is a fault. The fault value and the 
     * faultActivityId has to be set on the scope so that it is available for persistence when the 
     * next persistence point is hit.
     * @param fault
     * @param faultActivityId
     */
    void setFault(Fault fault, long faultActivityId);

    /**
     * return the activity id that threw a fault in this scope context. 
     * @return long, activityId 
     */
    long getFaultActivityId();

    /**
     * Returns the activity Id of the non-virtual <compensate> or the <compensateScope>
     * acivity that invoked this scope context for compensation handling. If the callee 
     * was a virtual <compensate> the retrun value would be the default 0.
     * @return long, activityId
     */
    long getCompensateId();
    
    /**
     * The int value indicating the order in which this scope completed and was added
     * to the parent scope context. 
     * @return int the completion order
     */
    long getCompletionOrder();

    /**
     * Returns the next iteration count for the passed in static scope activity id. 
     * @param scopeId, the static scope activity id.
     * @return
     */
    long getEnclosedScopesIterCount(long scopeId);
    
    /**
     * @param completedScope
     */
    void addCompletedScope(FaultHandlingContext completedScope);
    
    /**
     * This is called from recovery, where every scope being recovered
     * will have its completion order set. This method also encapsulates
     * the work of adding this recovered scope to its parent scope if its 
     * scopeState is 'Completed'. All other states of the scope does not
     * warrent the addition of the scope to the parent again.
     * Ex: if the recovered scope state is 'Running' then the scope state
     * will eventualy as part of its recovery and execution, get added to 
     * its parent when it completes its execution. 
     * @param completionOrder
     */
    void setCompletionOrder(long completionOrder);
    
    /**
     * @param scopeState
     */
    void setScopeState(String scopeState);
    
    /**
     * @param compensateId
     */
    void setCompensateId(long compensateId);
    
    /**
     * boolean value indicating if this scope has been inserted into the persistence
     * store.
     * @return
     */
    boolean isInserted();

    /**
     * marks that this scope context has been inserted into the persistence store.
     */
    void markInserted();
    
    /**
     * This method will return if the state of this FaultHandling context is in a FAULTED state.
     * The following states will constitute a TRUE condition
     * 1. BPELProcessInstanceImpl.java
     *    <code>
     *      currentProcessState == ProcessState.Faulted 
     *       || currentProcessState == ProcessState.FaultedInFH
     *    </code>
     *
     * 2. ScopeUnitImpl.java
     *    <code>
     *      currentScopeState == ScopeState.Faulted 
     *       || currentScopeState == ScopeState.FaultedInFH
     *       || currentScopeState == ScopeState.FaultedInTH
     *       || currentScopeState == ScopeState.FaultedInCH
     *    </code>
     *           
     * @return
     */
    boolean hasFaulted();
}