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
 * @(#)ScopeUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.CompensationHandlerHolder;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.FaultHandlerScope;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Scope activity unit implementation
 *
 * @author Sun Microsystems
 */
public class ScopeUnitImpl extends StructuredActivityUnitImpl implements Context, ScopeOrProcessUnit, 
		FaultHandlingContext {
	
    private static final Logger LOGGER = Logger.getLogger(ScopeUnitImpl.class.getName());
	
    private Map mRuntimeVariables = Collections.synchronizedMap(new HashMap());
    
    private Map<PartnerLink, RuntimePartnerLink> mRuntimePartnerLinks = 
        Collections.synchronizedMap(new HashMap<PartnerLink, RuntimePartnerLink>());
    
    private RuntimeEventHandlers mEventHandlersUnit;
    private ICallFrame mCallFrame;
    private IMAScopeBridge mIMAScope;
    private BPELProcessManager mBPELProcMgr;
    
    private enum ScopeState {Running, WaitingForEventsToComplete, Faulted, ExecutingFaultHandlers, 
    	Terminated, ExecutingTerminationHandler, ExecutingCompensationHandler, 
    	FaultedInFH, FaultedInTH, FaultedInCH, Completed, Done}
    private ScopeState currentScopeState = ScopeState.Running;
    
    private Set <ICallFrame> ownedCallFrames = Collections.synchronizedSet(new HashSet <ICallFrame>());
    private Set <FaultHandlingContext> enclosedScopes = Collections.synchronizedSet(new HashSet <FaultHandlingContext>());
    private Fault mFaultObj;
    /* The Stack of the completed contained scopes of this instance in the order of completion. */
    private Stack<Context> mCompletedScopes;
    /*The parent <compensate> or <compensateScope> activity that invoked this completed instance. */
    private ActivityUnit mCompensateContinuation = null;
    
    private Object mCompensationLockObj = new Object();
    private boolean mAcquired = false;
    private boolean mIsRecoveredUnit = false;
    
    protected Map<Long, Long>mEnclosedScopesIterationCount = 
    	Collections.synchronizedMap(new HashMap<Long, Long>()); 
    
    private String mScopeGuid;
    private boolean mIsInserted;
    private long mCompletionOrder;
    private long mCompensateId;
    private long mFaultActivityId;
    
    /**
     * Creates a new ScopeUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public ScopeUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
    	this(context, parentActUnit, act, branchId, null);
    }
    
    /**
     * Creates a new ScopeUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public ScopeUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId, String scopeGuid) {
        super(context, parentActUnit, act, branchId);
        mBPELProcMgr = context.getProcessInstance().getBPELProcessManager();
        mContext.getFaultHandlingContext().registerEnclosedScope(this);
        mIMAScope = new IMAScopeBridge (context.getProcessInstance(), mContext);
        context.getProcessInstance().addIMAScope(mIMAScope);
        
        Variables variables = ((Scope) mAct).getVariables();
        //TODO: Do we need iteration count?
        //long uniqueId = mAct.getUniqueId();
        //long iteration = mContext.getFaultHandlingContext().getEnclosedScopesIterCount(uniqueId);
        if(scopeGuid == null) {
            mScopeGuid = BPELHelper.getGUID();
        } else {
        	mScopeGuid = scopeGuid;
        }
        //The Scope stores the variables locally. If there are additional 
        //variables that need to be locally scoped, they can be added using
        //VariableScope.addRuntimeVariable()
        if (variables != null) {
            Iterator iterator = variables.getVariables().iterator();
            while(iterator.hasNext()) {
                RVariable variable = (RVariable)iterator.next();
                mRuntimeVariables.put(variable, null);
            }
        }
        mCompletedScopes = new Stack<Context>();
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.StructuredActivityUnitImpl#doAction(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        
    	if(mContext.getProcessInstance().isExiting()){
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
    		if(currentScopeState == ScopeState.Running 
    				|| currentScopeState == ScopeState.Done
    				|| currentScopeState == ScopeState.Completed){
    			BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
    			return true;
    		} else {
        		return doPassControlToParent(frame, bpit, rObjs);
    		}
    	}
    	
    	if (currentScopeState == ScopeState.Running) {
    		ActivityUnit childActUnit = null;
    		EventHandlers rEvhandlers = null;
    		// Start initialization of the scope.
    		try {
    			mCallFrame = frame;
    			ownedCallFrames.add(frame);
    			frame.setProgramCounter(this);
    			frame.onLineChange(this);
    			//Update the Scope on the StateImpl. Don't update if its a recovered unit
    			//or if this is an associated scope of an onEvent. In the latter case, the
    			//scope was already updated by the EventHandlersOnEventUnitImpl.
    			if (rObjs.getBPELProcessManager().isPersistenceEnabled() && !mIsRecoveredUnit
    					&& !(mAct.getParent() instanceof EventHandlersOnEvent)) {
    				updateScopeOnState(rObjs);
    			}

    			BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
    			frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	

    			Scope scope = (Scope) getStaticModelActivity();
    			rEvhandlers = scope.getEventHandlers();
    			if (rEvhandlers != null) {
    				if (!mIsRecoveredUnit) {
    					mEventHandlersUnit = new RuntimeEventHandlersImpl (this, frame.getProcessInstance(), 
    							this, this, rEvhandlers, getBranchId(), rObjs.getEngine(), rObjs.getInterp(), 
    							rObjs.getBPELProcessManager(), getScopeGuid());
    				}                
    				/* Ensure that the start activity of the process instance has completed before
    				 * scheduling the event handlers. Otherwise, add the EH to the process instance
    				 * so that it can schedule the EH once the start activity completes
    				 */
    				boolean startActdone = frame.getProcessInstance().addTobeInstantiatedEventHandlers(this);
    				if (startActdone) {
    					if (!mIsRecoveredUnit) { 
    						frame.getProcessInstance().getPersistenctMgr().updateState(this, 
    								mContext.getStateContext().getState(), false);
    						frame.getProcessInstance().getPersistenctMgr().persistState(this, 
    								mContext.getStateContext().getState());
    						mEventHandlersUnit.startEventHandlers();
    					}
    				}
    			}

    			childActUnit = ActivityUnitFactory.getInstance().createActivityUnit(
    					this, this, getChildActivity(), getBranchId());
    		} catch (Exception e) {
    			StandardException scopeIniExp = 
    				new StandardException(StandardException.Fault.ScopeInitializationFailure, e);
    			throw scopeIniExp;
    		}
            
    		// Scope initialization complete. Now execute the child activities.
            boolean childActCompleted = executeChildActivities(frame, bpit, rObjs, childActUnit);
            
            /* Note that when the control comes back here the scope activty would
             * either be completed or was terminated. Note that if the child activity 
             * would have caused a fault, the child activity would return control using 
             * doResumeAction()
             */
            
            //Child activity was completed successfully
            if (childActCompleted) {
                frame.setProgramCounter(this);
                
                if(mContext.getProcessInstance().isExiting()){
                    if (rEvhandlers != null) {
                    	currentScopeState = ScopeState.WaitingForEventsToComplete;
                        mEventHandlersUnit.associatedScopeCompletes();
                        return false;
                    } else {
                		doScopeCompletionTasks(frame, bpit, rObjs, false);
                		BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
                        return true;
                    }
        		} else if(mContext.getFaultHandlingContext().isBeingTerminated()) {
        			return doTerminationHandlingIfPossible(frame, bpit, rObjs);
                } else {
                    /* The main activity in the scope has completed, so if EH are present the scope 
                     * needs to wait for the EH to complete, before it returns control to its parent. 
                     * Change the scope state to indicate that it is waiting for EH to complete
                     * and inform EH that the scope has completed
                     */
                    if (rEvhandlers != null) {
                    	currentScopeState = ScopeState.WaitingForEventsToComplete;
                        mEventHandlersUnit.associatedScopeCompletes();
                        return false;
                        
                    } else {
                        //If no EH is present then the scope can complete
                        currentScopeState = ScopeState.Completed;
                        // clear all the FH related data as the scope has completed.
                        clearFHRelatedData();
                        unregisterFromParent(this);
                        // push this completed scope instance to the parent sope stack of completed scopes.
                        mCompletionOrder = getParentContext().getFaultHandlingContext().pushCompletedScope(this);
                        updateScopeOnState(rObjs);
                        doScopeCompletionTasks(frame, bpit, rObjs, true);
                        BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
                        return true;
                    }
                }
            } else {
            	//Either the child activity did not completed or caused a fault. Note the
            	//fault handlers will be executed when the control comes to doResumeAction()
            	//from the AbstractThrowUnitImpl
                return false;            	
            }
    	} else if (currentScopeState == ScopeState.WaitingForEventsToComplete) {
    		//The EH has completed and the scope can now complete
            currentScopeState = ScopeState.Completed;    		
            // clear all the FH related data as the scope has completed.
            clearFHRelatedData();
            unregisterFromParent(this);
            // push this completed scope instance to the parent scope stack of completed scopes.
            mCompletionOrder = getParentContext().getFaultHandlingContext().pushCompletedScope(this);
            updateScopeOnState(rObjs);
    		doScopeCompletionTasks(frame, bpit, rObjs, true);
			return super.doPassControlToParent(frame, bpit, rObjs);
			
    	} else if (currentScopeState == ScopeState.Terminated) {
    		//The scope was waiting for the EH to wind up after being Terminated. Now
    		//it can do the TH.
    		return this.doTerminationHandlingIfPossible(frame, bpit, rObjs);
    		
    	} else if  (currentScopeState == ScopeState.Faulted) {
        	return doFaultHandlingIfPossible(frame, bpit, rObjs, true);
        	
    	} else if  (currentScopeState == ScopeState.Completed ) {
    		return doCompensationHandling(frame, bpit, rObjs);
    		
    	} else if (currentScopeState == ScopeState.Done) {
    		// could come to this state if two concurrent <compensate> activities try
    		// to compensate this scope.
    		return true;
    		
    	} else {
    		LOGGER.warning(I18n.loc("BPCOR-6060: Illegal state - method {0} called when the " + 
    				"value of currentScopeState is {1}", "doAction", currentScopeState));
    		return false;
    	}
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.StructuredActivityUnitImpl#doResumeAction(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit,
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     * com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
    		BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        // This mCallFrame setting is for recovery. Perhaps with notifyFault implementation for recovery
        // this can be changed.
        mCallFrame = frame;
        frame.setProgramCounter(this);
        
		if(mContext.getProcessInstance().isExiting()){
		    // do not enter this condition if the scope state is executing fault handler or termination handler
		    // as in such cases it is guaranteed that the event handlers would have been cleared by the fault
		    // or termination handling logic. bug: https://open-esb.dev.java.net/issues/show_bug.cgi?id=2339 
            if (((Scope) getStaticModelActivity()).getEventHandlers() != null
                    && !(currentScopeState.equals(ScopeState.ExecutingFaultHandlers)
                            || currentScopeState.equals(ScopeState.ExecutingTerminationHandler))) {
            	currentScopeState = ScopeState.WaitingForEventsToComplete;        		
        		mEventHandlersUnit.associatedScopeCompletes();
        		return false;
        	} else {
        		doScopeCompletionTasks(frame, bpit, rObjs, false);
                return doPassControlToParent(frame, bpit, rObjs);
        	}
		}
        
    	//Either the scope's  child activity has completed successfully or
    	//it was terminated 
    	if(currentScopeState == ScopeState.Running) {
    		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
    			return doTerminationHandlingIfPossible(frame, bpit, rObjs);
    		} else {
                /* The main activity in the scope has completed, so if EH are present the scope 
                 * needs to wait for the EH to complete, before it returns control to its parent. 
                 * Change the scope state to indicate that it is waiting for EH to complete
                 * and inform EH that the scope has completed
                 */
                
                if (((Scope) getStaticModelActivity()).getEventHandlers() != null) {
                	currentScopeState = ScopeState.WaitingForEventsToComplete;
                    mEventHandlersUnit.associatedScopeCompletes();
                    return false;
                    
                } else {
                    //If no EH is present then the scope can complete
                    currentScopeState = ScopeState.Completed;
                    // clear all the FH related data as the scope has completed.
                    clearFHRelatedData();
                    unregisterFromParent(this);
                    // push this completed scope instance to the parent scope stack of completed scopes.
                    mCompletionOrder = getParentContext().getFaultHandlingContext().pushCompletedScope(this);
                    updateScopeOnState(rObjs);
                    doScopeCompletionTasks(frame, bpit, rObjs, true);
                    return doPassControlToParent(frame, bpit, rObjs);
                }
    		}
    	} else if(currentScopeState == ScopeState.Faulted) {
        	return doFaultHandlingIfPossible(frame, bpit, rObjs, false);
    		
    	} else if(currentScopeState == ScopeState.ExecutingTerminationHandler) {
    		/* The TH has completed successfully. Since this scope was terminated, this MUST 
    		 * be a Flow branch, so, after the TH has completed we ask the parent to do its 
    		 * action. Since this is a flow branch it will ultimately reach the doResumeAction 
    		 * of Flow which will return true. This will cause this branch to join the flow 
    		 * (in BPELInterpreter.execute() method)
    		 */
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
    		return doPassControlToParent(frame, bpit, rObjs);    		

    	} else if(currentScopeState == ScopeState.ExecutingFaultHandlers) {
    		//FH has completed successfully
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
            return doPassControlToParent(frame, bpit, rObjs);
            
    	} else if(currentScopeState == ScopeState.FaultedInFH
    				|| currentScopeState == ScopeState.FaultedInCH) {
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
    		//TODO: Do we need to break the method call here and call doAction like
    		//the AbstractThrowUnit?
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
    		mContext.getFaultHandlingContext().notifyFault(mFaultObj, frame, bpit, rObjs);
            return doPassControlToParent(frame, bpit, rObjs);
            
    	} else if (currentScopeState == ScopeState.FaultedInTH){
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
    		return doPassControlToParent(frame, bpit, rObjs);
            
    	} else if(currentScopeState == ScopeState.ExecutingCompensationHandler) {
    		// Execution reached here as there was a MessagingActivity in the Compensation Handler
    		// Its here as the Compensation Handler completed successfully (without Faults) as hence
    		// pass the control on to the parent and set state to compensated

    		//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
    		doScopeCompletionTasks(frame, bpit, rObjs, false);
    		return doPassControlToParent(frame, bpit, rObjs);
    		
    	} else {
    		LOGGER.warning(I18n.loc("BPCOR-6060: Illegal state - method {0} called when the " + 
    				"value of currentScopeState is {1}", "doResumeAction()", currentScopeState.toString()));    		
    		return false;    		
    	}
    }
    
    /**
     * Returns whether it is faulted.
     * @return
     */
    public boolean hasCompletedNormally() {
    	if (currentScopeState == ScopeState.Completed) {
    		return true;
    	} else {
    		return false;
    	}
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        return ((RActivityHolder) mAct).getChildActivity();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit#instantiateEventHandlers()
     */
    public void instantiateEventHandlers() {
        mEventHandlersUnit.startEventHandlers();
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit#eventHandlersComplete()
     */
    public void eventHandlersComplete() {
        mCallFrame.setProgramCounter(this);
        mBPELProcMgr.addToReadyToRunQueue(mCallFrame);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.meta.RVariable)
     */
    public RuntimeVariable getRuntimeVariable(RVariable variable) {
        
        if (mRuntimeVariables.containsKey(variable)) {
            return (RuntimeVariable) mRuntimeVariables.get(variable);
        } else {
            return mContext.getRuntimeVariable(variable);
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
     */
    public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
        
        if (mRuntimeVariables.containsKey(runtimeVariable.getVariableDef())) {
            mRuntimeVariables.put(runtimeVariable.getVariableDef(), runtimeVariable);
        } else {
            mContext.setRuntimeVariable(runtimeVariable);
        }
    }

    /**
     * Adds a new variable to the in-memory model and also update the underlying
     * state object. This is called from the ForEachUnitSerialImpl where the 
     * forEach counter variable is added to the child scope of the forEach 
     * construct. 
     * @param runtimeVariable
     */
    public void addVariable(RuntimeVariable runtimeVariable) {
        mRuntimeVariables.put(runtimeVariable.getVariableDef(), runtimeVariable);
        // update the variable on the state.
        getProcessInstance().getPersistenctMgr().updateState(getStateContext(), 
        		runtimeVariable, getBranchId());
    }
       
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
     */
    public Map getRuntimeVariables() {
        
        //Collect all the variables names in this context. Can be
        //cached if required
        ArrayList varNamesInCurrentContext = new ArrayList();
        Iterator iter1 = mRuntimeVariables.keySet().iterator();
        for(;iter1.hasNext();) {
            RVariable variable = (RVariable)iter1.next();
            varNamesInCurrentContext.add(variable.getName());
        }
       
        Map visibleRuntimeVars = new HashMap();
        visibleRuntimeVars.putAll(mRuntimeVariables);
        
        Map parentContextRuntimeVars = mContext.getRuntimeVariables();
        Iterator iter2 = parentContextRuntimeVars.entrySet().iterator();
        
        for(;iter2.hasNext();) {
            Map.Entry runtimeVarEntry =  (Map.Entry) iter2.next();
            RVariable variable = (RVariable)runtimeVarEntry.getKey();
            if(!varNamesInCurrentContext.contains(variable.getName())){
                visibleRuntimeVars.put(variable, runtimeVarEntry.getValue());
            }
        }
        
        return visibleRuntimeVars;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#createRuntimeVariable(com.sun.bpel.model.meta.RVariable)
     */
    public RuntimeVariable createRuntimeVariable(RVariable variable) {
        if (mRuntimeVariables.containsKey(variable)) {
            return new RuntimeVariableImpl(variable, getProcessInstance(), getScopeGuid());
        } else {
            return mContext.createRuntimeVariable(variable);
        }
    	
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
     */
    public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
        if (((RPartnerLink) partnerlink).getAssociatedScope() == mAct) {
            return new RuntimePartnerLinkImpl(partnerlink, getScopeGuid());
        } else {
            return mContext.createRuntimePartnerLink(partnerlink);
        }
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getProcessInstance()
     */
    public BPELProcessInstance getProcessInstance() {
        return mContext.getProcessInstance();
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
//    public Context getContext() {
//        return this;
//    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
     */
    public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
        for (Iterator itr = runtimeVariables.iterator(); itr.hasNext(); ) {
            RuntimeVariable rVar = (RuntimeVariable) itr.next();
            mRuntimeVariables.put(rVar.getVariableDef(), rVar);
            mContext.getProcessInstance().getMonitorMgr().addVariableScope(rVar.getVariableDef().getUniqueId(), this);
        }
        // TODO support for scope level partner links not yet there. hence ignore the 
        // second method parameter
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initEHUponRecovery(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers)
     */
    public void initEHUponRecovery(RuntimeEventHandlers rEH) {
        mEventHandlersUnit = rEH;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getParentContext()
     */
    public Context getParentContext() {
        return mContext;
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#getRequest(com.sun.jbi.engine.bpel.core.bpel.model.meta.RReply)
     */
    public MessageContainer removeRequest(RReply reply) {
        return mIMAScope.removeRequest(reply);
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#receive(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */    
    public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
         mIMAScope.addRequest(rcv, req);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RStartElement, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void removeRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
         mIMAScope.removeRequest(rcv, req);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(String updateValueKey)
     */
    public void addToCRMPUpdateList(String updateValueKey) {
    	mIMAScope.addToCRMPUpdateList(updateValueKey);
		
	}

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(String updateValueKey)
     */
	public boolean crmpUpdateListContains(String updateValueKey) {
		return mIMAScope.crmpUpdateListContains(updateValueKey);
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
     */
    public void declareDefaultMessageExchange() {
        mIMAScope.declareDefaultMessageExchange();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(java.lang.Exception)
     */
    public void sendErrorsForPendingRequests(Exception error) {
        mIMAScope.sendErrorsForPendingRequests(error);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
     */
    public void completePendingInOnlyRequests() {
    	mIMAScope.completePendingInOnlyRequests();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#getRuntimePartnerLink(com.sun.bpel.model.PartnerLink)
     */
    public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
        if (((RPartnerLink) pLink).getAssociatedScope() == mAct) {
            return mRuntimePartnerLinks.get(pLink);
        } else {
            return getParentContext().getRuntimePartnerLink(pLink);
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#setRuntimePartnerLink(
     * com.sun.bpel.model.PartnerLink, RuntimePartnerLink)
     */
    public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
        if (((RPartnerLink) pLink).getAssociatedScope() == mAct) {
            mRuntimePartnerLinks.put(pLink, runtimePLink);
        } else {
            getParentContext().setRuntimePartnerLink(pLink, runtimePLink);
        }
    }

    public Map getRuntimePartnerLinks() {
        final Map visibleRuntimePLs = new HashMap();
        visibleRuntimePLs.putAll(mRuntimePartnerLinks);
        visibleRuntimePLs.putAll(getParentContext().getRuntimePartnerLinks());
        
        return visibleRuntimePLs;
    }
    
    //**************************** Methods related to FCT Handlers **************************//
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#registerCallFrame(
	 * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
	 */
	public void registerCallFrame(ICallFrame callframe) {
		ownedCallFrames.add(callframe);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#registerEnclosedScope(
	 * com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
	 */
	public void registerEnclosedScope(FaultHandlingContext enclosedScope) {
		enclosedScopes.add(enclosedScope);
	}

    public void unregisterEnclosedScope(FaultHandlingContext enclosedScope) {
        enclosedScopes.remove(enclosedScope);
    }

    private void unregisterFromParent(FaultHandlingContext scope) {
        mContext.getFaultHandlingContext().unregisterEnclosedScope(scope);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#notifyFault(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, 
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public synchronized void notifyFault(Fault fault, ICallFrame frame, 
    		BusinessProcessInstanceThread bpit, RequiredObjects rObjs) {
    	
    	if (currentScopeState == ScopeState.Running) {
        	currentScopeState = ScopeState.Faulted;
        	mFaultObj = fault;
        	updateScopeOnState(rObjs);
            frame.getProcessInstance().getMonitorMgr().postEventForFault(this, fault);
        	activateWaitingCFsAndTerminateInnerScopes();
        	
    	} else if (currentScopeState == ScopeState.Faulted) {
    		//Already been notified of a fault by another branch, so return without
    		//doing anything
    		return;
    	} else if(currentScopeState == ScopeState.ExecutingFaultHandlers) {
    		currentScopeState = ScopeState.FaultedInFH;
        	mFaultObj = fault;
    		updateScopeOnState(rObjs);
    		activateWaitingCFsAndTerminateInnerScopes();
    	} else if(currentScopeState == ScopeState.ExecutingTerminationHandler) {
    		currentScopeState = ScopeState.FaultedInTH;
        	mFaultObj = fault;
    		updateScopeOnState(rObjs);    		
    		activateWaitingCFsAndTerminateInnerScopes();
    	} else if(currentScopeState == ScopeState.ExecutingCompensationHandler){
    		currentScopeState = ScopeState.FaultedInCH;
        	mFaultObj = fault;    		
    		updateScopeOnState(rObjs);    		
    		activateWaitingCFsAndTerminateInnerScopes();
    	} else if(currentScopeState == ScopeState.FaultedInCH
    			|| currentScopeState == ScopeState.FaultedInFH
    			|| currentScopeState == ScopeState.FaultedInTH) {
    		return;
    	} else {
    		LOGGER.warning(I18n.loc("BPCOR-6060: Illegal state - method {0} called when the " + 
    				"value of currentScopeState is {1}", "notifyFault()", currentScopeState.toString()));
    	}
	}
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#activateWaitingCallFrames()
     */
    public synchronized void activateWaitingCallFrames() {
    	
    	if(mContext.getProcessInstance().isExiting()){
        	activateWaitingCFsAndTerminateInnerScopes();
        	return;
    	}
    	
    	//The effect of calling this method is to terminated the scope. So we could name it 
    	//appropriately and set the state as terminated here. Currently we are setting the 
    	//state as terminated in doAction or doResumeAction which can be changed. No need 
    	//to activate if the scope state is any of the following. This is because the scope is 
    	//1. either faulted or doing fault handling or 
    	//2. already terminated or doing termination handling 
    	if (currentScopeState == ScopeState.Faulted
    			|| currentScopeState == ScopeState.ExecutingFaultHandlers
    			|| currentScopeState == ScopeState.ExecutingTerminationHandler
    			|| currentScopeState == ScopeState.FaultedInFH
    			|| currentScopeState == ScopeState.FaultedInTH
    			|| currentScopeState == ScopeState.Done) {
    		return;
    	} else {
        	activateWaitingCFsAndTerminateInnerScopes();
    	}
	}
    
    private void activateWaitingCFsAndTerminateInnerScopes() {
    	
    	/* Call the activateWaitingCallFrames() method on the immediately enclosed 
    	 * scopes so that they can put the waiting callframes belonging to them in 
    	 * the ReadyToRun queue.
    	 */
    	Iterator iter = enclosedScopes.iterator();
    	while (iter.hasNext()){
    		Context enclosedScope = (Context) iter.next();
    		enclosedScope.getFaultHandlingContext().activateWaitingCallFrames();
    	}
    	
    	if(!ownedCallFrames.isEmpty()) {
    		((ICallFrame)ownedCallFrames.iterator().next()).getProcessInstance()
    						.getBPELProcessManager().activateWaitingCallFrames(ownedCallFrames);
    	}
    	
    	// clean the FH related data.
    	clearFHRelatedData();
        unregisterFromParent(this);
    }
    
    private void clearFHRelatedData() {
    	//Clean all the owned callframes and inner scopes. When the FH or
    	//TH executes, new callframes and inner scopes will be registered
    	enclosedScopes.clear();
    	ownedCallFrames.clear();
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#isBeingTerminated()
	 */
	public boolean isBeingTerminated() {
		
		if(mContext.getProcessInstance().isExiting()){
			return true;
		}
		
		if(currentScopeState == ScopeState.ExecutingCompensationHandler) {
			return mContext.getFaultHandlingContext().isBeingTerminated();
		} else if(currentScopeState == ScopeState.ExecutingTerminationHandler 
				|| currentScopeState == ScopeState.ExecutingFaultHandlers) {
			return false;
		} else if(currentScopeState == ScopeState.Faulted 
				|| currentScopeState == ScopeState.FaultedInFH
				|| currentScopeState == ScopeState.FaultedInTH
				|| currentScopeState == ScopeState.FaultedInCH) {
			return true;
		} else {
			return mContext.getFaultHandlingContext().isBeingTerminated();
		}
	}
	
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getStateId()
	 */
	public String getStateId() {
		return getStateContext().getState().getId();
	}
	
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeId()
	 */
	public long getScopeId() {
		return mAct.getUniqueId();
	}

	/**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeGuid()
     */
    public String getScopeGuid() {
    	return mScopeGuid;
    }    
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getParentScopeGuid()
	 */
	public String getParentScopeGuid() {
		return mContext.getFaultHandlingContext().getScopeGuid();
	}
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeState()
     */
    public String getScopeState() {
    	if (currentScopeState == ScopeState.Running || 
    			currentScopeState == ScopeState.WaitingForEventsToComplete) {
    		return RUNNING;
    	} else if (currentScopeState == ScopeState.ExecutingFaultHandlers) {
    		return EXE_FAULT_HANDLER;
    	} else if (currentScopeState == ScopeState.ExecutingTerminationHandler) {
    		return EXE_TERMINATION_HANDLER;
    	} else if (currentScopeState == ScopeState.ExecutingCompensationHandler) {
    		return EXE_COMPENSATION_HANDLER;
    	} else if (currentScopeState == ScopeState.FaultedInFH) {
    		return FAULTED_IN_FH;
    	} else if (currentScopeState == ScopeState.FaultedInTH) {
    		return FAULTED_IN_TH;
    	} else if (currentScopeState == ScopeState.FaultedInCH) {
    		return FAULTED_IN_CH;
    	} else if (currentScopeState == ScopeState.Faulted) {
    		return FAULTED;
    	} else if (currentScopeState == ScopeState.Completed) {
    		return COMPLETED;
    	} else if (currentScopeState == ScopeState.Done) {
    		return DONE;
    	} else {
    		// only other state left is the ScopeState.Terminated.
    		//TODO: have to see if this state is needed.
    		return TERMINATED;
    	}
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getFault()
     */
    public Fault getFault() {
		return mFaultObj;
	}
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setFault(com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault, long)
     */
    public void setFault(Fault faultObj, long faultActivityId) {
    	mFaultObj = faultObj;
    	mFaultActivityId = faultActivityId;
    }
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getFaultActivityId()
	 */
	public long getFaultActivityId() {
		return mFaultActivityId;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompensateId()
     */
    public long getCompensateId() {
    	return mCompensateId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompletionOrder()
     */
    public long getCompletionOrder() {
    	return mCompletionOrder;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getEnclosedScopesIterCount(long)
     */
    public long getEnclosedScopesIterCount(long scopeId) {
    	//Each time a ScopeUnitImpl is created this method is called by that ScopeUnitImpl
    	//on its parent, which is this instance.
    	//During recovery this will get called. If the scope is Running, then this
    	//will correctly reflect the enclosed scopes' iteration counts
    	//If the scope state is other than running, for example EFH, then some
    	//of the scope could have been compensated. These scopes (that is, the corresponding
    	//ScopeUnitImpls) would not be created during recovery since these scope would not be
    	//present in the database. In that case, this may not correctly reflect the last iteration
    	//of some scopes. However, since this scope is in EFH, the last iteration of child such
    	//child scopes does not matter. TODO: Need to explain this a little better
    	Long iterVal = mEnclosedScopesIterationCount.get(scopeId);
    	long val = -1;
    	if (iterVal == null) {
    		val = 1;
    	} else {
    		val = iterVal.longValue();
    		val += 1;
    	}
    	mEnclosedScopesIterationCount.put(scopeId, new Long(val));
    	return val;
    }
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#addCompletedScope(com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
	 */
	public void addCompletedScope(FaultHandlingContext completedScope) {
		mCompletedScopes.push((Context)completedScope);
		Utility.sortScopesByCompletionOrder(mCompletedScopes);
	}
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setCompletionOrder(long)
	 */
	public void setCompletionOrder(long completionOrder) {
		// set the member 
		mCompletionOrder = completionOrder;
		if (currentScopeState == ScopeState.Completed) {
			// add to the parent scope only if recovered as 'Completed'.
			mContext.getFaultHandlingContext().addCompletedScope(this);
		}
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setCompensateId(long)
	 */
	public void setCompensateId(long compensateId) {
		mCompensateId = compensateId;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setScopeState(java.lang.String)
	 */
	public void setScopeState(String scopeState) {
		
		if (scopeState.equals(RUNNING)) {
			currentScopeState = ScopeState.Running;
		} else if (scopeState.equals(EXE_FAULT_HANDLER)) {
			currentScopeState = ScopeState.ExecutingFaultHandlers;
		} else if (scopeState.equals(EXE_TERMINATION_HANDLER)) {
			currentScopeState = ScopeState.ExecutingTerminationHandler;
		} else if (scopeState.equals(EXE_COMPENSATION_HANDLER)) {
			currentScopeState = ScopeState.ExecutingCompensationHandler;
		} else if (scopeState.equals(FAULTED_IN_FH)) {
			currentScopeState = ScopeState.FaultedInFH;
		} else if (scopeState.equals(FAULTED_IN_TH)) {
			currentScopeState = ScopeState.FaultedInTH;
		} else if (scopeState.equals(FAULTED_IN_CH)) {
			currentScopeState = ScopeState.FaultedInCH;
		} else if (scopeState.equals(FAULTED)) {
			currentScopeState = ScopeState.Faulted;
		} else if (scopeState.equals(COMPLETED)) {
			currentScopeState = ScopeState.Completed;
		} else if (scopeState.equals(DONE)) {
			currentScopeState = ScopeState.Done;
		} else {
			currentScopeState = ScopeState.Terminated;
		}
	}

	/**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#isInserted()
     */
    public boolean isInserted() {
    	return mIsInserted;
    }
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#markInserted()
	 */
	public void markInserted() {
		mIsInserted = true;
	}
    
    private boolean doFaultHandlingIfPossible (ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, boolean ehCompleted) throws Exception {
    	
    	if (ehCompleted == false) {
    		/* Inform the EH that it should wind up. Note that we use the same API
    		 * associatedScopeCompletes(). This is because the EH, onEvents, onAlarms, 
    		 * their associated scopes and the activities contained in those scopes 
    		 * will be terminated by checking Context.isBeing Terminated(). All the onEvents
    		 * and onAlarms would return control to the EH Unit which will inform this scope
    		 * that the EH has finished winding up.
    		 */
    		if (mEventHandlersUnit != null) {
    			/* Note that we don't change the state here to WaitingForEventsToComplete, since
    			 * the state is Faulted. When the doAction() of scope is called, the scope 
    			 * implicitly knows that it is waiting for EH to wind up. 
    			 */
    			mEventHandlersUnit.associatedScopeCompletes();
    			return false;
    		}
    	}
    	
		//We do not have EH or they were completed
		currentScopeState = ScopeState.ExecutingFaultHandlers;
    	updateScopeOnState(rObjs);

		Unit faultHandlingUnit = ActivityUnitFactory.getInstance().createFaultHandlingUnit(this,
				mFaultObj, ((FaultHandlerScope) getStaticModelActivity()).getFaultHandlers());

		if (faultHandlingUnit == null) {
			faultHandlingUnit = new VirtualCatchAllUnit(this, mAct, mFaultObj);
		}
		
		//The clearFHRelatedData() had removed all the callframes, so we need to add back
		//the callframe that is executing the scope
		ownedCallFrames.add(frame);
		
		//Call the fault handler to execute its activity
		boolean fhCompleted = faultHandlingUnit.doAction(frame, bpit, rObjs);

		//If the fault handling has completed, the scope itself has completed
		if (fhCompleted) {
			doScopeCompletionTasks(frame, bpit, rObjs, true);
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
    		return doPassControlToParent(frame, bpit, rObjs);
		} else {
			return false;
		}
    }
	
    private boolean doTerminationHandlingIfPossible (ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {
    	
    	if (currentScopeState != ScopeState.Terminated) {
        	//Child activity was terminated. Indicate that the scope is also terminated
        	currentScopeState = ScopeState.Terminated;
        	updateScopeOnState(rObjs);
        	
    		/* Inform the EH that it should wind up. Note that we use the same API
    		 * associatedScopeCompletes(). This is because the EH, onEvents, onAlarms, 
    		 * their associated scopes and the activities contained in those scopes 
    		 * will be terminated by checking Context.isBeing Terminated(). All the onEvents
    		 * and onAlarms would return control to the EH Unit which will inform this scope
    		 * that the EH has finished winding up.
    		 */
        	if (mEventHandlersUnit != null) {
        		/* Note that we dont change the state here to WaitingForEventsToComplete, since
        		 * the state is Terminated. When the doAction() of scope is called, the scope 
        		 * implicitly knows that it is waiting for EH to wind up. 
        		 */
        		mEventHandlersUnit.associatedScopeCompletes();
        		return false;
        	}
    	}
    	
    	/* Either we don't have EH or they are completed. Note that if this scope 
    	 * is being terminated, this MUST be a Flow branch. So, if the TH completes 
    	 * we ask the parent to do its action. Since this is a flow branch it will 
    	 * ultimately reach the doResumeAction of Flow which will return true. This 
    	 * will cause this branch to join the flow (in BPELInterpreter.execute() 
    	 * method)
    	 */
    	currentScopeState = ScopeState.ExecutingTerminationHandler;
    	updateScopeOnState(rObjs);
    	
    	//See if we have a TH defined. If yes, get the activity and execute it
    	Scope scopeAct = (Scope)mAct;
    	ActivityUnit thActivityUnit = null;
    	boolean isDefaultTH = false;
    	if(scopeAct.getTerminationHandler() != null) {
            thActivityUnit = ActivityUnitFactory.getInstance().createActivityUnit(this, 
            		(Unit) this, (RActivity) scopeAct.getTerminationHandler().getActivity(), 
            		((ActivityUnit) this).getBranchId());
    	} else {
    		//No TH defined. Execute the default termination handling logic of calling 
    		// <compensate>
    		isDefaultTH = true;
    		// do virtual compensation. Create an object of type Virtual CompensateUnit 
    		//and assign it to thActivityUnit
        	thActivityUnit = new CompensateUnitImpl(this, this, null, getBranchId(), true);
    	}
    	
		//The clearFHRelatedData() had removed all the callframes, so we need to add back
		//the callframe that is executing the scope
		ownedCallFrames.add(frame);
    	
    	boolean thCompleted = thActivityUnit.doAction(frame, bpit, rObjs);

    	//If the termination handling has completed, the scope itself has completed
		if (thCompleted) {
			doScopeCompletionTasks(frame, bpit, rObjs, true);
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
			return doPassControlToParent(frame, bpit, rObjs);
		} else {
			return false;
		}
    }
    
    private boolean doCompensationHandling(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {
    	
    	/*
    	 * Synchronization is to ensure that if one thread has access to the compensation of this 
    	 * scope instance then all others return true(meaning a scope can be attempted for
    	 * compensation only once, it will either complete or return a possible fault of its own).
    	 * TODO: we will go with this assumption for now and if need be changes will have to be made.
    	 */
    	synchronized (mCompensationLockObj) {
            if (mAcquired) {
                return true;
            }
            mAcquired = true;
        }
    	// check to see if the scope is already compensated.
    	if (currentScopeState == ScopeState.Done) {
    		// if the scope has been already compensted then its considered as a NO-OP.
    		return true;
    	}

    	mCallFrame = frame;
    	
    	/*IMP: Since a <compensate>/<compensateScope> activity has called this scope for compensation 
    	* the execution of the compensation handler when completed would need to return back to the 
    	* caller compensate activity and not continue to the next acitiviy in the branch of this scope
    	* as it would be in normal execution. This call to setCompletedActivityState() will ensure that
    	* the mNextActUnit member will be a dummyActivity indicating that the iteration is at an end.
    	* refer to the logic in ActivityUnitImpl.getNextActivityUnit() method.
    	*/
    	setCompletedActivityState();    	

    	CompensationHandler cHandler = ((CompensationHandlerHolder)getStaticModelActivity()).getCompensationHandler();
		//since we are going to execute the child of the compensation handler set the scope state to
		// Executing compensation handler.
		currentScopeState = ScopeState.ExecutingCompensationHandler;
    	updateScopeOnState(rObjs);

    	if (cHandler != null) {
    		RActivity act = (RActivity) cHandler.getActivity();
			ActivityUnit childActUnit = ActivityUnitFactory.getInstance().createActivityUnit
				(this, this, act, getBranchId());
			//boolean done = CodeReUseHelper.executeChildActivities(frame, bpit, rObjs, childActUnit);
			boolean done = childActUnit.doAction(frame, bpit, rObjs);
			if (done) {
	        	//update the Scope as done and update StateImpl for persistence
                frame.setProgramCounter(this);
	    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);

			}
    		return done;
    	} 

    	// do virtual compensation
    	ActivityUnit virtualCompensate = new CompensateUnitImpl(this, this, null, getBranchId(), true);
   		boolean virtualDone = virtualCompensate.doAction(frame, bpit, rObjs);
   		if (virtualDone) {
        	//update the Scope as done and update StateImpl for persistence
    		updateScopeStateAsDone(rObjs.getBPELProcessManager().isPersistenceEnabled(), rObjs);
   		}
   		return virtualDone;
    		
    }
    
    private void doScopeCompletionTasks(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, boolean orderlyCompletion) throws Exception {
    	
    	if(mContext.getProcessInstance().isExiting()) {
    		//This means the process instance is exiting
    		String message = Utility.appendLineNumberAndActName(
    				I18n.loc("BPCOR-6053: Sending errors for the pending requests in the scope, since the process instance is being terminated"), this);
    		sendErrorsForPendingRequests(new Exception(message, null));
    		
    	} else if (currentScopeState == ScopeState.FaultedInFH 
    			|| currentScopeState == ScopeState.FaultedInTH 
    			|| currentScopeState == ScopeState.FaultedInCH) {
    		
    		//This means we are exiting the scope because there is an unhandled fault 
    		String message = Utility.appendLineNumberAndActName(
    				I18n.loc("BPCOR-6133: A fault was not handled in the scope; Fault Name is {0}; Fault Data is {1}. Sending errors for the pending requests in the current scope before propagating the fault to the enclosing scope", 
					mFaultObj.getName(), mFaultObj.getData()), this);
    		sendErrorsForPendingRequests(new Exception(message, mFaultObj.getException()));
    		
    	} else {
    		//This means the scope is completing either normally of after completing FCT Handler
    		String message = Utility.appendLineNumberAndActName(
    				I18n.loc("BPCOR-6134: Sending errors for the pending requests in the current scope, since the scope has completed"), this);
    		sendErrorsForPendingRequests(new Exception(message, null));
    	}

    	if (orderlyCompletion) {
            frame.onActivityComplete(this);
            frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
    	} else {
    		frame.onActivityTerminate(this);
    	}
    	
        removeVariableReferences();
        frame.getProcessInstance().getPersistenctMgr().updateState(this, 
                mContext.getStateContext().getState(), true);
        unregisterFromParent(this);
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getFaultHandlingContext()
	 */
	public FaultHandlingContext getFaultHandlingContext() {
		return this;
	}
	
    public Context getEnclosingContext() {
    	return mContext;
    }
    
    /*
     * Over-ridden method: for a completed scope instance whose compensation handler has been 
     * invoked by a <compensate> or <compensateScope> of an enclosing scope, the return acitvity 
     * would be the <compensate> or <compensateScope> activity that invoked it.
     * There maybe activities in the sequence of the fault handler after the <compensate> and 
     * <compensateScope> to execute, which have to be given a chance to do so. 
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
    	if (mCompensateContinuation != null) {
    		return mCompensateContinuation;
    	}
		
		return super.getEnclosingUnit();
	}

	
    //**************************** End of methods related to FCT Handlers *******************//
	
	/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getStateContext()
	 */
	public StateContext getStateContext() {
	    return mContext.getStateContext();
	}
	
	/**
	 * When a contained scope of this instance completes successfully(without faults) 
	 * it would be added to this instance completed scopes stack in the order of 
	 * completion  
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#
	 * 	pushCompletedScope(com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context)
	 */
	public int pushCompletedScope(Context completedScope) {
		synchronized (mCompletedScopes) {
			if (currentScopeState == ScopeState.Running 
					|| currentScopeState == ScopeState.WaitingForEventsToComplete) {
				mCompletedScopes.push(completedScope);
				return mCompletedScopes.size();
			} else {
				return 0;
			}
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompletedScopes()
	 */
	public Stack<Context> getCompletedScopes() {
		return mCompletedScopes;
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#
	 * 	setCompensateContinuation(com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit)
	 */
	public void setCompensateContinuation(ActivityUnit unit) {
		mCompensateContinuation = unit;
		
		/*IMP:The scope now has been called from a compensate unit and so the branch on which
		 *the CH will executed is the branch of the compensate unit. So we need to change
		 *the scope's branch id to reflect that.
		 */
		mBranchId = unit.getBranchId();
		
		if ((mCompensateContinuation instanceof CompensateScopeUnitImpl) ||
				(mCompensateContinuation instanceof CompensateUnitImpl 
				&& !((CompensateUnitImpl)mCompensateContinuation).isVirtual())) {
			mCompensateId = mCompensateContinuation.getStaticModelActivity().getUniqueId();
		}
	}
	
	private Context popCompletedScope() {
		try {
			return mCompletedScopes.pop();
		} catch (EmptyStackException ex) {
			return null;
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.
	 * 	FaultHandlingContext#getScopeForCompensation()
	 */
	public Context getScopeForCompensation() {
		return popCompletedScope();
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext
	 * #getTargetScopeForCompensation(java.lang.String)
	 */
	public Context getTargetScopeForCompensation(String targetScopeName) {
		Context selectedScope = null;
		String selectedScopeName = null;
		Stack<Context> tempStack = new Stack<Context>();
		synchronized (mCompletedScopes) {
			try {
				while ((selectedScope = popCompletedScope()) != null) {
					selectedScopeName = ((ActivityUnit) selectedScope)
							.getStaticModelActivity().getName();
					if (selectedScopeName.equals(targetScopeName)) {
						return selectedScope;
					} else {
						tempStack.push(selectedScope);
					}
				}
				return null;
			} finally {
				Context tempContext = null;
				try {
					while ((tempContext = tempStack.pop()) != null) {
						//pushCompletedScope(tempContext);
						mCompletedScopes.push(tempContext);
					}
				} catch (EmptyStackException empExcep) {
					// ignore the exception. means that the tempStack is empty
				}
				tempContext = null;
				tempStack = null;
			}
		}
	}
	
	/**
     * On Scope comletion de-reference all the local variables of this scope
     * from the messages they are referencing
     */
    private void removeVariableReferences() {
        
        Iterator runtimeVariablesIter = mRuntimeVariables.entrySet().iterator();
        RuntimeVariable runtimeVariable = null;
        RVariable variable = null;
        WSMessage message = null;
        Map.Entry e = null;
        
        while(runtimeVariablesIter.hasNext()) {
            e = (Map.Entry)runtimeVariablesIter.next();
            variable = (RVariable)e.getKey();
            runtimeVariable = (RuntimeVariable)e.getValue();
            if (runtimeVariable != null) {
                message = runtimeVariable.getWSMessage();
                if (message != null) {
                    message.removeInternalReference(variable);
                }
            }
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    @Override
    public boolean doActionOnRecovery(RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
    	
		frame.convert();
		
		if (frame.getPC() != null) {

			if (LOGGER.isLoggable(Level.FINE)) {
				LOGGER.log(Level.FINE, 
						I18n.loc("BPCOR-3070: Recovery started from activity : {0}", frame.getPC().getName()));
			}
		}
		
		//Recovering from here means that we are just about to start the scope. So check
		//if the scope is being terminated. No need to do TH since we did even start the
		//execution.
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return doPassControlToParent(frame, bpit, rObjs);
		}
    	
        mIsRecoveredUnit = true;
        boolean retVal = doAction(frame, bpit, rObjs);
        if (retVal) {
            return doPassControlToParent(frame, bpit, rObjs);
        }
        return retVal;
    }
    
    /*
     * update the currentScopeState=ScopeState.Done since the scope has been handled
     * by FH/TH/CH and no other operation is possible on this scope.
     * if Persistence is enabled update this state change on the StateImpl object.
     * TODO: For persistence this extra operation could be optimized if we have the
     * iteration count persisted in the DBO, which would make the recovery logic
     * to commit to non-trival rules for selection of scopes for recovery based on 
     * the persisted state of the scopes.  
     */
    private void updateScopeStateAsDone(boolean persistenceEnabled, RequiredObjects rObjs) {
		currentScopeState = ScopeState.Done;
        unregisterFromParent(this);
		// update the Scope on the StateImpl
		if (persistenceEnabled) {
			updateScopeOnState(rObjs);
		}
    }
    
    /*
     * Utility to set the this scope obj on the StateImpl for persistence.
     * Added to have a single point of reference in this class for the 
     * scope state update.
     */
    private void updateScopeOnState(RequiredObjects rObjs) {
		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
	    	getStateContext().getState().updateScope(getBranchId(), this);
		}
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#hasFaulted()
     */
    public boolean hasFaulted() {
        if(currentScopeState == ScopeState.Faulted 
                || currentScopeState == ScopeState.FaultedInFH
                || currentScopeState == ScopeState.FaultedInTH
                || currentScopeState == ScopeState.FaultedInCH) {
            return true;
        } else {    
            return false;
        }
    }
}

