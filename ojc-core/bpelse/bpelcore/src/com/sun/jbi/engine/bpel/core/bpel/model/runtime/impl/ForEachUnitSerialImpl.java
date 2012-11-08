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
 * @(#)ForEachUnitSerialImpl.java 
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
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathException;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.Branches;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Implementation of ForEach unit when parallel="no"
 * 
 * @author Sun Microsystems
 * 
 */
public class ForEachUnitSerialImpl extends StructuredActivityUnitImpl {
	private static final Logger LOGGER = Logger.getLogger(ForEachUnitSerialImpl.class.getName());

	private int mStartCounter;
	private int mFinalCounter;
	private int mCounter;
	private int mSucceededCount;
	private int mBranches;
	private boolean mHasCompleteCondition;
	private boolean mCountCompletedBranches;
	private int mTotalCount;

	/**
	 * Constructor for a serial ForEach
	 * 
	 * @param parentActUnit Parent ActivityUnit
	 * @param act RActivity
	 * @param branchId BranchID
	 */
	public ForEachUnitSerialImpl(Context context, Unit parentActUnit, 
			RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
	}

	// TODO TEMPORARY UNTIL REFACTORING CAN OCCUR
	public void initUponRecovery(int counter, int successes, 
			int startCount, int finalCount, 
			int conditionCount) {
		// set values from DB
		mCounter = counter;
		mSucceededCount = successes;
		mStartCounter = startCount;
		mFinalCounter = finalCount;
		mTotalCount = mFinalCounter - mStartCounter + 1;
		mBranches = conditionCount;
		// parse model to initialize completion condition
		initCompletionCondition();
	}

	/**
	 * Returns the enclosed activity, it is always a scope
	 */
	protected RActivity getChildActivity() {
		return ((RActivityHolder) mAct).getChildActivity();
	}

	/**
	 * Continues the execution. This is called when the enclosed Scope runs to complete when the
	 * last event is received
	 */
	protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

		frame.setProgramCounter(this);

		//Either the child was complete or was terminated. We check if this
		//activity is being terminated, if yes we pass control to parent (which
		//will also do this check).
		if (mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return super.doPassControlToParent(frame, bpit, rObjs);
		}

		//This means the current iteration is done
		boolean childScopeCompletedNormally = ((ScopeUnitImpl) childActUnit).hasCompletedNormally();
		if (childScopeCompletedNormally) {
			mSucceededCount++;
		}

		mCounter++;
		if (!isPossible()) {
			frame.onActivityComplete(this);
			frame.getProcessInstance().getMonitorMgr().postActivityFaultedEvent(this);    	
			throw new StandardException(StandardException.Fault.CompletionConditionFailure);
		}

		if (isDone()) {
			specificUpdateState(rObjs, true);
			frame.onActivityComplete(this);
			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
			return super.doPassControlToParent(frame, bpit, rObjs);
		} 

		boolean loopCompleted = doForEach(frame, bpit, rObjs);

		if (loopCompleted) {
			//Check whether foreach was terminated (true could be returned when the
			//foreach is completed or when the foreach was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doForEach
				return super.doPassControlToParent(frame, bpit, rObjs);
			} else {
				specificUpdateState(rObjs, true);
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
				return super.doPassControlToParent(frame, bpit, rObjs);
			}
		} else {
			//forEach not completed yet.
			return false;
		}
	}

	/**
	 * Execute ForEach The first entry of ForEach does the following: 
	 * 1. Initialize 2. calls doForEach
	 * 
	 * @param frame The Callframe
	 * @param bpit The BusinessProcessInstance
	 * @param rObjs The RequiredObjects
	 */
	public boolean doAction(ICallFrame frame, 
			BusinessProcessInstanceThread bpit,
			RequiredObjects rObjs) throws Exception {

		frame.setProgramCounter(this);
		frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);
		BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());

		//Check whether we need to terminate this activity
		if (mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return true;
		}

		boolean shouldProceed = initialize(frame, rObjs);
        if (!shouldProceed) {
            return true;
        }
		specificUpdateState(rObjs, false);

		boolean loopCompleted = doForEach(frame, bpit, rObjs);

		if(loopCompleted) {
			//Check whether foreach was terminated (true could be returned when the
			//foreach is completed or when the foreach was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doForEach
				return true;
			} else {
				specificUpdateState(rObjs, true);
				BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
				return true;
			}
		} else {
			return false;
		}
	}

	/**
	 * Do each iteration
	 * 
	 * @param frame
	 * @param bpit
	 * @param rObjs
	 * @return
	 * @throws Exception
	 */
	private boolean doForEach(ICallFrame frame, BusinessProcessInstanceThread bpit, 
			RequiredObjects rObjs) throws Exception {
		frame.onLineChange(this);
		// begin loop
		for (int i = mCounter; mCounter < mTotalCount; i++) {
			frame.onLineChange(this);
			updateCounterState(rObjs, frame);
			ScopeUnitImpl childScopeUnit = (ScopeUnitImpl) ActivityUnitFactory.getInstance()
			.createActivityUnit(mContext, this, getChildActivity(), getBranchId());

			//Add variable to the child scope unit.
			addVariable(childScopeUnit, mCounter + mStartCounter, rObjs, frame);           

			boolean childScopeCompleted = executeChildActivities(frame, bpit, rObjs, childScopeUnit);
			if (childScopeCompleted == false) {
				return false;
			} else {
				frame.setProgramCounter(this);

				//Child activity completed or was terminated, check if the forEach is being 
				//terminated, before we do the next iteration.
				if (mContext.getFaultHandlingContext().isBeingTerminated()) {
					frame.onActivityTerminate(this);
					return true;
				}
			}

			if(rObjs.getEngine().getState() == Engine.STOPPED){
				return false; // Engine is stopped return from here no more processing.
			}

			boolean childScopeCompletedNormally = childScopeUnit.hasCompletedNormally();
			if (childScopeCompletedNormally) {
				mSucceededCount++;
			}
			mCounter++;

			if (!isPossible()) {
				throw new StandardException(StandardException.Fault.CompletionConditionFailure);
			}
			if (isDone()) {
				return true;
			}
		}
		return true;
	}

	private void specificUpdateState(RequiredObjects rObjs, boolean isUnitDone) {
		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
			long branchId = getBranchId();
			MutableState state = mContext.getStateContext().getState();
			if (isUnitDone) {
				state.endForEach(mAct.getUniqueId(), branchId);
			} else {
				state.addForEach(mAct.getUniqueId(), branchId, mStartCounter,
						mFinalCounter, mBranches);
				state.startForEach(mAct.getUniqueId(), branchId);
			}
		}
	}

	/* Notifies state the loop has begun. */
	private void updateCounterState(RequiredObjects rObjs, ICallFrame frame) {
		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
			MutableState state = mContext.getStateContext().getState();

			state.updateForEach(mAct.getUniqueId(), getBranchId(), mCounter,
					mSucceededCount);
		}
	}

	/**
	 * Evaluate the N out M complete condition
	 * @return whether it is done
	 */
	protected boolean isDone () {
		if (mHasCompleteCondition) {
			if (mCountCompletedBranches) {
				if (mSucceededCount >= mBranches) {
					return true;
				}
			}
			else if (mCounter >= mBranches) {
				return true;
			}
		} 
		return false;
	}

	/**
	 * Checks whether it is still possible to proceed
	 * 
	 * @return
	 */
	protected boolean isPossible() {
		if (!mHasCompleteCondition) {
			return true;
		}
		if (mCountCompletedBranches && 
				(mSucceededCount + mTotalCount - mCounter < mBranches)) {
			return false;
		}
		return true;
	}

	/**
	 * Initialize the counter values and decide whether to proceed
	 * 
	 * @param frame The callFrame
	 * @param rObjs The RequiredObjects
	 * @return true, proceed, false, no need to proceed further. false when final counter value 
     *  is less than the start counter value
	 */
	private boolean initialize(ICallFrame frame, RequiredObjects rObjs) {
	    ForEach lForEach = (ForEach) mAct;

	    String startCounterString = lForEach.getIterator().getStartCounterValue().getValue();
	    String finalCounterString = lForEach.getIterator().getFinalCounterValue().getValue();
	    String branchesString = null;

	    if (lForEach.getCompletionCondition() != null) {
	        Branches lbranches = lForEach.getCompletionCondition().getBranches();
	        branchesString = lbranches.getValue();
	        String cntCBs = lbranches.getCountCompletedBranchesOnly();
	        mCountCompletedBranches = (cntCBs != null && cntCBs.equals("yes"));
	        mHasCompleteCondition = true;
	    }

	    try {
	        mStartCounter = evaluateCounter(rObjs.getInterp(), 
	                frame, 
	                mAct, 
	                startCounterString, /* lenient= */
	                "false", 
	        "startCounterValue");
	        if (mStartCounter < 0) {
	            throw new NumberFormatException(
	                    I18n.loc("BPCOR-6051: ForEach start counter :{0} shouldn't be negative", startCounterString));
	        }
	        mFinalCounter = evaluateCounter(rObjs.getInterp(), 
	                frame, 
	                mAct, 
	                finalCounterString, /* lenient= */
	                "false", 
	        "finalCounterValue");
	        if (mFinalCounter < 0) {
	            throw new NumberFormatException(
	                    I18n.loc("BPCOR-6150: ForEach final counter :{0} shouldn't be negative", finalCounterString));
	        }
	        if (branchesString != null) {
	            mBranches = evaluateCounter(rObjs.getInterp(), 
	                    frame, 
	                    mAct, 
	                    branchesString, /* lenient= */
	                    "false", 
	            "branches");
	        }
	        if (mBranches < 0) {
	            throw new NumberFormatException(
	                    I18n.loc("BPCOR-6138: ForEach completion condition :{0} shouldn't be negative", branchesString));
	        }
	    } catch (NumberFormatException e) {
	        if (LOGGER.isLoggable(Level.FINE)) {
	            LOGGER.log(Level.FINE, I18n.loc("BPCOR-3001: associated forEach block is: {0}", frame.getPC()), e);
	        }
	        throw new StandardException(StandardException.Fault.InvalidExpressionValue, e);
	    }
	    if (mFinalCounter < mStartCounter) {
	        return false;
	    }
	    mTotalCount = mFinalCounter - mStartCounter + 1;

	    if (mHasCompleteCondition && (mBranches > mTotalCount)) {
	        throw new StandardException(StandardException.Fault.InvalidBranchCondition);
	    }

	    // If impossible to fulfill the complete condition
	    if (!isPossible()) {
	        throw new StandardException(StandardException.Fault.CompletionConditionFailure);
	    }
	    return true;
	}

	/**
	 * Initialize the completion condition during recovery.
	 * 
	 * @param frame The callFrame
	 * @param rObjs The RequiredObjects
	 * @return true, proceed, false, no need to proceed further
	 */
	private void initCompletionCondition() {
		ForEach lForEach = (ForEach) mAct;

		if (lForEach.getCompletionCondition() != null) {
			Branches lbranches = lForEach.getCompletionCondition().getBranches();
			String cntCBs = lbranches.getCountCompletedBranchesOnly();
			mCountCompletedBranches = (cntCBs != null && cntCBs.equals("yes"));
			mHasCompleteCondition = true;
		}
	}

	/* Adds variable to scope. This variable is also added on to the state object for persistence. */
	private void addVariable(ScopeUnitImpl scopeUnit, int counter,
			RequiredObjects rObjs, ICallFrame frame) {
		ForEach lForEach = (ForEach) mAct;
		RVariable var = (RVariable) lForEach.getCounterVariable();
		// ForEach counter variable belongs to the child scope of the ForEach construct.
		// and hence is not delegated to the scopeUnit to create the variable.
		// The variable is created with the child scope unit id as its scope id.
		RuntimeVariable counterVar = new RuntimeVariableImpl(var, 
				scopeUnit.getProcessInstance(), scopeUnit.getScopeGuid());
		counterVar.setXSDVariableData(new Integer(counter));
		postVarEvent((RVariable) ((ForEach) mAct).getCounterVariable(), counterVar.getXSDVariableData(), rObjs, frame);
		scopeUnit.addVariable(counterVar);
	}       

	/**
	 * Evaluate a boolean XPath expression in the context of the current thread
	 * 
	 * @param frame
	 *            The current thread
	 * @param xpath
	 *            expression
	 * @param lenient
	 *            xpath leniency
	 * 
	 * @return Result of evaluation
	 * 
	 * @throws RuntimeException
	 *             DOCUMENT ME!
	 */
	private int evaluateCounter(BPELInterpreter iter, ICallFrame frame, BPELElement element,
			String xpath, String lenient, String varName) {
	    int returnVal = 0;
	    Object result;
	    try {
	        JXPathContext jxpathContext = Utility.newJXPathContext(element, mContext, null);
	        result = jxpathContext.getValue(xpath);
	    } catch (JXPathException ex) {

	        String msg = I18n.loc("BPCOR-6052: Exception while evaluating the xpath {0} " +
	                "in BPEL({1}) \n at line number {2} " + 
	                "Associated BPEL artifact is: {3}", ex.getMessage(), frame.getProcess().getBPELId(), 
	                element.getLocator().getLineNumber(), element);
	        LOGGER.log(Level.WARNING, msg);

	        throw new RuntimeException(msg);
	    }
	    if (result instanceof String) {
	        returnVal = Integer.parseInt((String) result);
	    } else if (result instanceof Number) {
	        returnVal = ((Number) result).intValue();
	    } else {
	        throw new NumberFormatException(I18n.loc("BPCOR-6055: ForEach counter :{0} is not valid", varName));
	    }

	    return returnVal;
	}

	private void postVarEvent(RVariable variable, Object content, RequiredObjects objs, ICallFrame frame) {
		if (frame.getProcessInstance().getMonitorMgr().generateEventsForVariable(variable.getUniqueId())) {
			Variable var = new VariableImpl(variable, content, mContext);
			Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
			variableMap.put(VariableEvent.VariableType.COUNTER,
					new ArrayList<Variable>());
			variableMap.get(VariableEvent.VariableType.COUNTER).add(var);
			frame.getProcessInstance().getMonitorMgr().postVariableEvent(this, variableMap, false, null, null);
		}
	}		
}
