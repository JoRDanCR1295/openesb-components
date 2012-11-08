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
 * @(#)StructuredActivityUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StructuredActivity;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;


/**
 * Structured activity unit implementation
 *
 * @author Sun Microsystems
 */
public abstract class StructuredActivityUnitImpl extends ActivityUnitImpl

implements StructuredActivity {
	/**
	 * Creates a new StructuredActivityUnitImpl object.
	 */
	protected StructuredActivityUnitImpl() {
		super();
	}

	/**
	 * Creates a new StructuredActivityUnitImpl object.
	 *
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public StructuredActivityUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
	 *      BusinessProcessInstanceThread, RequiredObjects)
	 */
	public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
			RequiredObjects rObjs) throws Exception {

		//TODO: If this is done in the overidding method before this is called
		//in all the cases, we should remove it from here
		frame.setProgramCounter(this);
		RActivity childAct = getChildActivity();

		//The child activity could be null
		if (childAct == null) {
			frame.onActivityComplete(this);
			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);  	
			return true;
		}

		ActivityUnit childActUnit = ActivityUnitFactory.getInstance().createActivityUnit(mContext, 
				this, childAct, getBranchId());

		boolean childActCompleted = executeChildActivities(frame, bpit, rObjs, childActUnit);
		

		if (childActCompleted) {
			//Either the child was complete or the structured activity was terminated. So we 
			//need to check again the if structured activity is being terminated.
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this); was already done in executeChildActivities()
				return true;
			} 
//Do not compete the activity here because repeated activity will go to next iteration and not necessary complete			
//			else {
//				//structured activity was complete
//				frame.onActivityComplete(this);
//				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this, rObjs, 
//						bpit.getCallFrame().getBPId());
//			}
		}

		return childActCompleted;
	}

	/**
	 * continue enclosing activity
	 *
	 * @param childActUnit child activity unit
	 * @param frame callframe
	 * @param bpit BP process instance thread
	 * @param rObjs required objects
	 *
	 * @return boolean: on completion of enclosing activity unit, returns true; otherwise, returns
	 *         falsef
	 *
	 * @throws Exception Exception
	 */
	protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

		frame.setProgramCounter(this);
    	//Check if suspended
    	if (frame.getProcessInstance().getMonitorMgr().checkInstanceSuspended(bpit)) {
    		return false;
    	}    	


		//Either the child was complete or was terminated. We check if this structured
		//activity is being terminated, if yes we pass control to parent (which will 
		//also do this check).
		if (mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return super.doPassControlToParent(frame, bpit, rObjs);
		}

		//Execute the next activity. This code will only be executed for sequence
		boolean childActsCompleted = executeChildActivities(frame, bpit, rObjs, 
				childActUnit.getNextActivityUnit());

		if (childActsCompleted) {
			//Either the child was complete or the structured activity was terminated. So we 
			//need to check again the if structured activity is being terminated.
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this); was already done in executeChildActivities()
				return super.doPassControlToParent(frame, bpit, rObjs);
			} else {
				//Strutured activity is complete
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
				return doPassControlToParent(frame, bpit, rObjs);
			}

		} else {
			//Structured activity is incomplete
			return false;
		}
	}

	/**
	 * gets child activity
	 *
	 * @return RActivity child activity
	 */
	protected abstract RActivity getChildActivity();

	/**
	 * executes child activities
	 *
	 * @param frame callframe
	 * @param bpit BP process instance thread
	 * @param rObjs required objects
	 * @param actUnit activity unit
	 *
	 * @return true if the children is complete, false if execution is halted  on some activity.
	 *
	 * @throws Exception Exception
	 */
	protected boolean executeChildActivities(
			ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs,
			ActivityUnit actUnit
	) throws Exception {
		return CodeReUseHelper.executeChildActivities(frame, bpit, rObjs, actUnit);
	}
}
