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
 * @(#)RepeatUntilUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.While;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * RepeatUntil activity unit implementation
 * 
 * @author Sun Microsystems
 */

public class RepeatUntilUnitImpl extends StructuredActivityUnitImpl {

	//counter
	private long mCounter = 0;

	/**
	 * Creates a new RepeatUntilUnitImpl object.
	 * 
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public RepeatUntilUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.ActivityUnitImpl#doAction(ICallFrame,
	 *      BusinessProcessInstanceThread, RequiredObjects)
	 */
	public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
			RequiredObjects rObjs) throws Exception {

		frame.setProgramCounter(this);
		frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);
		BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());

		//Check whether we need to terminate this activity
		if (mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
			return true;
		}

		boolean loopCompleted = doRepeatUntil(frame, bpit, rObjs); 
		if (loopCompleted) {
			//Check whether repeatUntil was terminated (true could be returned when the
			//repeatUntil is completed or when the repeatUntil was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doRepeatUntil
				return true;
			} else {
				//repeatUntil is completed
				BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
				return true;
			}
		} else {
			//repeatUntil is not completed
			return false;
		}
	}

	/**
	 * @see StructuredActivityUnitImpl#doResumeAction(com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
	 *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
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

		/* This means that the child activty has completed (which means the suspended loop
		 * has completed. We check if the loop condition has been met, if not we continue
		 * with the next iteration of the loop.
		 */
		boolean loopCompleted = Utility.evaluateCondition(mAct, ((RepeatUntil) mAct).getCondition(), mContext, frame);

		if (loopCompleted == false) {
			//The repeatUntil activity is not completed yet. It will be complete only when 
			//the condition satisfies. 
			loopCompleted = doRepeatUntil(frame, bpit, rObjs);
		}

		if (loopCompleted) {
			//Check whether repeatUntil was terminated (true could be returned when the
			//repeatUntil is completed or when the repeatUntil was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doRepeatUntil
				return super.doPassControlToParent(frame, bpit, rObjs);
			} else {
				//The repeatUntil is complete.
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
				return super.doPassControlToParent(frame, bpit, rObjs);
			}
		} else {
			//repeatUntil is not yet complete.
			return false;
		}
	}

	protected RActivity getChildActivity() {
		return ((RActivityHolder) mAct).getChildActivity();
	}

	/**
	 * executes RepeatUntil
	 * 
	 * @param frame callframe
	 * @param bpit BP process instnace thread
	 * @param rObjs required objects
	 * @return false if the RepeatUntil loop is not complete. true if the condition is evaluated to
	 *         false (meaning RepeatUntil is complete).
	 * @throws Exception Exception
	 */
	private boolean doRepeatUntil(ICallFrame frame, BusinessProcessInstanceThread bpit, 
			RequiredObjects rObjs) throws Exception {

		do {
			frame.onLineChange(this);
			mCounter++;
			boolean childActCompleted = super.doAction(frame, bpit, rObjs);

			/* If the child activity did not complete suspend the loop. The loop will be 
			 * resumed once the child activity completes.
			 */            
			if (childActCompleted == false) {
				return false;
			} else {
				frame.setProgramCounter(this);

				//Child activity completed or was terminated, check if the repeatUntil is being 
				//terminated, before we do the next iteration.
				if (mContext.getFaultHandlingContext().isBeingTerminated()) {
					frame.onActivityTerminate(this);
					return true;
				}
			}

			//If the engine is stopped, no need to continue the repeatUntil loop            
			if(rObjs.getEngine().getState() == Engine.STOPPED){
				return false; // Engine is stopped return from here no more processing.
			}

			//Child activity is completed, continue with the next iteration.

		} while (Utility.evaluateCondition(mAct, ((RepeatUntil) mAct).getCondition(), mContext, frame) == false);

		//repeatUntil completed successfully.
		return true;
	}
}
