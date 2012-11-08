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
 * @(#)WhileUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

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
 * While activity unit implementation
 *
 * @author Sun Microsystems
 */
public class WhileUnitImpl extends StructuredActivityUnitImpl {
	/** counter */
	long mCounter = Long.MIN_VALUE;

	/**
	 * Creates a new WhileUnitImpl object.
	 *
	 * @param parentActUnit parent activity unit
	 * @param act activity
	 * @param branchId branch ID
	 */
	public WhileUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
		mCounter = 0;
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

		boolean loopCompleted = doWhile(frame, bpit, rObjs);

		if (loopCompleted) {
			//Check whether while was terminated (true could be returned when the
			//while is completed or when the while was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doWhile
				return true;
			} else {
				//The while is complete.
				BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
				return true;
			}
		} else {
			//while is not completed yet
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

		/* This means that the child activty has completed (which means the suspended 
		 * loop has completed. We continue with the next iteration until the while 
		 * condition remains true.
		 */
		boolean loopCompleted = doWhile(frame, bpit, rObjs);

		if (loopCompleted) {
			//Check whether while was terminated (true could be returned when the
			//while is completed or when the while was terminated, so we need to
			//check again).
			if (mContext.getFaultHandlingContext().isBeingTerminated()) {
				//frame.onActivityTerminate(this) was already called in doWhile
				return super.doPassControlToParent(frame, bpit, rObjs);
			} else {
				//The while is complete.
				frame.onActivityComplete(this);
				frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
				return super.doPassControlToParent(frame, bpit, rObjs);
			}
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

	/**
	 * executes while
	 *
	 * @param frame callframe
	 * @param bpit BP process instance thread
	 * @param rObjs required objects
	 *
	 * @return false if the while loop is not complete. true if the condition is evaluated to false
	 *         (meaning while is complete).
	 *
	 * @throws Exception Exception
	 */
	private boolean doWhile(ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
	throws Exception {

		while (onLineChange (frame)) {
			mCounter++;
			boolean childActCompleted = super.doAction(frame, bpit, rObjs);

			/* If the child activity did not complete suspend the loop. The loop will be 
			 * resumed once the child activity completes.
			 */

			if (childActCompleted == false) {
				return false;
			} else {
				frame.setProgramCounter(this);

				//Child activity completed or was terminated, check if the forEach is being terminated, 
				//before we do the next iteration.
				if (mContext.getFaultHandlingContext().isBeingTerminated()) {
					frame.onActivityTerminate(this);
					return true;
				}
			}

			//If the engine is stopped, no need to continue the while loop
			if (rObjs.getEngine().getState() == Engine.STOPPED) {
				return false;
			}

			//Child activity is completed, continue with the next iteration.
		}

		//While completed successfully.
		return true;
	}

	private boolean onLineChange (ICallFrame frame) {
		frame.onLineChange(this);
		//BPELTraceManager.getInstance().doTraceOnStart(mAct.getTrace(), mContext);
		return Utility.evaluateCondition(mAct, ((While) mAct).getCondition(), mContext, frame);
	}
}
