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
 * @(#)CompensateScopeUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RCompensateScope;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.CompensateContinuation;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;

/**
 * The runtime model of the <compensateScope> activity.
 * @author pVarghese
 *
 */
public class CompensateScopeUnitImpl extends ActivityUnitImpl 
	implements CompensateContinuation {
	
	/*
	 * Stores the next iteration of the target scope instance group, only if
	 * There was a blocking activity in the compensation handler and on the 
	 * return path would have to compensate the next iteration instance of this
	 * scope, which would be stored in this variable. 
	 */
	private Context mNextIterationOfTargetScope = null;
	
	public CompensateScopeUnitImpl(Context context, Unit parentActUnit, 
			RActivity act, long branchId) {
		super(context, parentActUnit, act, branchId);
	}
	
	private String getTargetScope() {
		return ((RCompensateScope)mAct).getRTargetName();
	}

	/*
	 * Compesate the target Scope instance or the target scope insatnce group if 
	 * the target Scope is in an iterative construct.
	 */
	public boolean doAction(ICallFrame frame,
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
		throws Exception {
		FaultHandlingContext fContext = mContext.getFaultHandlingContext();
		String targetScopeName = getTargetScope();
		Context completedScope = null;

		if (mNextIterationOfTargetScope != null) {
			completedScope = mNextIterationOfTargetScope;
			mNextIterationOfTargetScope = null;
		} else {
			completedScope = fContext.getTargetScopeForCompensation(targetScopeName);
		}
		
		while (completedScope != null) {
			((FaultHandlingContext)completedScope).setCompensateContinuation(this);
			boolean isChildDone = CodeReUseHelper.executeChildActivities(
					frame, bpit, rObjs, (ActivityUnit) completedScope);
			if (!isChildDone) {
				return false;
			}
			completedScope = fContext.getTargetScopeForCompensation(targetScopeName);
		}
		
		return true;
	}

	public boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
		
		FaultHandlingContext fContext = mContext.getFaultHandlingContext();

		if(fContext.isBeingTerminated()){
			return doPassControlToParent(frame, bpit, rObjs);			
		}
		
		String targetScopeName = getTargetScope();
		if ((mNextIterationOfTargetScope = fContext.getTargetScopeForCompensation(targetScopeName)) 
				!= null) {
			boolean childCompleted = doAction(frame, bpit, rObjs);
			if (childCompleted) {
				return doPassControlToParent(frame, bpit, rObjs);
			} else {
				return false;
			}
		} else {
			return doPassControlToParent(frame, bpit, rObjs);
		}
	}
}
