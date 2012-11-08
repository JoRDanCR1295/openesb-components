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
 * @(#)CompensateUnitImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.CompensateContinuation;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;

/**
 * The runtime model of the <compensate> activity both for the virtual and user
 * defined.
 * @author pVarghese
 *
 */
public class CompensateUnitImpl extends ActivityUnitImpl 
	implements CompensateContinuation {

	// set if this unit is a virtual unit.
	private boolean mIsVirtual;
	private ActivityUnitImpl mVirtualNextAct;
	
    public CompensateUnitImpl(Context context, Unit parentActUnit, 
    		RActivity act, long branchId, boolean isVirtual) {
    	super(context, parentActUnit, act, branchId);
    	mIsVirtual = isVirtual;
    }
    
    public boolean isVirtual() {
    	return mIsVirtual;
    }
    
    /*
     * For a virtual <compensate> activity, there can only be a virtual <rethrow>
     * activity comming after it, when used in the Default Fault Handler(virtual 
     * catchAll). This public api is used only for that purpose and will set this 
     * rethrow activity only if the <compensate> is marked as virtual by the 
     * mIsVirtual instance flag.
     */
    public void setNextVirtualActivity(VirtualRethrowUnit unit) {
    	if (mIsVirtual) {
        	mVirtualNextAct = unit;
    	}
    }
    
    /*
     * Over-ridden: This is over ridden to check for the case where the <compensate>
     * activity is a virtual compensate activity used in a Default Fault Handler. In 
     * which case it would return the virtual <rethrow> activity. 
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#getNextActivityUnit()
     */
    public ActivityUnit getNextActivityUnit() {
    	if (mIsVirtual && mVirtualNextAct != null) {
    		return mVirtualNextAct;
    	}
    	return super.getNextActivityUnit();
    }    
    
	/*
	 * Get the compeleted scopes of the scope(context) and compensate them 
	 * in the reverse order of completion. Here the <compensate> activity
	 * should compensate all the completed scopes. There could be a condition
	 * where one compensation handler could have a blocking messaging activity
	 * which would on resumption call into the doResumeAction of this class
	 * where the chance for the other scopes that was not compensated should be 
	 * given. 
	 *
	 */
	public boolean doAction(ICallFrame frame,
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
			throws Exception {
		FaultHandlingContext fContext = mContext.getFaultHandlingContext();

		Context completedScope = null;
		while ((completedScope = fContext.getScopeForCompensation()) != null) {
			((FaultHandlingContext) completedScope).setCompensateContinuation(this);
			boolean isChildDone = CodeReUseHelper.executeChildActivities(
					frame, bpit, rObjs, (ActivityUnit) completedScope);
			if (!isChildDone) {
				return false;
			}
		}
		return true;
	}
	
	/*
	 * Takes care to compensate other completed Scopes that did not get a chance
	 * to get compesated in the previous iteration. 
	 * Refer to the doAction() method docs for information. 
	 */
	public boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

		FaultHandlingContext fContext = mContext.getFaultHandlingContext();
		
		if(fContext.isBeingTerminated()){
			return doPassControlToParent(frame, bpit, rObjs);			
		}
		
		if (fContext.getCompletedScopes().size() > 0) {
			boolean childCompleted = doAction(frame, bpit, rObjs);
			if (childCompleted) {
				return doPassControlToParent(frame, bpit, rObjs);
			} else {
				return false;
			}
		} else {
			// if all the scopes have been compensated then return iteration to parent.
			return doPassControlToParent(frame, bpit, rObjs);
		}
	}
}
