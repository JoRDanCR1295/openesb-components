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
 * @(#)VirtualCatchAllUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;

/**
 * Unit that encapsulates the behavior of a default fault handler
 *
 * @author Sun Microsystems
 */
public class VirtualCatchAllUnit extends CatchAllUnitImpl {
    private RActivity mScopeOrProcess;
    private VirtualRethrowUnit virtualRethrowUnit;
    private CompensateUnitImpl virtualCompensateUnit;
    
    /**
     * This Unit represents the catchAll in the default fault handler
     * <catchAll>
     * 	<sequence>
     *   <compensate/>
     *   <rehtrow/>
     *  </sequence>
     * </catchAll>
     * It creates a synthetic CompensateUnit and RethrowUnit and executes
     * them one after the other.
     * @param catchAll catchall
     * @param fault TODO
     * @param scopeUnit scope activity unit
     */
    public VirtualCatchAllUnit(Context context, RActivity scopeAct, Fault fault) {
        mContext = context;
        mScopeOrProcess = scopeAct;
        mFault = fault;
        mIsProcessLevelFaultHandling = (mContext.getParentContext() == null);
        
        long branchId = ((ActivityUnit) mContext).getBranchId();

        /*
         * The default catch all will have a <compensate> and <rethrow> activity.
         */
        virtualCompensateUnit = new CompensateUnitImpl(this, (Unit) this, mScopeOrProcess, branchId, true);
        virtualRethrowUnit = new VirtualRethrowUnit(this, (Unit) this, mScopeOrProcess, branchId);
        
        // set the <rethrow> activity as the next activity on the <compensate> activity.
        virtualCompensateUnit.setNextVirtualActivity(virtualRethrowUnit);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        return CodeReUseHelper.executeChildActivities(frame, bpit, rObjs, virtualCompensateUnit);
    }
    
    /**
     * DOCUMENT ME!
     * @param frame DOCUMENT ME!
     * @param bpit DOCUMENT ME!
     * @param rObjs DOCUMENT ME!
     * @param childActUnit DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame,
            BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
    	
    	if (childActUnit instanceof VirtualRethrowUnit) {
    		return ((StructuredActivityUnitImpl) mContext).doResumeAction(null, frame, bpit, rObjs);
    	} else {
        	ActivityUnit nextChildActUnit = childActUnit.getNextActivityUnit();
    		return nextChildActUnit.doAction(frame, bpit, rObjs);
    	}
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
        return (ActivityUnit) mContext;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingScopeUnit()
     */
    public ActivityUnit getEnclosingScopeUnit() {
        return (ActivityUnit) mContext;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        return mContext;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit#getStaticModel()
     */
    public BPELElement getStaticModel() {
        return mScopeOrProcess;
    }

    /**
     * @return the virtualCompensateUnit
     */
    public CompensateUnitImpl getVirtualCompensateUnit() {
    	return virtualCompensateUnit;
    }
}
