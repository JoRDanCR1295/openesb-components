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
 * @(#)VirtualThrowUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;


import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;


/**
 * Synthetic throw activity unit implementation
 *
 * @author Sun Microsystems
 */
public class VirtualThrowUnitImpl extends AbstractThrowUnitImpl {
    private ActivityUnit mActualUnit;
    private boolean mUpdateState;
    
    /**
     * Creates a new VirtualThrowUnitImpl object.
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     * @param actualUnit TODO
     * @param fault fault that was thrown
     * @param updateState, flag that suggest either persistence needs to be done or not
     */
    public VirtualThrowUnitImpl(Context context, Unit parentActUnit, 
            RActivity act, long branchId, ActivityUnit actualUnit, 
            boolean updateState, Fault fault) {
        super(context, parentActUnit, act, branchId);
        mFault = fault;
        mActualUnit = actualUnit;
        mUpdateState = updateState;
    }
    
    protected boolean doThrowAction(
    		ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {

    	QName mesgType = mFault.getData() != null ? mFault.getData().getMessageType() : null;
    	frame.onFault(getActivityUnit(), mFault.getName(), mesgType, mFault.getData());
    	frame.getProcessInstance().getMonitorMgr().postEventForFault(this, mFault);
    	mContext.getFaultHandlingContext().notifyFault(mFault, frame, bpit, rObjs);
    	return doPassControlToParent(frame, bpit, rObjs);
    }    

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.AbstractThrowUnitImpl#getActivityUnit()
     */
    protected ActivityUnit getActivityUnit() {
    	return mActualUnit;
    }

    @Override
    protected void onLineChange(RequiredObjects rObjs, ICallFrame frame) {
    	//Doesn't support
    }    
}
