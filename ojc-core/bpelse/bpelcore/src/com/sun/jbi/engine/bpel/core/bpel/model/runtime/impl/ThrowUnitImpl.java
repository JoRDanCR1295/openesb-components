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
 * @(#)ThrowUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RThrow;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;


/**
 * Throw activity unit implementation
 *
 * @author Sun Microsystems
 */
public class ThrowUnitImpl extends AbstractThrowUnitImpl {
    
    /**
     * Creates a new ThrowUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public ThrowUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
        RThrow throwAct = (RThrow) mAct;
        QName faultName = throwAct.getRFaultName();
        
        RVariable faultVariable = ((RVariableElement) throwAct).getRVariable();
        WSMessage faultData = null;
        
        if (faultVariable != null) {
            RuntimeVariable rVar = mContext.getRuntimeVariable(faultVariable);
            if ((rVar == null) && (faultVariable.getWSDLMessageType().getParts().size() == 0)) {
                rVar = mContext.createRuntimeVariable(faultVariable);
                Utility.initializeVariableValue(rVar);
                mContext.setRuntimeVariable(rVar);
                StateContext stateCtx = mContext.getStateContext();
                mContext.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                        rVar, getBranchId());
            }/* else {
                    Utility.verifyValue(faultVariable.getName(), rVar, false);
            }*/
            faultData = rVar.getWSMessage();
            //faultData = faultData.copy();
        }
        mFault = new FaultImpl(faultName, faultData);
    }

   
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.AbstractThrowUnitImpl#getActivityUnit()
     */
    protected ActivityUnit getActivityUnit() {
        return this;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.AbstractThrowUnitImpl#onLineChange(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    protected void onLineChange(RequiredObjects rObjs, ICallFrame frame) {
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);  
    }

    @Override
    protected boolean doBeforeThrowAction(ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        RVariable faultVariable = ((RVariableElement)mAct).getRVariable();
        if (faultVariable != null)
            Utility.verifyValue(faultVariable.getName(), mContext.getRuntimeVariable(faultVariable), false);
        frame.setProgramCounter(this);
        onLineChange(rObjs, frame);
        setWaitingToThrow(true);
        BPELProcessManager processManager = rObjs.getBPELProcessManager();
        processManager.addToReadyToRunQueue(frame);
        return false;
    }


}
