/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;

/**
 * @author Sun Inc
 * Jul 30, 2007
 */
public abstract class AbstractThrowUnitImpl extends ActivityUnitImpl {
    private boolean mWaitingToThrow = false;
    
    public AbstractThrowUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl#doAction(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {
   	
        if (mWaitingToThrow) {
            return doThrowAction(frame, bpit, rObjs);
        } else {
            return doBeforeThrowAction(frame, bpit, rObjs);
        }
    }

    protected boolean doThrowAction(
    		ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {

    	QName mesgType = mFault.getData() != null ? mFault.getData().getMessageType() : null;
    	frame.onFault(getActivityUnit(), mFault.getName(), mesgType, mFault.getData());
    	frame.getProcessInstance().getMonitorMgr().postEventForFault(this, mFault);
    	frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
    	mContext.getFaultHandlingContext().notifyFault(mFault, frame, bpit, rObjs);
    	return doPassControlToParent(frame, bpit, rObjs);
    }    

    protected boolean doBeforeThrowAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        frame.setProgramCounter(this);
        onLineChange(rObjs, frame);
        mWaitingToThrow = true;
        BPELProcessManager processManager = rObjs.getBPELProcessManager();
        processManager.addToReadyToRunQueue(frame);
        return false;
    }

    protected void setWaitingToThrow(boolean waitingToThrow) {
        mWaitingToThrow = waitingToThrow;
    }

    protected boolean getWaitingtoThrow() {
        return mWaitingToThrow;
    }
    
    protected abstract ActivityUnit getActivityUnit();
    
    protected abstract void onLineChange(RequiredObjects rObjs, ICallFrame frame);
}
