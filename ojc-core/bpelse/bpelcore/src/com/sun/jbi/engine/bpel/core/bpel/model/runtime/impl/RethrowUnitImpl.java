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

import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;

/**
 * @author Sun Inc
 * Jul 25, 2007
 */
public class RethrowUnitImpl extends AbstractThrowUnitImpl {

    /**
     * @param context RethrowContext
     * @param parentActUnit
     * @param act
     * @param branchId
     * @param isSynthetic TODO
     */
    public RethrowUnitImpl(Context context, Unit parentActUnit, RActivity act,
            long branchId) {
       
        super(context, parentActUnit, act, branchId);
        mFault = context.getFaultHandlingContext().getFault();
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

}
