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
 * @(#)EmptyUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;


/**
 * Empty activity unit implementation
 *
 * @author Sun Microsystems
 */
public class EmptyUnitImpl extends ActivityUnitImpl {
    
    /**
     * Creates a new EmptyUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public EmptyUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.ActivityUnitImpl#doAction(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
    	//No need to check for termination, since the activity may be allowed
    	//to complete as per the termination semantics
        frame.setProgramCounter(this);
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);        
        BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
        frame.onActivityComplete(this);
        frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);    	
        return true;
    }
}
