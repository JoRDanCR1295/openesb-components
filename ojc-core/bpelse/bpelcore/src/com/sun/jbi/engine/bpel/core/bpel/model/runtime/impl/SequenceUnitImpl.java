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
 * @(#)SequenceUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;


/**
 * Sequence activity unit implementation
 *
 * @author Sun Microsystems
 */
public class SequenceUnitImpl extends StructuredActivityUnitImpl {
    
    /**
     * Creates a new SequenceUnitImpl object.
     */
    protected SequenceUnitImpl() {
        super();
    }

    /**
     * Creates a new SequenceUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public SequenceUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        return ((RActivityHolder) mAct).getChildActivity();
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(
            ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {

        frame.setProgramCounter(this);
        frame.onLineChange(this);
        BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
        frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	
    	
        //Check whether we need to terminate this activity
        if (mContext.getFaultHandlingContext().isBeingTerminated()) {
        	frame.onActivityTerminate(this);
        	return true;
        }
    	
        boolean result = super.doAction(frame, bpit, rObjs);
        if (result) {
        	if (mContext.getFaultHandlingContext().isBeingTerminated()) {
            	frame.onActivityTerminate(this);
            } else {
            	BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
     			frame.onActivityComplete(this);
    			frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);     	
            }
        }
        return result;
    }

}
