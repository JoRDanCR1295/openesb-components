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
 * @(#)BusinessProcessInstanceThreadForPick.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;


/**
 * BPEL process instance thread for Pick
 *
 * @author Sun Microsystems
 */
class BusinessProcessInstanceThreadForPick extends BusinessProcessInstanceThread {
    private int mOnMessageCount;
    private PickManagerImpl mPickMgr;

    /**
     * constructor
     * @param engine 
     * @param procMgr 
     *
     * @param frame callframe
     * @param deadline deadline
     * @param onMesgCount TODO
     * @param pickMgr Pick manager
     */
    BusinessProcessInstanceThreadForPick(BPELProcessManager procMgr, Engine engine, ICallFrame frame, long deadline,
        int onMesgCount, PickManagerImpl pickMgr) {
        super(procMgr, engine, frame);
        setType(BusinessProcessInstanceThread.TIMEOUT);
        setTimeout(deadline);
        mOnMessageCount = onMesgCount;
        mPickMgr = pickMgr;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread#isReady()
     */
    public boolean isReady() {

        boolean flag = super.isReady();

        if (flag) {
            flag = hasExpired();
            if (flag) {
            	if (getType() == SCALABILITY_PASSIVATED) {
            		return flag;
            	} else {
            		flag = mPickMgr.removeFromPendingQueue(getCallFrame());
            	}
            }
        }

        // If the flag is false it means that the on-message was executed and 
        // this timer based path is no longer valid.
        return flag;
    }
}
