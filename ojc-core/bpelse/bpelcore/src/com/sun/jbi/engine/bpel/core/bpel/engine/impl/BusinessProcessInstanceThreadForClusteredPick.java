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
 * @(#)BusinessProcessInstanceThreadForClusteredPick.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author mbhasin
 *
 */
public class BusinessProcessInstanceThreadForClusteredPick extends BusinessProcessInstanceThread {

    private static final Logger LOGGER = Logger.getLogger(BusinessProcessInstanceThreadForClusteredPick.class.getName());

    BPELProcessManager mProcMgr;

    String mInstanceId;

    long mDeadline;


    /**
     * @param procMgr
     * @param engine
     * @param instanceId
     * @param deadline
     */
    public BusinessProcessInstanceThreadForClusteredPick(BPELProcessManager procMgr, Engine engine, String instanceId, 
    		long deadline) {
        super(procMgr, engine, null);
        this.mProcMgr = procMgr;
        this.mInstanceId = instanceId;
        this.mDeadline = deadline;
        setType(BusinessProcessInstanceThread.TIMEOUT);
        setTimeout(deadline);
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread#execute()
     */
    public void execute() {
        // If the instance is passivated. Activate the instance here and pass it for recovery
        if (activateInstance()) {
            recoverInstance();
        }        
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread#isReady()
     */
    public boolean isReady() {
        if (super.isReady()) {
            return true;
        }
        return false;
    }

    /**
     * activate the instance
     */
    private boolean activateInstance() {
        int updateCount = mProcMgr.activateClusteringPassInstance(mInstanceId);
        if (updateCount == 1) {
            return true;
        }
        return false;
    }

    /**
     * recover the instance
     */
    private void recoverInstance() {
        List list = new ArrayList();
        list.add(mInstanceId);
        try {
            mProcMgr.recover(list);
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6024: Failed During Recovery"), ex);
        }
    }
}
