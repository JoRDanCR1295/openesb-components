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
 * @(#)RecoveryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.rmi.server.UID;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELProcessManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class RecoveryTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.test.bpelpersist.RecoveryTest");

    public RecoveryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mEng = OneTimesetUp.getSetUp().getEngine();
        mEngId = mEng.getId();
        mConnProp = OneTimesetUp.getSetUp().getConnectionProperties();
        mStateMgr = mEng.getStateManager();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(RecoveryTest.class);

        return suite;
    }

    /** Assumes that the FirstTest class hasn't failed.
     * @throws Exception
     */
    public void testCallFrameCount() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testCallFrameCount");

        RBPELProcess processBeforeCrash = loadBPELModel("modelIdParsing.bpel");
        BPELProcessManager bpelMgrBeforeCrash = new BPELProcessManagerImpl(processBeforeCrash, mEng, null, null);
        mEng.addModel(processBeforeCrash, bpelMgrBeforeCrash);
        //BPELProcessManager bpelMgr = new BPELProcessManagerImpl(processBeforeCrash, mEng);
        //processBeforeCrash.setBPELProcessManager(bpelMgr);
        String bpelId = new UID().toString();

        StateImpl state = createState(processBeforeCrash.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating a crash at "invoke1"
        RActivity invoke1Act = HelperFunc.getActivity(processBeforeCrash,
                "Invoke1");

        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invoke1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        RBPELProcess process = loadBPELModel("modelIdParsing.bpel");
        BPELProcessManager bpelMgrNew = new BPELProcessManagerImpl(process, mEng, null, null);
        //process.setBPELProcessManager(bpelMgrNew);
        /*Collection cfs = mStateMgr.getCallframe(process, stateId,
                mEng); */
        Collection cfs = mStateMgr.getCallframe(bpelMgrNew, stateId,
                mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Invoke",
            cf.getPC() instanceof Invoke);
        assertTrue(cf.getPC().getName().equals("Invoke1"));

        // simulating a crash at "Reply1"
        RActivity reply1Act = HelperFunc.getActivity(processBeforeCrash,
                "Reply1");

        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            reply1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        //cfs = mStateMgr.getCallframe(process, stateId, mEng);
        cfs = mStateMgr.getCallframe(bpelMgrNew, stateId, mEng);
        assertEquals(1, cfs.size());
        cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Reply",
            cf.getPC() instanceof Reply);
        assertTrue(cf.getPC().getName().equals("Reply1"));

        // simulating a crash at "Wait2"
        RActivity wait2Act = HelperFunc.getActivity(processBeforeCrash, "Wait2");
        long branchInvokeCounter = 0;
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            wait2Act.getUniqueId(), (new Date()).getTime(), branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        //cfs = mStateMgr.getCallframe(process, stateId, mEng);
        cfs = mStateMgr.getCallframe(bpelMgrNew, stateId, mEng);
        assertEquals(1, cfs.size());
        cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Reply",
            cf.getPC() instanceof Wait);
        assertTrue(cf.getPC().getName().equals("Wait2"));

        // simulating a crash at "reply2"
        RActivity reply2Act = HelperFunc.getActivity(processBeforeCrash,
                "Reply2");

        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            reply2Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        //cfs = mStateMgr.getCallframe(process, stateId, mEng);
        cfs = mStateMgr.getCallframe(bpelMgrNew, stateId, mEng);
        assertEquals(1, cfs.size());
        cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Reply",
            cf.getPC() instanceof Reply);
        assertTrue(cf.getPC().getName().equals("Reply2"));
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(processBeforeCrash.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testCallFrameCount");
    }
}
