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
 * @(#)RecoveryForPickTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.rmi.server.UID;
import java.util.Collection;
import java.util.Date;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.ROnAlarm;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
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
public class RecoveryForPickTest extends AbstractTestCase {
    public RecoveryForPickTest(String testName) {
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
        TestSuite suite = new TestSuite(RecoveryForPickTest.class);

        return suite;
    }

    public void testCallFrameCountForPick() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testCallFrameCountForPick");

        RBPELProcess processBeforeCrash = loadBPELModel("modelPickBased.bpel");
        BPELProcessManager bpelMgrBeforeCrash = new BPELProcessManagerImpl(processBeforeCrash, mEng, null, null);
        mEng.addModel(processBeforeCrash, bpelMgrBeforeCrash);
        //BPELProcessManager bpelMgr = new BPELProcessManagerImpl(processBeforeCrash, mEng);
        //processBeforeCrash.setBPELProcessManager(bpelMgr);
        String bpelId = new UID().toString();

        // probably should read this BPEL from the DB or from the deployment. 
        RBPELProcess processAfterCrash = loadBPELModel("modelPickBased.bpel");
        BPELProcessManager bpelMgrAfterCrash = new BPELProcessManagerImpl(processAfterCrash, mEng, null, null);
        //processAfterCrash.setBPELProcessManager(bpelMgrNew);
        BPELInterpreter interp = new BPELInterpreter(mEng);
        /*VarHolder varHolder = new VarHolder(processBeforeCrash,
                processAfterCrash, interp);*/
        VarHolder varHolder = new VarHolder(bpelMgrBeforeCrash, bpelMgrAfterCrash, interp);

        StateImpl state = createState(processBeforeCrash.getBPELId());
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity pick1Act = HelperFunc.getActivity(processBeforeCrash,
                    "pick1");

            // simulating a persistence after "msg1"
            ROnMessage onMsg = (ROnMessage) ((Pick) pick1Act).getOnMessage(0);
            onMsg1(onMsg, processBeforeCrash, state, varHolder,
                pick1Act.getUniqueId());
        }

        // simulating a persistence after "Receive"      
        receive(processBeforeCrash, state, varHolder);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity pick2Act = HelperFunc.getActivity(processBeforeCrash,
                    "pick2");

            // if there are no onAlarms on the pick activity we don't persist.
            if ((((Pick) pick2Act).getOnAlarms() != null) &&
                    (((Pick) pick2Act).getOnAlarms().size() > 0)) {
                //assuming timer2_3 branch will win we try to simulate that 
                // branch of pick.
                ROnAlarm timer2_3 = (ROnAlarm) ((Pick) pick2Act).getOnAlarm(1);
                pick2(processBeforeCrash, state, pick2Act, timer2_3, varHolder);
            }
        }

        invoke2_3(processBeforeCrash, state, varHolder);
        invoke(processBeforeCrash, state, varHolder);
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(processBeforeCrash.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testCallFrameCountForPick");
    }

    private void onMsg1(ROnMessage onMsg1, RBPELProcess process, StateImpl state,
        VarHolder varHolder, long pickId) throws Exception {
        String stateId = state.getId();
        long branchInvokeCounter = 0;
        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pickId, (new Date()).getTime(), onMsg1.getUniqueId(), branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Pick",
            cf.getPC() instanceof Pick);
        assertTrue(cf.getPC().getName().equals("pick1"));
    }

    private void receive(RBPELProcess process, StateImpl state, VarHolder varHolder)
        throws Exception {
        RActivity receiveAct = HelperFunc.getActivity(process, "receive");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            receiveAct.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be receive",
            cf.getPC() instanceof Receive);
        assertTrue(cf.getPC().getName().equals("receive"));
    }

    private void pick2(RBPELProcess process, StateImpl state, RActivity pick2Act,
        ROnAlarm timer2_3, VarHolder varHolder) throws Exception {
        String stateId = state.getId();
        long branchInvokeCounter = 0;
        // instead of using the timer activities date, just using the current date 
        // for the simplicity of the test case code.
        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pick2Act.getUniqueId(), System.currentTimeMillis() + 10000, timer2_3.getUniqueId(),
            branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Pick",
            cf.getPC() instanceof Pick);
        assertTrue(cf.getPC().getName().equals("pick2"));

        // After recovery, this onAlarm is assumed to be the chosen path.
        // persist this state.
        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pick2Act.getUniqueId(), (new Date()).getTime(), timer2_3.getUniqueId(), 
            branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash, stateId,
                mEng);*/
        cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash, stateId,
                mEng);
        assertEquals(1, cfs.size());
        cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Pick",
            cf.getPC() instanceof Pick);
        assertTrue(cf.getPC().getName().equals("pick2"));
    }

    private void invoke2_3(RBPELProcess process, StateImpl state,
        VarHolder varHolder) throws Exception {
        RActivity invoke2_3Act = HelperFunc.getActivity(process, "invoke2_3");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invoke2_3Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.      
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be invoke",
            cf.getPC() instanceof Invoke);
        assertTrue(cf.getPC().getName().equals("invoke2_3"));
    }

    private void invoke(RBPELProcess process, StateImpl state, VarHolder varHolder)
        throws Exception {
        RActivity invokeAct = HelperFunc.getActivity(process, "invoke");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invokeAct.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.      
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be invoke",
            cf.getPC() instanceof Invoke);
        assertTrue(cf.getPC().getName().equals("invoke"));
    }

    private class VarHolder {
        //RBPELProcess mProcessBeforeCrash;
        //RBPELProcess mProcessAfterCrash;
    	BPELProcessManager mProcMgrBeforeCrash;
    	BPELProcessManager mProcMgrAfterCrash;
        BPELInterpreter mInterp;

        VarHolder(BPELProcessManager procMgrBeforeCrash,
        		BPELProcessManager procMgrAfterCrash, BPELInterpreter interp) {
            //mProcessBeforeCrash = processBeforeCrash;
            //mProcessAfterCrash = processAfterCrash;
        	mProcMgrBeforeCrash = procMgrBeforeCrash;
        	mProcMgrAfterCrash = procMgrAfterCrash;
            mInterp = interp;
        }
    }
}
