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
 * @(#)RecoveryForFlowTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.rmi.server.UID;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
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
public class RecoveryForFlowTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.bpelpersist.RecoveryForFlowTest");

    public RecoveryForFlowTest(String testName) {
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
        TestSuite suite = new TestSuite(RecoveryForFlowTest.class);

        return suite;
    }

    public void testCallFrameCountForFlow() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testCallFrameCountForFlow");

        RBPELProcess processBeforeCrash = loadBPELModel("modelFlowBased.bpel");
        BPELProcessManager bpelMgrBeforeCrash = new BPELProcessManagerImpl(processBeforeCrash, mEng, null, null);
        mEng.addModel(processBeforeCrash, bpelMgrBeforeCrash);
        //BPELProcessManager bpelMgr = new BPELProcessManagerImpl(processBeforeCrash, mEng);
        //processBeforeCrash.setBPELProcessManager(bpelMgr);
        StateImpl state = createState(processBeforeCrash.getBPELId());
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // probably should read this BPEL from the DB or from the deployment. 
        RBPELProcess processAfterCrash = loadBPELModel("modelFlowBased.bpel");
        BPELProcessManager bpelMgrAfterCrash  = new BPELProcessManagerImpl(processAfterCrash, mEng, null, null);
        //BPELProcessManager bpelMgrNew = new BPELProcessManagerImpl(processAfterCrash, mEng);
        //processAfterCrash.setBPELProcessManager(bpelMgrNew);

        BPELInterpreter interp = new BPELInterpreter(mEng);
        VarHolder varHolder = new VarHolder(bpelMgrBeforeCrash, bpelMgrAfterCrash, interp);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity flow1Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow1");
            Collection branches = ((Flow) flow1Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }

            state.enterFlow(flow1Act.getUniqueId(), ids);

            // simulating a persistence after "Receive1"
            receive1(flow1Act, state, varHolder);

            // simulating a persistence after "invoke1"
            invoke1(flow1Act, state, varHolder);

            state.exitFlow(flow1Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
        }

        // simulating a persistence after "Receive2"      
        receive2(state, varHolder);

        // simulating a persistence after "Receive2_0"      
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow2");
            Collection branches = ((Flow) flow2Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }

            state.enterFlow(flow2Act.getUniqueId(), ids);

            receive2_0(flow2Act, state, varHolder);
        }

        // simulating persistence after REceive2_1
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2_1Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow2_1");
            Collection branches = ((Flow) flow2_1Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }

            state.enterFlow(flow2_1Act.getUniqueId(), ids);

            receive2_1(flow2_1Act, state, varHolder);
        }

        // simulating a persistence after invoke2_0
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow2");

            invoke2_0(flow2Act, state, varHolder);
        }

        // TODO cutting short the walk of bpel.
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity flow2_1Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow2_1");
            RActivity flow2Act = HelperFunc.getActivity(processBeforeCrash,
                    "Flow2");
            // the branch ID in the following ((2 lines) code is not accurate. 
            state.exitFlow(flow2_1Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
            state.exitFlow(flow2Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
        }

        invoke2(state, varHolder);
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(processBeforeCrash.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testCallFrameCountForFlow");
    }

    private void receive1(RActivity flow1Act, StateImpl state, VarHolder varHolder)
        throws Exception {
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow1Act,
                "Receive1", Long.MIN_VALUE);
        RActivity receive1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        String stateId = state.getId();
        state.updatePC(branchId, receive1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash, stateId, mEng);
        assertEquals(2, cfs.size());

        ICallFrame cf;
        ICallFrame parentCF = null;
        ICallFrame receive1CF = null;

        for (Iterator itr = cfs.iterator(); itr.hasNext();) {
            cf = (ICallFrame) itr.next();
            if (cf.getPC() instanceof Receive) {
                receive1CF = cf;
            }
        }
        parentCF = receive1CF.getParent();

        assertNotNull(receive1CF);
        assertTrue(
            "Persisted activity name is 'Receive1' and \n Retrieved activity " +
            "name is " + receive1CF.getPC().getName(),
            "Receive1".equals(receive1CF.getPC().getName()));

        RActivity parentPC = parentCF.getPC();
        assertTrue(parentPC instanceof Flow);
        assertTrue(parentPC.getName().equals("Flow1"));
    }

    private void invoke1(RActivity flow1Act, StateImpl state, VarHolder varHolder)
        throws Exception {
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow1Act,
                "invoke1", Long.MIN_VALUE);
        RActivity invoke1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        String stateId = state.getId();
        state.updatePC(branchId, invoke1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash, stateId, mEng);
        assertEquals(2, cfs.size());

        ICallFrame cf;
        ICallFrame parentCF = null;
        ICallFrame invoke1CF = null;

        for (Iterator itr = cfs.iterator(); itr.hasNext();) {
            cf = (ICallFrame) itr.next();
            if (cf.getPC() instanceof Invoke) {
                invoke1CF = cf;
            }
        }
        parentCF = invoke1CF.getParent();

        assertNotNull(invoke1CF);
        assertTrue(
            "Persisted activity name is 'invoke1' and \n Retrieved activity " +
            "name is " + invoke1CF.getPC().getName(),
            "invoke1".equals(invoke1CF.getPC().getName()));

        RActivity parentPC = parentCF.getPC();
        assertTrue(parentPC instanceof Flow);
        assertTrue(parentPC.getName().equals("Flow1"));
    }

    private void receive2(StateImpl state, VarHolder varHolder)
        throws Exception {
        /*RActivity receive2Act = HelperFunc.getActivity(varHolder.mProcessBeforeCrash,
                "Receive2"); */
        RActivity receive2Act = HelperFunc.getActivity(varHolder.mProcMgrBeforeCrash.getBPELProcess(),
        	"Receive2");
        
        String stateId = state.getId();

        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            receive2Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());

        ICallFrame cf = ((ICallFrame[]) cfs.toArray(new ICallFrame[] {  }))[0];
        assertTrue("Persisted and retrieved activity are not same, should be Receive",
            cf.getPC() instanceof Receive);
        assertTrue(cf.getPC().getName().equals("Receive2"));
    }

    private void receive2_0(RActivity flow2Act, StateImpl state, VarHolder varHolder)
        throws Exception {
        String stateId = state.getId();

        // simulating a crash after "Receive2_0"      
        Collection branches = ((Flow) flow2Act).getActivities();
        List ids = new ArrayList();
        RActivity childTree = null;

        for (Iterator itr = branches.iterator(); itr.hasNext();) {
            childTree = (RActivity) itr.next();
            ids.add(new Long(childTree.getUniqueId()));
        }

        state.enterFlow(flow2Act.getUniqueId(), ids);

        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "Receive2_0", Long.MIN_VALUE);
        RActivity receive2_0Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, receive2_0Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash, stateId, mEng);
        assertEquals(4, cfs.size());

        ICallFrame cf;
        ICallFrame parentCF = null;
        ICallFrame receive2_0CF = null;
        int siblingCFCount = 0;

        for (Iterator itr = cfs.iterator(); itr.hasNext();) {
            cf = (ICallFrame) itr.next();
            if (cf.getPC() instanceof Receive) {
                receive2_0CF = cf;
            } else if (cf.getPC() instanceof Sequence) {
                siblingCFCount++;
            }
        }
        parentCF = receive2_0CF.getParent();
        
        assertNotNull(receive2_0CF);
        assertTrue(
            "Persisted activity name is 'Receive2_0' and \n Retrieved activity " +
            "name is " + receive2_0CF.getPC().getName(),
            "Receive2_0".equals(receive2_0CF.getPC().getName()));

        RActivity parentPC = parentCF.getPC();
        assertTrue(parentPC instanceof Flow);
        assertTrue(parentPC.getName().equals("Flow2"));
        assertEquals("Should have two sibling branches ", 2, siblingCFCount);
    }

    private void receive2_1(RActivity flow2Act, StateImpl state, VarHolder varHolder)
        throws Exception {
        String stateId = state.getId();
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "Receive2_1", Long.MIN_VALUE);
        RActivity receive2_1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, receive2_1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(5, cfs.size());
    }

    private void invoke2_0(RActivity flow2Act, StateImpl state, VarHolder varHolder)
        throws Exception {
        String stateId = state.getId();
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "invoke2_0", Long.MIN_VALUE);
        RActivity invoke2_0Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, invoke2_0Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(5, cfs.size());
    }

    private void invoke2(StateImpl state, VarHolder varHolder)
        throws Exception {
        /*RActivity invoke2Act = HelperFunc.getActivity(varHolder.mProcessBeforeCrash,
                "invoke2");*/
        RActivity invoke2Act = HelperFunc.getActivity(varHolder.mProcMgrBeforeCrash.getBPELProcess(),
        "invoke2");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invoke2Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // crashed here.
        // simulating a load of process after the crash.
        /*Collection cfs = mStateMgr.getCallframe(varHolder.mProcessAfterCrash,
                stateId, mEng);*/
        Collection cfs = mStateMgr.getCallframe(varHolder.mProcMgrAfterCrash,
                stateId, mEng);
        assertEquals(1, cfs.size());
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
