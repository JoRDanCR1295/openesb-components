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
 * @(#)DBStateForFlowTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.rmi.server.UID;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Flow;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class DBStateForFlowTest extends AbstractTestCase {
    public DBStateForFlowTest(String testName) {
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
        TestSuite suite = new TestSuite(DBStateForFlowTest.class);

        /*      TestSuite suite = new TestSuite();
                suite.addTest(new DBStateForFlowTest("testPCVals"));
        */
        return suite;
    }

    public void testPCVals() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testPCVals");

        RBPELProcess process = loadBPELModel("modelFlowBased.bpel");
        mEng.addModel(process, null, null);
        String bpelId = new UID().toString();

        StateImpl state = createState(process.getBPELId());
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity flow1Act = HelperFunc.getActivity(process, "Flow1");
            Collection branches = ((Flow) flow1Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }

            state.enterFlow(flow1Act.getUniqueId(), ids);

            // simulating a persistence after "Receive1"
            receive1(flow1Act, process, state);

            // simulating a persistence after "invoke1"
            invoke1(flow1Act, process, state);

            state.exitFlow(flow1Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
        }

        // simulating a persistence after "Receive2"      
        receive2(process, state);

        // simulating a persistence after "Receive2_0"      
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2Act = HelperFunc.getActivity(process, "Flow2");
            Collection branches = ((Flow) flow2Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }
            //test is not accounting that FLOW is adding the persistance point. 
            //It needs to be enhanced
    		//state.updatePC(branchid, flow2Act.getUniqueId());
    		//mStateMgr.persistState((State) state, TransactionInfo.getLocalTxInfo(), null);
            
            state.enterFlow(flow2Act.getUniqueId(), ids);

            receive2_0(flow2Act, process, state);
        }

        // simulating persistence after REceive2_1
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2_1Act = HelperFunc.getActivity(process, "Flow2_1");
            Collection branches = ((Flow) flow2_1Act).getActivities();
            List ids = new ArrayList();
            RActivity childTree = null;

            for (Iterator itr = branches.iterator(); itr.hasNext();) {
                childTree = (RActivity) itr.next();
                ids.add(new Long(childTree.getUniqueId()));
            }

            state.enterFlow(flow2_1Act.getUniqueId(), ids);

            receive2_1(flow2_1Act, process, state);
        }

        // simulating a persistence after invoke2_0
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            String stateId = state.getId();
            RActivity flow2Act = HelperFunc.getActivity(process, "Flow2");

            invoke2_0(flow2Act, process, state);
        }

        // TODO cutting short the walk of bpel.
        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity flow2_1Act = HelperFunc.getActivity(process, "Flow2_1");
            RActivity flow2Act = HelperFunc.getActivity(process, "Flow2");
            // the branch ID in the following ((2 lines) code is not accurate. 
            state.exitFlow(flow2_1Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
            state.exitFlow(flow2Act.getUniqueId(), RBPELProcess.DEFAULT_PROCESS_BRANCH_ID);
        }

        invoke2(process, state);
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testPCVals");
    }

    private void receive1(RActivity flow1Act, RBPELProcess process, StateImpl state)
        throws Exception {
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow1Act,
                "Receive1", Long.MIN_VALUE);
        RActivity receive1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        String stateId = state.getId();
        state.updatePC(branchId, receive1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify state objects state.
            ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                    "'");
            assertTrue(rs.next());

            String persistedStatusVal = rs.getString(1);
            assertEquals("RUNNING", persistedStatusVal);

            // verify lastcheckpoint insert
            rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                    "where stateid = '" + stateId + "'");
            assertTrue(rs.next());

            long persistedPCVal = rs.getLong(1);
            assertEquals(receive1Act.getUniqueId(), persistedPCVal);
            conn.close();
        }
    }

    private void invoke1(RActivity flow1Act, RBPELProcess process, StateImpl state)
        throws Exception {
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow1Act,
                "invoke1", Long.MIN_VALUE);
        RActivity invoke1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        String stateId = state.getId();
        state.updatePC(branchId, invoke1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify state objects state.
        ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                "'");
        assertTrue(rs.next());

        String persistedStatusVal = rs.getString(1);
        assertEquals("RUNNING", persistedStatusVal);

        // verify lastcheckpoint insert
        rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal1 = rs.getLong(1);
        assertTrue(rs.next());

        long persistedPCVal2 = rs.getLong(1);
        assertTrue((invoke1Act.getUniqueId() == persistedPCVal1) ||
            (invoke1Act.getUniqueId() == persistedPCVal2));
        conn.close();
    }

    private void receive2(RBPELProcess process, StateImpl state)
        throws Exception {
        RActivity receive2Act = HelperFunc.getActivity(process, "Receive2");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            receive2Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify state objects state.
        ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                "'");
        assertTrue(rs.next());

        String persistedStatusVal = rs.getString(1);
        assertEquals("RUNNING", persistedStatusVal);

        // verify lastcheckpoint insert
        rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal = rs.getLong(1);
        assertEquals(receive2Act.getUniqueId(), persistedPCVal);
        conn.close();
    }

    private void receive2_0(RActivity flow2Act, RBPELProcess process,
        StateImpl state) throws Exception {
        String stateId = state.getId();
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "Receive2_0", Long.MIN_VALUE);
        RActivity receive2_0Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, receive2_0Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // scoping variables. No other reason for this if.
        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());
        List persistedPCValList = new ArrayList();
        persistedPCValList.add(rs.getLong(1));

        assertTrue(rs.next());
        persistedPCValList.add(rs.getLong(1));

        assertTrue(persistedPCValList.contains(receive2_0Act.getUniqueId()));
        //assertTrue(persistedPCValList.contains(flow2Act.getUniqueId()));
        conn.close();
    }

    private void receive2_1(RActivity flow2Act, RBPELProcess process,
        StateImpl state) throws Exception {
        String stateId = state.getId();
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "Receive2_1", Long.MIN_VALUE);
        RActivity receive2_1Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, receive2_1Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // scoping variables. No other reason for this if.
        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal1 = rs.getLong(1);
        assertTrue(rs.next());

        long persistedPCVal2 = rs.getLong(1);
        assertTrue((receive2_1Act.getUniqueId() == persistedPCVal1) ||
            (receive2_1Act.getUniqueId() == persistedPCVal2));
        conn.close();
    }

    private void invoke2_0(RActivity flow2Act, RBPELProcess process, StateImpl state)
        throws Exception {
        String stateId = state.getId();
        Object[] ret = HelperFunc.getActivityWithBranchId((RActivityHolder) flow2Act,
                "invoke2_0", Long.MIN_VALUE);
        RActivity invoke2_0Act = (RActivity) ret[0];
        long branchId = ((Long) ret[1]).longValue();

        state.updatePC(branchId, invoke2_0Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // scoping variables. No other reason for this if.
        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        List persistedPCValList = new ArrayList();
        persistedPCValList.add(rs.getLong(1));

        assertTrue(rs.next());
        persistedPCValList.add(rs.getLong(1));

        assertTrue(rs.next());
        persistedPCValList.add(rs.getLong(1));
        
        assertTrue( persistedPCValList.contains(invoke2_0Act.getUniqueId()));

        conn.close();
    }

    private void invoke2(RBPELProcess process, StateImpl state)
        throws Exception {
        RActivity invoke2Act = HelperFunc.getActivity(process, "invoke2");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invoke2Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal = rs.getLong(1);
        assertEquals(invoke2Act.getUniqueId(), persistedPCVal);
        conn.close();
    }
}
