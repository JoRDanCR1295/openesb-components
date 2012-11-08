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
 * @(#)DBstateForPickTest.java 
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
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Date;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.ROnAlarm;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DateTime;
import com.sun.jbi.engine.bpel.core.bpel.util.Duration;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class DBstateForPickTest extends AbstractTestCase {
    public DBstateForPickTest(String testName) {
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
        TestSuite suite = new TestSuite(DBstateForPickTest.class);

        return suite;
    }

    public void testPCVals() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testPCVals");

        RBPELProcess process = loadBPELModel("modelPickBased.bpel");
        mEng.addModel(process, null, null);
        String bpelId = new UID().toString();

        StateImpl state = createState(process.getBPELId());
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity pick1Act = HelperFunc.getActivity(process, "pick1");

            // simulating a persistence after "msg1"
            ROnMessage onMsg = (ROnMessage) ((Pick) pick1Act).getOnMessage(0);
            onMsg1(pick1Act, onMsg.getUniqueId(), process, state);
        }

        // simulating a persistence after "Receive"      
        receive(process, state);

        if (true) {
            // scoping variables. so no clash and no overlooked errors.
            RActivity pick2Act = HelperFunc.getActivity(process, "pick2");

            // if there are no onAlarms on the pick activity we don't persist.
            if ((((Pick) pick2Act).getOnAlarms() != null) &&
                    (((Pick) pick2Act).getOnAlarms().size() > 0)) {
                //assuming timer2_3 branch will win we try to simulate that 
                // branch of pick.
                ROnAlarm timer2_3 = (ROnAlarm) ((Pick) pick2Act).getOnAlarm(1);
                pick2(process, state, pick2Act, timer2_3);
            }
        }

        invoke2_3(process, state);
        invoke(process, state);
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);   
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testPCVals");
    }

    private void onMsg1(RActivity pick, long onMesgId, RBPELProcess process,
        StateImpl state) throws Exception {
        String stateId = state.getId();
        long branchInvokeCounter = 0;
        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pick.getUniqueId(), onMesgId, branchInvokeCounter);
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
        rs = stmt.executeQuery(
                "select activityid, timeval, pickcompositeactid from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal = rs.getLong(1);
        assertEquals(pick.getUniqueId(), persistedPCVal);
        assertNull(rs.getDate(2));

        long pickCompositeId = rs.getLong(3);
        assertEquals(pickCompositeId, onMesgId);
        conn.close();
    }

    private void receive(RBPELProcess process, StateImpl state)
        throws Exception {
        RActivity receiveAct = HelperFunc.getActivity(process, "receive");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            receiveAct.getUniqueId());
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
        assertEquals(receiveAct.getUniqueId(), persistedPCVal);
        conn.close();
    }

    private void pick2(RBPELProcess process, StateImpl state, RActivity pick2Act,
        ROnAlarm timer2_3) throws Exception {
        String stateId = state.getId();
        DateTime dateTime = new DateTime();

        Duration dur = Duration.parse(((OnAlarm) timer2_3).getFor());
        dateTime.add(dur);

        Date date = dateTime.toCalendar().getTime();

        long waitTime = date.getTime();
        Timestamp currDate = new Timestamp(waitTime);
        
        long branchInvokeCounter = 0;
        //Timestamp currDate = new Timestamp(System.currentTimeMillis());
        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pick2Act.getUniqueId(), currDate.getTime(), timer2_3.getUniqueId(), 
            branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // just scoping variables.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify state objects state.
            ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                    "'");
            assertTrue(rs.next());

            String persistedStatusVal = rs.getString(1);
            assertEquals("RUNNING", persistedStatusVal);

            // verify lastcheckpoint insert
            rs = stmt.executeQuery(
                    "select activityid, timeval, pickcompositeactid from LASTCHECKPOINT " +
                    "where stateid = '" + stateId + "'");
            assertTrue(rs.next());

            long persistedPCVal = rs.getLong(1);
            assertEquals(pick2Act.getUniqueId(), persistedPCVal);
            assertNotNull(rs.getTimestamp(2));
            // DEVNOTE: MySQL has a bug which causes it to lose millisecond information. Using date format to get 
            // around that bug.
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            assertEquals(sdf.format(rs.getTimestamp(2)), sdf.format(currDate));
            //assertEquals(rs.getTimestamp(2), currDate);

            long pickCompositeActId = rs.getLong(3);
            assertEquals(pickCompositeActId, timer2_3.getUniqueId());
            conn.close();
        }

        state.updatePCWithPickCompositeId(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            pick2Act.getUniqueId(), timer2_3.getUniqueId(), branchInvokeCounter);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // just scoping variables.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify state objects state.
            ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                    "'");
            assertTrue(rs.next());

            String persistedStatusVal = rs.getString(1);
            assertEquals("RUNNING", persistedStatusVal);

            // verify lastcheckpoint insert
            rs = stmt.executeQuery(
                    "select activityid, timeval, pickcompositeactid from LASTCHECKPOINT " +
                    "where stateid = '" + stateId + "'");
            assertTrue(rs.next());

            long persistedPCVal = rs.getLong(1);
            assertEquals(pick2Act.getUniqueId(), persistedPCVal);
            assertNull(rs.getTimestamp(2));

            long pickCompositeActId = rs.getLong(3);
            assertEquals(pickCompositeActId, timer2_3.getUniqueId());
            conn.close();
        }
    }

    private void invoke2_3(RBPELProcess process, StateImpl state)
        throws Exception {
        RActivity invoke2_3Act = HelperFunc.getActivity(process, "invoke2_3");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invoke2_3Act.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid, timeval from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal = rs.getLong(1);
        assertEquals(invoke2_3Act.getUniqueId(), persistedPCVal);
        assertNull(rs.getDate(2));
        conn.close();
    }

    private void invoke(RBPELProcess process, StateImpl state)
        throws Exception {
        RActivity invokeAct = HelperFunc.getActivity(process, "invoke");
        String stateId = state.getId();
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            invokeAct.getUniqueId());
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify lastcheckpoint insert
        ResultSet rs = stmt.executeQuery("select activityid, timeval from LASTCHECKPOINT " +
                "where stateid = '" + stateId + "'");
        assertTrue(rs.next());

        long persistedPCVal = rs.getLong(1);
        assertEquals(invokeAct.getUniqueId(), persistedPCVal);
        assertNull(rs.getDate(2));
        conn.close();
    }
}
