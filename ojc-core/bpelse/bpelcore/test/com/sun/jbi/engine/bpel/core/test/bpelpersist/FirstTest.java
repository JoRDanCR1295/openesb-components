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
 * @(#)FirstTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.io.BufferedReader;
import java.io.InputStream;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.bpel.model.Variables;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.test.common.DummyRBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import javax.wsdl.Definition;
import javax.wsdl.Message;


/** simple Receive - Assign - Invoke test case.
 * Test cases for this needs to be written
 * Receive - Assign - Flow - Receive1 / Receive2 - (1W)Invoke
 * @author Sun Microsystems
 */
public class FirstTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.test.bpelpersist.FirstTest");
    //static String mBPELId1 = new UID().toString();
    static QName mBPELId1 = QName.valueOf(new UID().toString());
    static String mBPELId2 = new UID().toString();
    static byte[] mBPELId1_blob = "Receive - Assign - (1W)Invoke".getBytes();
    static byte[] mBPELId2_blob = "Receive - Assign - Flow - Receive1 / Receive2 - (1W)Invoke".getBytes();
    
    protected RBPELProcess mProcess;

    public FirstTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        mProcess = new DummyRBPELProcess(mBPELId1);
        mEng.addModel(mProcess, null, null);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        mEng.removeModel(mBPELId1);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FirstTest.class);

        return suite;
    }

    // test1 is for Receive - Assign - (1way)Invoke. The following is the ID map.

    /* bpelProcess - -2 (default).
     * branchId - -1 (defualt main branch id)
     * sequence - 1
     * receive - 2
     * assign - 3
     * invoke - 4
     * var1 (in from receive) - 5
     * var2 (out to invoke) - 6
     *
     */
    public void test1() throws Exception {
        StateImpl state = createState(mProcess.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);

        RuntimeVariable var1 = createVariable("request");
        long varID = var1.getVariableDef().getUniqueId();
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var1);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            System.out.println("Connection: " + conn);

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
            assertEquals(2, persistedPCVal);

            // verify variable insert
            rs = stmt.executeQuery("select varid, value from VARIABLE where stateid = '" +
                    stateId + "'");
            assertTrue(rs.next());

            long persistedVarName = rs.getLong(1);
            assertEquals(varID, persistedVarName);

            String document = null;
            if( dbType == ConnectionProperties.POSTGRES_DB ){
                document = rs.getString(2);
            } else {
                Clob clobVal = rs.getClob(2);
                document = clobVal.getSubString(1, (int) clobVal.length());
            }

            WSDLDocument doc = loadWSDL("data/assign.wsdl");
            Definition defn = doc.getDefinition();
            QName qName = new QName(defn.getTargetNamespace(),
            "AssignMessageType");
            Message msg = defn.getMessage(qName);
            WSMessage msgVal = new JBIMessageImpl(DOMHelper.createDOM(document).getOwnerDocument(), msg);
            assertTrue(msgVal != null);
            
            conn.close();
        }

        // simulating the assign
        RuntimeVariable var2 = createVariable("response");
        long varID2 = var2.getVariableDef().getUniqueId();
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var2);
        
        // simulating the invoke
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 4);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify lastcheckpoint insert
            ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                    "where stateid = '" + stateId + "'");
            assertTrue(rs.next());

            long persistedPCVal = rs.getLong(1);
            assertEquals(4, persistedPCVal);

            // verify variable insert
            rs = stmt.executeQuery("select varid from VARIABLE where stateid = '" +
                    stateId + "' AND varid != " + varID);
            assertTrue(rs.next());

            long persistedVarName = rs.getLong(1);
            assertEquals(varID2, persistedVarName);
            conn.close();
        }

        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify state objects state.
            ResultSet rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId +
                    "'");
            System.out.println("statement: " +
                "select status from STATE where id = '" + stateId + "'");
            System.out.println("resultset: " + rs);
            assertTrue(rs.next());

            String persistedStatusVal = rs.getString(1);
            assertEquals("DONE", persistedStatusVal);
            conn.close();
        }
    }

    // contains the BPIds that are created by 
    //private static List test2_test3_correlation = new ArrayList();

    /** test2() and test3() go hand in hand. test2() simulates the crash and
     *  test3() recovers the BP crashed by test2().
     * same as test1() but then this simulates a crash after assign.
     */
    public void test2() throws Exception {
        StateImpl state = createState(mProcess.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);

        RuntimeVariable var1 = createVariable("request");
        long varID = var1.getVariableDef().getUniqueId();
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var1);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
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
            assertEquals(2, persistedPCVal);

            // verify variable insert
            rs = stmt.executeQuery("select varid from VARIABLE where stateid = '" +
                    stateId + "'");
            assertTrue(rs.next());

            long persistedVarName = rs.getLong(1);
            assertEquals(varID, persistedVarName);
            conn.close();
        }
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        //test2_test3_correlation.add(stateId);
    }

    // test1 is for Receive - Assign - (1way)Invoke. The following is the ID map.

    /* bpelProcess - -2 (default).
     * branchId - -1 (defualt main branch id)
     * sequence - 1
     * receive - 2
     * assign - 3
     * invoke - 4
     * var1 (in from receive) - 5
     * var2 (out to invoke) - 6
     *
     */
    public void testVariableContent() throws Exception {
        StateImpl state = createState(mProcess.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);

        RuntimeVariable var1 = createVariable("request");
        long varID = var1.getVariableDef().getUniqueId();
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var1);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
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
            assertEquals(2, persistedPCVal);

            // verify variable insert
            rs = stmt.executeQuery("select varid from VARIABLE where stateid = '" +
                    stateId + "'");
            assertTrue(rs.next());

            long persistedVarName = rs.getLong(1);
            assertEquals(varID, persistedVarName);
            conn.close();
        }

        // simulating the assign
        RuntimeVariable var2 = createVariable("response");
        long varID2 = var2.getVariableDef().getUniqueId();
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var2);
        
        // simulating the invoke
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 4);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();

            // verify lastcheckpoint insert
            ResultSet rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                    "where stateid = '" + stateId + "'");
            assertTrue(rs.next());

            long persistedPCVal = rs.getLong(1);
            assertEquals(4, persistedPCVal);

            // verify variable insert
            rs = stmt.executeQuery("select varid from VARIABLE where stateid = '" +
                    stateId + "' AND varid != " + varID);
            assertTrue(rs.next());

            long persistedVarName = rs.getLong(1);
            assertEquals(varID2, persistedVarName);
            conn.close();
        }

        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // one more persistent point here, that either deletes or marks the instance
        // for deletion
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
            assertEquals("DONE", persistedStatusVal);
            conn.close();
        }
    }

//    private StateImpl createState() {
//        String bpId = new UID().toString();
//
//        return (StateImpl) StateFactory.getStateFactory().createState(mEng, mBPELId1, bpId);
//    }

    RuntimeVariable createVariable(String varName) {
        RBPELProcess process = loadBPELModel("assign.bpel");
        Variables vars = process.getVariables();
        RVariable var = (RVariable) vars.getVariable(varName);
        // assuming that the scopeId of this variable is in the process level
        // as seen from the bpel doc.
        String scopeId = FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID;
        RuntimeVariable retVal = new RuntimeVariableImpl(var, null, scopeId);
        WSMessage msg = constructMsg();
        msg.addInternalReference(var);
        retVal.setWSMessage(msg);

        return retVal;
    }

    private WSMessage constructMsg() {
        String dataFilePath = "data/" + "assign.xml";
        InputSource ipSource = null;
        WSMessage domMsg = null;

        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);

            //Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(ipSource);
            Element elem = Utility.createDOMElement(ipSource);

            WSDLDocument doc = loadWSDL("data/assign.wsdl");
            Definition defn = doc.getDefinition();

            QName qName = new QName(defn.getTargetNamespace(),
                    "AssignMessageType");

            //domMsg = new JBIMessageImpl(doc, msg);
            domMsg = new JBIMessageImpl(elem.getOwnerDocument(), defn.getMessage(qName));
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return domMsg;
    }

}
