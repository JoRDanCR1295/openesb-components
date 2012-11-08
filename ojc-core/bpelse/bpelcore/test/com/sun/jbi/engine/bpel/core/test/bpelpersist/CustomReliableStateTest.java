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
 * @(#)CustomReliableStateTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.bpel.model.Variables;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.test.common.DummyRBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import com.sun.wsdl4j.ext.WSDL4JExt;

import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;

import com.sun.bpel.xml.wsdl.WSDLDocument;


/**
 * @author Sun Microsystems
 */
public class CustomReliableStateTest extends AbstractTestCase {
    //static String mBPELId1 = new UID().toString();
    static QName mBPELId1 = QName.valueOf(new UID().toString());
    
    protected RBPELProcess mProcess;

    public CustomReliableStateTest(String testName) {
        super(testName);
    }
    protected void setUp() throws Exception {
        mEng = OneTimesetUp.getSetUp().getEngine();
        mEngId = mEng.getId();
        mConnProp = OneTimesetUp.getSetUp().getConnectionProperties();
        mStateMgr = mEng.getStateManager();
        mProcess = new DummyRBPELProcess(mBPELId1);
        mEng.addModel(mProcess, null, null);
    }

    protected void tearDown() throws Exception {
        mEng.removeModel(mProcess.getBPELId());
    }
    
    /** Simulating and testing a invoke and a non-invoke activity
     * @throws Exception
     */
    public void testCustomReliableInvoke() throws Exception {
        Connection conn = null;
        Statement stmt = null;
        ResultSet rs = null;
        try {
            StateImpl state = createState(mProcess.getBPELId());
            String stateId = state.getId();
            //state.enterScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
            state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
   
            // simulating a non-invoke activity (receive)
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
   
            RuntimeVariable var1 = createVariable("request");
            long varID = var1.getVariableDef().getUniqueId();
   
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
                var1);
   
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
   
            if (true) {
                // scoping variables. No other reason for this if.
   
                // verify state objects state.

                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                String persistedStatusVal = rs.getString(1);
                assertEquals("RUNNING", persistedStatusVal);
                
                // verify lastcheckpoint insert
                rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedPCVal = rs.getLong(1);
                assertEquals(2, persistedPCVal);
                
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(LastCheckPointDBO.DEFAULT_BRANCH_INVOKE_COUNTER, 
                        persistedBranchInvokeCounter);
   
            }
   
            // simulating a invoke activity
            state.updatePCWithBranchInvokeCounter(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2, 
                    12);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
            if (true) {
                // scoping variables. No other reason for this if.
                // verify lastcheckpoint insert
   
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());
                
                String persistedStatusVal = rs.getString(1);
                assertEquals("RUNNING", persistedStatusVal);
                
                // verify lastcheckpoint insert
                rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedPCVal = rs.getLong(1);
                assertEquals(2, persistedPCVal);
                
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(persistedBranchInvokeCounter, 12);
            }
   
            //state.exitScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
            state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
   
            // one more persistent point here, that either deletes or marks the instance
            // for deletion
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
   
            if (true) {
                // scoping variables. No other reason for this if.
                // verify state objects state.
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());
                
                String persistedStatusVal = rs.getString(1);
                assertEquals("DONE", persistedStatusVal);
            }
        } catch (SQLException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } catch (Exception ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } finally {
        	if (stmt != null) stmt.close();
        	if (rs != null) rs.close();
        	if (conn != null) conn.close();
        }
    }

    /** Simulating and testing a receive and reply pair from a custom reliable 
     * protocol perspective
     * @throws Exception
     */
    public void testCustomReliableReceiveReply() throws Exception {
        StateImpl state = createState(mProcess.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);

        String crmpInvId = BPELHelper.getGUID();
        String partnerLink = "partnerLink";
        String operation = "operation";
        String bpelMesgExchange = "bpelME";
        
        state.updateCRMPState(crmpInvId, partnerLink, operation, bpelMesgExchange);
        
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = null;
            Statement stmt = null;

            // verify state objects state.
            ResultSet rs = null;

            try {
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());

                String persistedStatusVal = rs.getString(1);
                assertEquals("RUNNING", persistedStatusVal);

                // verify lastcheckpoint insert
                rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());

                long persistedPCVal = rs.getLong(1);
                assertEquals(2, persistedPCVal);

                rs = stmt.executeQuery("select * from CRMP where stateid = '" 
                        + stateId + "'");
                assertTrue(rs.next());
                String persistedCrmpinvokeid = rs.getString(2);
                String persistedPartnerlink = rs.getString(3);
                String persistedOperation = rs.getString(4);
                String persistedBpelmessageexchange = rs.getString(5);
                assertEquals(crmpInvId, persistedCrmpinvokeid);
                assertEquals(partnerLink, persistedPartnerlink);
                assertEquals(operation, persistedOperation);
                assertEquals(null, persistedBpelmessageexchange);

            } finally {
                stmt.close();
                rs.close();
                conn.close();
            }
        }

        // simulating a reply activity
        RuntimeVariable var1 = createVariable("request");
        long replyVarId = var1.getVariableDef().getUniqueId();

        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
            var1);
        
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 3);
        Object serialVar = var1.getSerializedValue(); 
        
        state.updateCRMPState(partnerLink, operation, bpelMesgExchange, 
                replyVarId, serialVar.toString().toCharArray());
        
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        if (true) {
            // scoping variables. No other reason for this if.
            // verify lastcheckpoint insert
            Connection conn = null;
            ResultSet rs = null;
            Statement stmt = null;

            try {
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());

                String persistedStatusVal = rs.getString(1);
                assertEquals("RUNNING", persistedStatusVal);

                // verify lastcheckpoint insert
                rs = stmt.executeQuery("select activityid from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());

                long persistedPCVal = rs.getLong(1);
                assertEquals(3, persistedPCVal);

                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());

                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(LastCheckPointDBO.DEFAULT_BRANCH_INVOKE_COUNTER, persistedBranchInvokeCounter);
                
                rs = stmt.executeQuery("select * from CRMP where stateid = '" 
                        + stateId + "'");
                assertTrue(rs.next());
                String persistedCrmpinvokeid = rs.getString(2);
                String persistedPartnerlink = rs.getString(3);
                String persistedOperation = rs.getString(4);
                String persistedBpelmessageexchange = rs.getString(5);
                long persistedReplyVarId = rs.getLong(6);
                
                assertEquals(crmpInvId, persistedCrmpinvokeid);
                assertEquals(partnerLink, persistedPartnerlink);
                assertEquals(operation, persistedOperation);
                assertEquals(null, persistedBpelmessageexchange);
                assertEquals(replyVarId, persistedReplyVarId);
                
            } finally {
                stmt.close();
                rs.close();
                conn.close();
            }
        }

        //state.exitScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        if (true) {
            // scoping variables. No other reason for this if.
            Connection conn = null;
            Statement stmt = null;

            // verify state objects state.
            ResultSet rs = null;

            try {
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());

                String persistedStatusVal = rs.getString(1);
                assertEquals("DONE", persistedStatusVal);
            } finally {
                stmt.close();
                rs.close();
                conn.close();
            }
        }
    }

//    private StateImpl createState() {
//        String bpId = new UID().toString();
//
//        return (StateImpl) StateFactory.getStateFactory().createState(mEng, mBPELId1, bpId);
//    }

    private RuntimeVariable createVariable(String varName) {
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

            String wsdlFilePath = "data/" + "assign.wsdl";
            URL wsdlURL = getClass().getResource(wsdlFilePath);
            URI uri = wsdlURL.toURI();
            
            WSDLReader wsdlReader = WSDL4JExt.newWSDLReader(null);
            Definition defn = wsdlReader.readWSDL(new File(uri).getAbsolutePath());

            QName qName = new QName(defn.getTargetNamespace(),
                    "AssignMessageType");

            //domMsg = new JBIMessageImpl(doc, msg);
            domMsg = new JBIMessageImpl(defn.getMessage(qName));
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return domMsg;
    }
    
    /** Simulating and testing Receive, Invoke, Receive, Invoke test case for Branch ID value
     * @throws Exception
     */
    public void testCustomReliableRecInvRecInv() throws Exception {
        Connection conn = null;
        Statement stmt = null;
        ResultSet rs = null;
        try {
            StateImpl state = createState(mProcess.getBPELId());
            String stateId = state.getId();
            //state.enterScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
            state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
            // simulating a non-invoke activity (receive)
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
    
            RuntimeVariable var1 = createVariable("request");
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
                var1);
    
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    
            if (true) {
                // scoping variables. No other reason for this if.
    
                // verify state objects state.
    
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(LastCheckPointDBO.DEFAULT_BRANCH_INVOKE_COUNTER, 
                        persistedBranchInvokeCounter);
    
            }
    
            // simulating a invoke activity
            long branchInvokeCounter = 12;
            state.updatePCWithBranchInvokeCounter(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2, 
                    branchInvokeCounter);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
            if (true) {
                // scoping variables. No other reason for this if.
                // verify lastcheckpoint insert
    
                conn = getConnection();
                stmt = conn.createStatement();
                
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(persistedBranchInvokeCounter, branchInvokeCounter);
            }
    
            // simulating a non-invoke activity (receive)
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
    
            RuntimeVariable var2 = createVariable("request");
    
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(),
                var2);
    
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    
            if (true) {
                // scoping variables. No other reason for this if.
                // verify state objects state.
    
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(persistedBranchInvokeCounter, branchInvokeCounter);    
            }
    
            // simulating a invoke activity
            long branchInvokeCounter2 = 13;
            state.updatePCWithBranchInvokeCounter(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2, 
                    branchInvokeCounter2);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
            if (true) {
                // scoping variables. No other reason for this if.
                // verify lastcheckpoint insert
    
                conn = getConnection();
                stmt = conn.createStatement();
                
                rs = stmt.executeQuery("select branchinvokecounter from LASTCHECKPOINT " +
                        "where stateid = '" + stateId + "'");
                assertTrue(rs.next());
                
                long persistedBranchInvokeCounter = rs.getLong(1);
                assertEquals(persistedBranchInvokeCounter, branchInvokeCounter2);
            }
            
            //state.exitScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
            state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
            // one more persistent point here, that either deletes or marks the instance
            // for deletion
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    
            if (true) {
                // scoping variables. No other reason for this if.
                // verify state objects state.
                conn = getConnection();
                stmt = conn.createStatement();
                rs = stmt.executeQuery("select status from STATE where stateid = '" +
                        stateId + "'");
                assertTrue(rs.next());
                
                String persistedStatusVal = rs.getString(1);
                assertEquals("DONE", persistedStatusVal);
            }
        } catch (SQLException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } catch (Exception ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } finally {
            if(stmt != null) stmt.close();
            if(rs != null) rs.close();
            if(conn != null) conn.close();
        }
    }
}
