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
 * @(#)XSDVariableTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
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
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import javax.wsdl.Definition;

/**
 * @author Sun Inc
 * Jul 11, 2006
 */
public class XSDVariableTest extends AbstractTestCase {

    String processScopeId;
	/**
     * @param testName
     */
    public XSDVariableTest(String testName) {
        super(testName);
        // assuming that the scopeId of this variable is in the process level
        // as seen from the bpel doc.
        processScopeId = FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID;
        
    }
    
    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();

        // remove BPELs and inserted instance data??
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(XSDVariableTest.class);

        return suite;
    }
    
    public void testXSDSimpleType() throws Exception {
        
        RBPELProcess process = loadBPELModel("XSDVariables_SimpleTypes/xsddatatype_simpleTypes1.bpel");
        mEng.addModel(process, null, null);
        Variables vars = process.getVariables();
        RVariable var = (RVariable) vars.getVariable("xsdDataType_SimpleType1");
        RuntimeVariable rVar = new RuntimeVariableImpl(var, null, processScopeId);
        
        String dataFilePath = "bpel/XSDVariables_SimpleTypes/data/sdtData.xml";
        Element elem = getRootElement(dataFilePath);
        rVar.setXSDVariableData(elem.getTextContent());
        
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVar);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // verifying the persisted variable
        Connection conn = getConnection();

        Statement stmt = conn.createStatement();
        // verify variable insert
        ResultSet rs = stmt.executeQuery("select varid, stringvalue from SIMPLEVARIABLE where stateid = '" +
                stateId + "'");
        assertTrue(rs.next());

        long persistedVarName = rs.getLong(1);
        assertEquals(var.getUniqueId(), persistedVarName);

        /*
         * xsd simple type vars are not stored as clob anymore
         * 
        Clob clobVal = rs.getClob(2);
        BufferedReader reader = new BufferedReader(clobVal.getCharacterStream());
        char[] charArrayVal = new char[(int) clobVal.length()];
        reader.read(charArrayVal, 0, (int) clobVal.length());
        */
        String recoveredVal = rs.getString(2);
        conn.close();
        
        String origVal = elem.getTextContent();
        //String recoveredVal = new String(charArrayVal);
        assertTrue(origVal.equals(recoveredVal));
        // just to mark the entry as "Done" in the DB
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
    }
    
    public void testXSDComplexType() throws Exception {
        // construct the variable with the data
        RBPELProcess process = loadBPELModel("XSDVariables_Types_Elems/xsddatatype_specexample1.bpel");
        mEng.addModel(process, null, null);
        Variables vars = process.getVariables();
        RVariable var = (RVariable) vars.getVariable("XSDVariable_USAddress_CType");
        RuntimeVariable rVar = new RuntimeVariableImpl(var, null, processScopeId);
        
        // variable initialization is now deferred until use, not at construction
        com.sun.jbi.engine.bpel.core.bpel.util.Utility.initializeVariableValue(rVar);
        
        String dataFilePath = "bpel/XSDVariables_Types_Elems/data/cdtData.xml";
        Element elem = getRootElement(dataFilePath);
        
        Element persistedElem = (Element)rVar.getXSDVariableData();
        Document doc = persistedElem.getOwnerDocument();
        Node childNode = elem.getFirstChild();
        Node importedChildNode = null;
        while (childNode != null) {
            importedChildNode = doc.importNode(childNode, true);
            persistedElem.appendChild(importedChildNode);
            childNode = childNode.getNextSibling();
        }
        
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVar);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    
        // verifying the persisted variable
        Connection conn = getConnection();
    
        Statement stmt = conn.createStatement();
        // verify variable insert
        ResultSet rs = stmt.executeQuery("select varid, value from VARIABLE where stateid = '" +
                stateId + "'");
        assertTrue(rs.next());
    
        long persistedVarName = rs.getLong(1);
        assertEquals(var.getUniqueId(), persistedVarName);
    
        BufferedReader reader = null;
        if( dbType == ConnectionProperties.POSTGRES_DB ){
            reader = new BufferedReader( new StringReader(rs.getString(2)) );
        } else {
            Clob clobVal = rs.getClob(2);
            reader = new BufferedReader(clobVal.getCharacterStream());
        }
        //Element recoveredElemVal = DOMHelper.createDOM(new String(charArrayVal));
        Element recoveredElemVal = DOMHelper.readDocument(reader).getDocumentElement();
        conn.close();
        // TODO better comparision is required here. Couldn't come up with anything better and yet simple
        assertTrue(recoveredElemVal.getChildNodes().getLength() == persistedElem.getChildNodes().getLength());
        // just to mark the entry as "Done" in the DB
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
    }

    public void testXSDElement() throws Exception {
        // construct the variable with the data
        RBPELProcess process = loadBPELModel("XSDVariables_Types_Elems/xsddatatype_specexample1.bpel");
        mEng.addModel(process, null, null);
        Variables vars = process.getVariables();
        RVariable var = (RVariable) vars.getVariable("XSDVariable_Person1");
        RuntimeVariable rVar = new RuntimeVariableImpl(var, null, processScopeId);
        
        // variable initialization is now deferred until use, not at construction
        com.sun.jbi.engine.bpel.core.bpel.util.Utility.initializeVariableValue(rVar);

        String dataFilePath = "bpel/XSDVariables_Types_Elems/data/elemData.xml";
        Element elem = getRootElement(dataFilePath);
        
        Element persistedElem = (Element)rVar.getXSDVariableData();
        Document doc = persistedElem.getOwnerDocument();
        Node childNode = elem.getFirstChild();
        Node importedChildNode = null;
        while (childNode != null) {
            importedChildNode = doc.importNode(childNode, true);
            persistedElem.appendChild(importedChildNode);
            childNode = childNode.getNextSibling();
        }
        
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVar);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // verifying the persisted variable
        Connection conn = getConnection();

        Statement stmt = conn.createStatement();
        // verify variable insert
        ResultSet rs = stmt.executeQuery("select varid, value from VARIABLE where stateid = '" +
                stateId + "'");
        assertTrue(rs.next());

        long persistedVarName = rs.getLong(1);
        assertEquals(var.getUniqueId(), persistedVarName);

        String document = null;
        if(dbType == ConnectionProperties.POSTGRES_DB ){
            document = rs.getString(2);
        } else {
            Clob clobVal = rs.getClob(2);
            document = clobVal.getSubString(1, (int) clobVal.length());
        }
        conn.close();
        
        Element recoveredElemVal = DOMHelper.createDOM(document);

        // TODO better comparision is required here. Couldn't come up with anything better and yet simple
        assertTrue(recoveredElemVal.getChildNodes().getLength() == persistedElem.getChildNodes().getLength());
 
        // just to mark the entry as "Done" in the DB
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
    }
    
    /** partly negative test from an updateVariable API perspective. Because in reality
     * the RuntimeVariable object doesn't change, just it's value changes.
     * @throws Exception
     */
    public void testBatchUpdate() throws Exception {
            RBPELProcess process = loadBPELModel("assign.bpel");
            mEng.addModel(process, null, null);
            Variables vars = process.getVariables();
    
            RVariable var = (RVariable) vars.getVariable("request");
            RuntimeVariable rVar = new RuntimeVariableImpl(var, null, processScopeId);
            String dataFilePath = "data/assign.xml";
            WSMessage msg = constructMsg(dataFilePath);
            msg.addInternalReference(var);
            rVar.setWSMessage(msg);
            
            RVariable varSimpleXSD1 = (RVariable) vars.getVariable("xsdDataType_SimpleType1");
            RuntimeVariable rVarSimpleXSD1 = new RuntimeVariableImpl(varSimpleXSD1, null, processScopeId);
            dataFilePath = "bpel/batchUpdate/data/sdt1Data.xml";
            Element elem = getRootElement(dataFilePath);
            rVarSimpleXSD1.setXSDVariableData(elem.getTextContent());
            
            RVariable varSimpleXSD2 = (RVariable) vars.getVariable("xsdDataType_SimpleType2");
            RuntimeVariable rVarSimpleXSD2 = new RuntimeVariableImpl(varSimpleXSD2, null, processScopeId);
            dataFilePath = "bpel/batchUpdate/data/sdt2Data.xml";
            elem = getRootElement(dataFilePath);
            rVarSimpleXSD2.setXSDVariableData(elem.getTextContent());
            
            RVariable varSimpleXSD3 = (RVariable) vars.getVariable("xsdDataType_SimpleType3");
            RuntimeVariable rVarSimpleXSD3 = new RuntimeVariableImpl(varSimpleXSD3, null, processScopeId);
            dataFilePath = "bpel/batchUpdate/data/sdt3Data.xml";
            elem = getRootElement(dataFilePath);
            rVarSimpleXSD3.setXSDVariableData(elem.getTextContent());
    
            RVariable varSimpleXSD4 = (RVariable) vars.getVariable("xsdDataType_SimpleType4");
            RuntimeVariable rVarSimpleXSD4 = new RuntimeVariableImpl(varSimpleXSD4, null, processScopeId);
            dataFilePath = "bpel/batchUpdate/data/sdt4Data.xml";
            elem = getRootElement(dataFilePath);
            rVarSimpleXSD4.setXSDVariableData(elem.getTextContent());
    
            
            StateImpl state = createState(process.getBPELId());
            String stateId = state.getId();
            state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
            // simulating the receive
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
//            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                    rVar);
//            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                    rVarSimpleXSD1);
//            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                    rVarSimpleXSD2);
//            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                    rVarSimpleXSD3);
//            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                    rVarSimpleXSD4);
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVar);
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD1);
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD2);
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD3);
            state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD4);
            
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

            // just to mark the entry as "Done" in the DB
            state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
            mEng.removeModel(process.getBPELId());
        }

    /** partly negative test from an updateVariable API perspective. Because in reality
     * the RuntimeVariable object doesn't change, just it's value changes.
     * @throws Exception
     */
    public void testBatchUpdate2() throws Exception {
        // tests updating the variable 2 times before actually persisting it.
        RBPELProcess process = loadBPELModel("assign.bpel");
        mEng.addModel(process, null, null);
        Variables vars = process.getVariables();

        RVariable var = (RVariable) vars.getVariable("request");
        RuntimeVariable rVar = new RuntimeVariableImpl(var, null, processScopeId);
        String dataFilePath = "data/assign.xml";
        WSMessage msg = constructMsg(dataFilePath);
        msg.addInternalReference(var);
        rVar.setWSMessage(msg);
        
        RVariable varSimpleXSD1 = (RVariable) vars.getVariable("xsdDataType_SimpleType1");
        RuntimeVariable rVarSimpleXSD1 = new RuntimeVariableImpl(varSimpleXSD1, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt1Data.xml";
        Element elem = getRootElement(dataFilePath);
        rVarSimpleXSD1.setXSDVariableData(elem.getTextContent());
        
        RVariable varSimpleXSD2 = (RVariable) vars.getVariable("xsdDataType_SimpleType2");
        RuntimeVariable rVarSimpleXSD2 = new RuntimeVariableImpl(varSimpleXSD2, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt3Data.xml"; // purposefully sdt3Data.xml
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD2.setXSDVariableData(elem.getTextContent());
        
        // Negative test of the API, because a different runtimeVariable is constructed using the same
        // static variable definition
        RVariable varSimpleXSD2_1 = (RVariable) vars.getVariable("xsdDataType_SimpleType2");
        RuntimeVariable rVarSimpleXSD2_1 = new RuntimeVariableImpl(varSimpleXSD2_1, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt2Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD2_1.setXSDVariableData(elem.getTextContent());
        
        RVariable varSimpleXSD3 = (RVariable) vars.getVariable("xsdDataType_SimpleType3");
        RuntimeVariable rVarSimpleXSD3 = new RuntimeVariableImpl(varSimpleXSD3, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt3Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD3.setXSDVariableData(elem.getTextContent());
        
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                rVar);
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                rVarSimpleXSD1);
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                rVarSimpleXSD2_1); // this object will be ignored subsequently
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                rVarSimpleXSD2); // this object will be the one to be marked as "inserted"
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//                rVarSimpleXSD3);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVar);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD1);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, 
                             rVarSimpleXSD2_1); // this object will be ignored subsequently
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, 
                             rVarSimpleXSD2); // this object will be the one to be marked as "inserted"
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD3);
        
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        
        // now tests a list of variables to be inserted and a list of variables to 
        // be updated all in one go.
        RVariable varSimpleXSD4 = (RVariable) vars.getVariable("xsdDataType_SimpleType4");
        RuntimeVariable rVarSimpleXSD4 = new RuntimeVariableImpl(varSimpleXSD4, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt1Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD4.setXSDVariableData(elem.getTextContent());
      
        RVariable varSimpleXSD5 = (RVariable) vars.getVariable("xsdDataType_SimpleType5");
        RuntimeVariable rVarSimpleXSD5 = new RuntimeVariableImpl(varSimpleXSD5, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt5Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD5.setXSDVariableData(elem.getTextContent());

        RVariable varSimpleXSD3_update = (RVariable) vars.getVariable("xsdDataType_SimpleType3");
        RuntimeVariable rVarSimpleXSD3_update = new RuntimeVariableImpl(varSimpleXSD3_update, null, processScopeId);
        dataFilePath = "bpel/batchUpdate/data/sdt3Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD3_update.setXSDVariableData(elem.getTextContent());
        
        // new values and will be inserts to DB
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD4);
 
        dataFilePath = "bpel/batchUpdate/data/sdt4Data.xml";
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD4.setXSDVariableData(elem.getTextContent());
 
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD4);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD5);
        // old values and will be updates to DB
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD2);

        dataFilePath = "bpel/batchUpdate/data/sdt3Data.xml"; // purposefully sdt3Data.xml
        elem = getRootElement(dataFilePath);
        rVarSimpleXSD2.setXSDVariableData(elem.getTextContent());
        
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD2);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rVarSimpleXSD3);
        
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 3);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        
        // just to mark the entry as "Done" in the DB
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
    }
    
//    private StateImpl createState() {
//        String bpId = new UID().toString();
//        //String bpelID = new UID().toString();
//        QName bpelID = QName.valueOf(new UID().toString());
//        return (StateImpl) StateFactory.getStateFactory().createState(mEng, bpelID, bpId);
//    }
    
    private Element getRootElement(String dataFilePath) {
        InputSource ipSource = null;
        Element elem = null;
        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);
            elem = Utility.createDOMElement(ipSource);
        } catch (Exception t) {
            fail(" failed to parse the file " + t.getMessage());
        }
        return elem;
    }
    
    private WSMessage constructMsg(String dataFilePath) {
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
