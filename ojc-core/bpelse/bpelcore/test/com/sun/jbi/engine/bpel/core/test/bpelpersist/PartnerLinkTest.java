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

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

import javax.sql.rowset.serial.SerialClob;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.bpel.model.BPELProcessOrScope;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimePartnerLinkImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimeVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.Utility;

/**
 * @author Sun Inc
 * Jul 11, 2006
 */
public class PartnerLinkTest extends AbstractTestCase {
    static QName mBPELId = QName.valueOf(new UID().toString());
    
    /**
     * @param testName
     */
    public PartnerLinkTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();

        // remove BPELs and inserted instance data??
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(PartnerLinkTest.class);

        return suite;
    }

    
    public void testInternalEPR() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInternalEPR");
    
        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        mEng.addModel(process, null, null);
    
        int pLinksSize = process.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 3);
    
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
    
        RuntimeVariable var1 = createVariable();
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
                
        if (true) {
            RPartnerLink pLink = (RPartnerLink) process.getPartnerLinks().getPartnerLink(0);
            RuntimePartnerLink rPLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
            assertTrue(rPLink.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
    
            // simulating a second persistence point where the partner link is changed and the PC updated
            state.updatePartnerLink(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rPLink);
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 3);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();
    
            // verify partnerlink insert
            ResultSet rs = stmt.executeQuery("select * from PARTNERLINK where stateid = '" +
                    stateId + "' and plinkid = " + pLink.getUniqueId());
            assertTrue(rs.next());
            
            Reader rdr = null;
            if (dbType == ConnectionProperties.POSTGRES_DB ){
                String content = rs.getString(3);
                rdr = new StringReader(content);
            } else {
                SerialClob content = new SerialClob(rs.getClob(3));
                rdr = content.getCharacterStream();
            }
            RuntimePartnerLink rPLink2 = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
            assertTrue(rPLink2.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
            
            rPLink2.setSerializedValue(rdr);
            
            assertTrue(rPLink2.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
            
            conn.close();
        }
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testInternalEPR"); 
    }

    public void testDocFragment() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testDocFragment");
    
        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        mEng.addModel(process, null, null);
    
        int pLinksSize = process.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 3);
    
        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
    
        RuntimeVariable var1 = createVariable();
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
                
        BPELProcessOrScope scope = (BPELProcessOrScope) HelperFunc.getScopeByName(process, "Scope1");
        
        if (true) {
            // second partnerlink with a service reference (Document fragment) value.
            // The document fragment has namespaces that are not really associated with
            // the BPEL or WSDL, but for this test case it doesn't matter.
            DocumentFragment frag = constructDocFrag();
            pLinksSize = scope.getPartnerLinks().getPartnerLinksSize();
            RPartnerLink pLink = (RPartnerLink) process.getPartnerLinks().getPartnerLink(1);
            RuntimePartnerLink rPLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
            rPLink.setServiceRef(frag);
            assertTrue(rPLink.getServiceRef() instanceof DocumentFragment);
    
            state.updatePartnerLink(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rPLink);
            state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 4);
            mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    
            // scoping variables. No other reason for this if.
            Connection conn = getConnection();
            Statement stmt = conn.createStatement();
    
            // verify partnerlink insert
            ResultSet rs = stmt.executeQuery("select * from PARTNERLINK where stateid = '" +
                    stateId + "' and plinkid = " + pLink.getUniqueId());
            assertTrue(rs.next());
    
            Reader rdr = null;
            if (dbType == ConnectionProperties.POSTGRES_DB ){
                String content = rs.getString(3);
                rdr = new StringReader(content);
            } else {
                SerialClob content = new SerialClob(rs.getClob(3));
                rdr = content.getCharacterStream();
            }
            RuntimePartnerLink rPLink2 = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
            assertTrue(rPLink2.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
    
            rPLink2.setSerializedValue(rdr);
    
            assertTrue(rPLink2.getServiceRef() instanceof DocumentFragment);
    
            conn.close();
        }
        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testDocFragment"); 
    }

    public void testInternalEPRFromAnotherPLink() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInternalEPRFromAnotherPLink");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        mEng.addModel(process, null, null);

        int pLinksSize = process.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 3);

        StateImpl state = createState(process.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);

        RuntimeVariable var1 = createVariable();
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // copy myRole's EPR into some other partnerlink's partnerRole and persist and recover.
        RPartnerLink partnerRolePLink = null;
        RPartnerLink myRolePLink = null;
        for (Object parLink : process.getPartnerLinks().getPartnerLinks()) {
            if ("PartnerLink3".equals(((RPartnerLink) parLink).getName())) {
                partnerRolePLink = (RPartnerLink) parLink;
            } else if ("PartnerLink2".equals(((RPartnerLink) parLink).getName())) {
                myRolePLink = (RPartnerLink) parLink;
            }
        }

        Object myEPR = RuntimePartnerLinkImpl.getEPR(myRolePLink, true);

        RPartnerLink pLink = partnerRolePLink;
        RuntimePartnerLink rPLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
        assertTrue(rPLink.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
        QName serviceName = QName.valueOf("{http://enterprise.netbeans.org/bpel/BPELModelJUnitTest/PartnerLinkModelTest}PartnerLink3");
        assertTrue (serviceName.equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getService()));
        assertTrue("newWSDLPortTypeRole_partnerRole".equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getEndPoint()));

        rPLink.setServiceRef(myEPR);

        QName serviceName2 = QName.valueOf("{http://enterprise.netbeans.org/bpel/BPELModelJUnitTest/PartnerLinkModelTest}PartnerLink2");
        assertTrue (serviceName2.equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getService()));
        assertTrue("newWSDLPortTypeRole_myRole".equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getEndPoint()));

        state.updatePartnerLink(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, rPLink);
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 3);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // scoping variables. No other reason for this if.
        Connection conn = getConnection();
        Statement stmt = conn.createStatement();

        // verify partnerlink insert
        ResultSet rs = stmt.executeQuery("select * from PARTNERLINK where stateid = '" +
                stateId + "' and plinkid = " + pLink.getUniqueId());
        assertTrue(rs.next());

        Reader rdr = null;
        if (dbType == ConnectionProperties.POSTGRES_DB ){
            String content = rs.getString(3);
            rdr = new StringReader(content);
        } else {
            SerialClob content = new SerialClob(rs.getClob(3));
            rdr = content.getCharacterStream();
        }
        RuntimePartnerLink rPLink2 = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
        assertTrue(rPLink2.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);

        rPLink2.setSerializedValue(rdr);

        assertTrue(rPLink2.getServiceRef() instanceof RuntimePartnerLink.InternalEPR);
        QName serviceName3 = QName.valueOf("{http://enterprise.netbeans.org/bpel/BPELModelJUnitTest/PartnerLinkModelTest}PartnerLink2");
        assertTrue (serviceName3.equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getService()));
        assertTrue("newWSDLPortTypeRole_myRole".equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getEndPoint()));

        assertTrue(rPLink2.getServiceRef().toString().equals(myEPR.toString()));
        
        conn.close();

        
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testInternalEPRFromAnotherPLink"); 
    }
    
    private RuntimeVariable createVariable() {
        String varName = "NewWSDLOperationIn";
        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        Variables vars = process.getVariables();
        RVariable var = (RVariable) vars.getVariable(varName);
        RuntimeVariable retVal = new RuntimeVariableImpl(var, null, FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        WSMessage msg = constructMsg();
        msg.addInternalReference(var);
        retVal.setWSMessage(msg);

        return retVal;
    }

    private WSMessage constructMsg() {
        String dataFilePath = "bpel/partnerlink/PartnerLinkModelTestInput.xml";
        InputSource ipSource = null;
        WSMessage domMsg = null;

        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);

            Element elem = Utility.createDOMElement(ipSource);

            WSDLDocument wsdlDoc = loadWSDL("bpel/partnerlink/newWSDL.wsdl");
            Definition defn = wsdlDoc.getDefinition();

            QName qName = new QName(defn.getTargetNamespace(),
                    "newWSDLOperationRequest");

            domMsg = new JBIMessageImpl(elem.getOwnerDocument(), defn.getMessage(qName));
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return domMsg;
    }

    private DocumentFragment constructDocFrag() {
        String dataFilePath = "bpel/partnerlink/serviceRef.xml";
        InputSource ipSource = null;
        DocumentFragment frag = null;        
        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);

            Element elem = Utility.createDOMElement(ipSource);
            frag = elem.getOwnerDocument().createDocumentFragment();
            frag.appendChild(elem);
            
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }
        return frag;
    }
    
//    private StateImpl createState() {
//        String bpId = new UID().toString();
//
//        return (StateImpl) StateFactory.getStateFactory().createState(mEng, mBPELId, bpId);
//    }
}
