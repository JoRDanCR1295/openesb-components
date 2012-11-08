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
 * @(#)ActivityTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.activity;

import java.io.File;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URL;
import java.rmi.server.UID;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.jbi.JBIException;
import javax.management.MBeanServerFactory;
import javax.naming.InitialContext;
import javax.transaction.TransactionManager;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.Assert;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.EngineHelper;
import com.sun.jbi.engine.bpel.XmlResourceProviderPoolImpl;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestContext;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDeploymentLookup;
import com.sun.jbi.engine.bpel.util.SUArtifacts;

public class ActivityTest extends TestCase {
    private static DocumentBuilderFactory mDocBuilderFactory = null;
    private static TransformerFactory mTransformerFactory = null;
    
    static {
        mDocBuilderFactory = DocumentBuilderFactory.newInstance();
        mDocBuilderFactory.setNamespaceAware(true);
        mTransformerFactory = TransformerFactory.newInstance();
    }
    
    protected Engine mEng;
    protected EngineChannelSimulatorAdaptor mChannel;


    /** Construct test instance. */
    public ActivityTest(String name) {
        super(name);
    }

    /** Creates test suite. */
    public static TestSuite suite() {
        return new TestSuite(ActivityTest.class);
    }

    public void testForEachWithPick() throws Exception {
        runTest("bpels/ForEachWithPick/deployedFolder", 
                "ForEachWithPick",
                "bpels/ForEachWithPick/input/Input.xml",
                "bpels/ForEachWithPick/output/Output.xml",
                QName.valueOf("{http://enterprise.netbeans.org/bpel/ForEachWithPick}PartnerLink1"),
                "ForEachPLinkTypeRole_myRole", "ForEachOperation", 
                QName.valueOf("{http://enterprise.netbeans.org/wsdl/ForEachWithPick}ForEachInputMsg"));
    }
    
    protected void runTest(String deployedFolder, String testName,
                           String inputFile, final String expectedOutputFile,
                           QName service, String endpoint, String operation,
                           QName msgType) throws Exception {
        DeploymentBindings deplBindings = deploy(deployedFolder, testName);
        // send message
        String inputXML = getXMLString(inputFile);
        System.out.println("sending message "+ ":\n");
        System.out.println(inputXML);
        Document doc = getDocument(inputFile);

//        QName service = QName.valueOf("{urn:assign:assignService}caller");
//        String endPoint = "caller_myRole";
//        String oper = "assign";
//        QName ipMsgType = QName.valueOf("{urn:assign:assignService}AssignMessageType");
//        QName opMsgType =  QName.valueOf("{urn:assign:assignService}AssignMessageType");

        InComingKey key = deplBindings.createInComingBindingsKey(
                service, endpoint, operation);
        final InComingEventModel model = deplBindings.getInComingEventModel(key);
        Message wsdlMessage = model.getBPELProcess().getWSDLMessage(msgType);

        JBIMessageImpl jbiMsg = new JBIMessageImpl(doc, wsdlMessage);
        MessageContainer container = 
                MessageContainerFactory.createMessage("TestID", jbiMsg, 
                                                      null, null);
        mChannel = new EngineChannelSimulatorAdaptor() {
            private boolean hasInvoked = false;
            public void reply(MessageContainer msgContainer) {
                acceptReply(msgContainer, model, 
                            expectedOutputFile);
            }

            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
                                 QName operation, boolean oneWay, RBPELProcess process) {
                if (hasInvoked) {
                    fail("Invoke should not happen more than once!");
                }
                else {
                    hasInvoked = true;
                }
                return acceptInvoke(msgContainer, model, expectedOutputFile);
            }

        };
        mEng.setOutChannel(mChannel);
        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
        //mEng.processRequest(container, model);
        mEng.process(event, container);
        // receive reply
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        Properties connProp = new Properties();
        InputStream is = 
                getClass().getResourceAsStream(
                        "/com/sun/jbi/engine/bpel/jbiadaptor/test/common/bpelse2.properties");
        connProp.load(is);

        DummyTxManagerAndDataSource dummyTMAndDS = 
                new DummyTxManagerAndDataSource(connProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(connProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ic = 
                new TestContext(dummyNonXATMAndDS, dummyTMAndDS, connProp);
        mEng = new EngineImpl(connProp, ic);
        mEng.preStart();
        registerXmlResourceProviderPool();
    }

    private void registerXmlResourceProviderPool(){
        //Register a XmlResourceProviderPool with the Registry. We use the name of
        //the interface XmlResourceProviderPool to register the pool. We create only
        //one object in the pool, since this is a single threaded environment.
        BPELSERegistry registry = BPELSERegistry.getInstance();

        XmlResourceProviderPoolImpl xmlResProviderPool;
        try {
            xmlResProviderPool = new XmlResourceProviderPoolImpl(1);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        registry.register(XmlResourceProviderPool.class.getName(), xmlResProviderPool);
    }

    /** @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        mEng = null;
        super.tearDown();
    }

    protected DeploymentBindings deploy(String filePath, String testName) throws Exception {
//        String filePath = "bpels/firstAssignTest/deployedFolder";
        URL url = getClass().getResource(filePath);
        File deployDir = new File(url.toURI());

        StatusProviderHelper statusProviderHelper = null;
        try {
            statusProviderHelper = new StatusProviderHelper("BPELSE Status",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE,
                    "Bpel_engine", MBeanServerFactory
                            .createMBeanServer());
            statusProviderHelper.registerMBean();
        } 
        catch (Exception ex) {
            throw new JBIException("Failed to register status provider MBean",
                                   ex);
        }

        SUArtifacts suArtifacts = EngineHelper.deploy(testName, deployDir,
                new TestDeploymentLookup());

        Iterator bpItr = suArtifacts.getBPs().iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            mEng.addModel(bpelProcess, testName, testName);
        }
        
        DeploymentBindings deplBindings = new DeploymentBindings();
        Set inEventModelSet = suArtifacts.getInComingEventModel().entrySet();
        Iterator inEventModelIter = inEventModelSet.iterator();
        while (inEventModelIter.hasNext()) {
            Map.Entry inEventModelMapEntry = (Map.Entry) inEventModelIter.next();
            InComingKey key = (InComingKey) inEventModelMapEntry.getKey();
            InComingEventModel model = (InComingEventModel) inEventModelMapEntry.getValue();
            deplBindings.addInComingEventModel(testName, key, model);
            mEng.addStartActivityModel(model.getBPELProcess(), 
                    model.getStartElement(), model.getOperPattern());
        }
        
        return deplBindings;
    }
    
    protected void compareOutput(MessageContainer container, 
                                 String expectedOutputFile) throws Exception {
        JBIMessageImpl jbiMsg = (JBIMessageImpl) container.getContent();
        String receivedXML = getXMLString(jbiMsg.getElement());
        System.out.println("received message"+ ":\n");
        System.out.println(receivedXML);
//            Document receivedDoc = jbiMsg.getElement().getOwnerDocument();

        String expectedOutputXML = getXMLString(expectedOutputFile);
        System.out.println("Expected output: \n");
        System.out.println(expectedOutputXML);
//            Document expectedDoc = getDocument(expectedOutputFile);

        DocumentBuilderFactory docFactory =  DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        XMLUnit.setTestDocumentBuilderFactory(docFactory);
        XMLUnit.setControlDocumentBuilderFactory(docFactory);
        XMLUnit.setIgnoreWhitespace(true);

        Diff diff = XMLUnit.compare(expectedOutputXML, receivedXML);
        assertTrue(diff.similar());
/*          TODO FIXME the following is also a good comparision test that we should enable.
 *          But for the time being, i think because of a bug in XMLUnit, this fails.
 *          Revisit this later.
 *
            Diff docDiff = new Diff(expectedDoc, receivedDoc);
            System.out.println(diff.toString());
            DetailedDiff docDiff2 = new DetailedDiff(docDiff);
            System.out.println(docDiff2.getAllDifferences());
            assertTrue(docDiff.similar());
*/
    }
    
    protected String acceptInvoke(MessageContainer container, 
                                  InComingEventModel model,
                                  String expectedOutputFile) {
        try {
            compareOutput(container, expectedOutputFile);
            String messageExchangeId = new UID().toString();
            MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(messageExchangeId, null, null);
            //This is used only for one way invokes.
            statusContainer.setTransaction(container.getTransaction());
            InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(model.getBPELProcess(), Event.DONE, messageExchangeId);
            mEng.process(event, statusContainer);
            //mEng.processDone(statusContainer, model.getBPELProcess());
            return messageExchangeId;
            
        } catch (Exception e) {
            String messageExchangeId = new UID().toString();
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId, e.getMessage(), null, null);
            InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(model.getBPELProcess(), Event.ERROR, messageExchangeId);
            mEng.process(event, statusContainer);
            //mEng.processDone(statusContainer, model.getBPELProcess());
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;
        }
    }
    
    protected void acceptReply(MessageContainer container, 
                               InComingEventModel model,
                               String expectedOutputFile) {
        try {
            compareOutput(container, expectedOutputFile);
            MessageContainer statusContainer = 
                    MessageContainerFactory.createDoneStatus("TestID", null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
            mEng.process(event, statusContainer);
            //mEng.processDone(statusContainer, model.getBPELProcess());
        } 
        catch (Exception e) {
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus("TestID",
                    e.getMessage(), null,  null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.ERROR, statusContainer.getId()); 
            mEng.process(event, statusContainer);
            //mEng.processDone(statusContainer, model.getBPELProcess());
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    protected String getXMLString(String msgFilePath) throws Exception {
//        String msgFilePath = "bpels/firstAssignTest/" + fileName;
        URL ipMsgFileURL = getClass().getResource(msgFilePath);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        Document doc = mDocBuilderFactory.newDocumentBuilder().parse(ipMsgFile);
        Element elem = doc.getDocumentElement();

        DOMSource src = new DOMSource(elem);
        Transformer transformer = mTransformerFactory.newTransformer();
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);

        transformer.transform(src, dest);
        return writer.toString();
    }

    protected Document getDocument(String msgFilePath) throws Exception {
        URL ipMsgFileURL = getClass().getResource(msgFilePath);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        Document doc = mDocBuilderFactory.newDocumentBuilder().parse(ipMsgFile);

        return doc;
    }

    protected String getXMLString(Element elem) throws Exception {
        DOMSource src = new DOMSource(elem);
        Transformer transformer = mTransformerFactory.newTransformer();
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);

        transformer.transform(src, dest);
        return writer.toString();
    }
}
