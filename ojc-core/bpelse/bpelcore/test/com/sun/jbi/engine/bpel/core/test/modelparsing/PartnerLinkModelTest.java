package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.io.InputStream;
import java.io.StringWriter;
import java.net.URL;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELProcessOrScope;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.From;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.To;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELProcessManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RuntimePartnerLinkImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;

public class PartnerLinkModelTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(PartnerLinkModelTest.class.getName());
    private Engine mEng;
    
    public PartnerLinkModelTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mEng = OneTimesetUp.getSetUp().getEngine();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(PartnerLinkModelTest.class);

        return suite;
    }
    
    public void testProcessLevelPlinks() {
        Utility.logEnter(getClass().getSimpleName(), "testProcessLevelPlinks");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        
        int pLinksSize = process.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 3);

        RPartnerLink pLink = (RPartnerLink) process.getPartnerLinks().getPartnerLink(0);
        assertTrue(pLink.getAssociatedScope() == process);
        
        assertTrue(pLink.getUniqueId() != Long.MIN_VALUE);
        
        Utility.logExit(getClass().getSimpleName(), "testProcessLevelPlinks");
   }
    
    public void testScopeLevelPLinks() {
        Utility.logEnter(getClass().getSimpleName(), "testScopeLevelPLinks");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");

        BPELProcessOrScope scope = (BPELProcessOrScope) HelperFunc.getScopeByName(process, "Scope1");
        
        int pLinksSize = scope.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 4);

        RPartnerLink pLink = (RPartnerLink) scope.getPartnerLinks().getPartnerLink(0);
        assertTrue(pLink.getAssociatedScope() == scope);
        
        assertTrue(pLink.getUniqueId() != Long.MIN_VALUE);
        
        Utility.logExit(getClass().getSimpleName(), "testScopeLevelPLinks"); 
    }
    
    public void testRunTimePLinks() {
        Utility.logEnter(getClass().getSimpleName(), "testRunTimePLinks");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");
        mEng.addModel(process, null, null);
        BPELProcessManagerImpl mgr = new BPELProcessManagerImpl(process, mEng, null, null);
        BPELProcessInstance instance = new BPELProcessInstanceImpl(mgr, mEng, "123");
        
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
        .createActivityUnit((Context) instance, null, act, RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue());
        assertTrue(rootActUnit.getContext() instanceof PartnerLinkScope);
        
        RPartnerLink pLink = (RPartnerLink) process.getPartnerLinks().getPartnerLink(0);
        assertTrue(pLink.getAssociatedScope() == process);
        
        Object epr1 = new Object();
        RuntimePartnerLink runtimePLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
        runtimePLink.setServiceRef(epr1);
        rootActUnit.getContext().setRuntimePartnerLink(pLink, runtimePLink);
        assertTrue((rootActUnit.getContext().getRuntimePartnerLink(pLink).getServiceRef() == epr1));

        long rootBranchId = rootActUnit.getBranchId();
//      walk the bpel.
        Receive rec = (Receive) ((RActivityHolder) act).getChildActivity();
        ActivityUnit recUnit = ActivityUnitFactory.getInstance()
            .createActivityUnit((Context) instance, rootActUnit, (RActivity) rec, rootBranchId);
                
        Scope scope = (Scope) ((RActivity) rec).getNextActivity();
        ActivityUnit scopeUnit = ActivityUnitFactory.getInstance()
        .createActivityUnit((Context) instance, rootActUnit, (RActivity) scope, rootBranchId);
        assertTrue(scopeUnit instanceof PartnerLinkScope);
        
        RPartnerLink scopepLink1 = (RPartnerLink) scope.getPartnerLinks().getPartnerLink(0);
        RPartnerLink processpLink2 = (RPartnerLink) process.getPartnerLinks().getPartnerLink(1);
        
        Object eprScope1 = new Object();
        RuntimePartnerLink runtimePLinkScope1 = ((PartnerLinkScope) scopeUnit).createRuntimePartnerLink(pLink);
        runtimePLinkScope1.setServiceRef(eprScope1);        
        ((PartnerLinkScope) scopeUnit).setRuntimePartnerLink(scopepLink1, runtimePLinkScope1);
        
        Object eprProcess2 = new Object();
        RuntimePartnerLink runtimePLinkProcess2 = ((PartnerLinkScope) scopeUnit).createRuntimePartnerLink(pLink);
        runtimePLinkProcess2.setServiceRef(eprProcess2);        
        ((Context) scopeUnit).setRuntimePartnerLink(processpLink2, runtimePLinkProcess2);

        assertTrue(((PartnerLinkScope) instance).getRuntimePartnerLink(processpLink2).getServiceRef() == eprProcess2);
        assertTrue(((PartnerLinkScope) instance).getRuntimePartnerLink(scopepLink1) == null);
        assertTrue(((PartnerLinkScope) scopeUnit).getRuntimePartnerLink(scopepLink1).getServiceRef() == eprScope1);
        mEng.removeModel(process.getBPELId());
        Utility.logExit(getClass().getSimpleName(), "testRunTimePLinks");
    }
    
    public void testPartnerLinkAssignModelTest() {

        Utility.logEnter(getClass().getSimpleName(), "testPartnerLinkAssignModelTest");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkAssignModel.bpel");
        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "Assign1");
        Copy copy = assignActivity.getCopy(0);
        assertTrue(copy != null);
        
        From from = copy.getFrom();
        assertTrue(from != null);
        
        PartnerLink pLink = from.getBPELPartnerLink();
        assertTrue(pLink != null);
        
        String eprRef = from.getEndPointReference();
        assertTrue(eprRef.equals("myRole"));
        
        To to = copy.getTo();
        assertTrue(to != null);
        
        PartnerLink toPLink = to.getBPELPartnerLink();
        assertTrue(toPLink != null);
        
        Utility.logExit(getClass().getSimpleName(), "testPartnerLinkAssignModelTest"); 
    }
 
    public void testScopedPartnerLinkAssignModelTest() {

        Utility.logEnter(getClass().getSimpleName(), "testScopedPartnerLinkAssignModelTest");

        RBPELProcess process = loadBPELModel("partnerlink/ScopedPartnerLinkAssignModel.bpel");
        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "Assign1");
        BPELProcessOrScope scope = (BPELProcessOrScope) HelperFunc.getScopeByName(process, "Scope1");
        if (true) {
            Copy copy = assignActivity.getCopy(0);
            assertTrue(copy != null);

            From from = copy.getFrom();
            assertTrue(from != null);

            PartnerLink pLink = from.getBPELPartnerLink();
            assertTrue(pLink != null);

            String eprRef = from.getEndPointReference();
            assertTrue(eprRef.equals(PartnerLink.ATTR.MY_ROLE));

            To to = copy.getTo();
            assertTrue(to != null);

            PartnerLink toPLink = to.getBPELPartnerLink();
            assertTrue(toPLink != null);
            
            assertTrue(((RPartnerLink) pLink).getAssociatedScope() == scope);
            assertTrue(((RPartnerLink) toPLink).getAssociatedScope() == scope);            
        }
        if (true) {
            Copy copy = assignActivity.getCopy(1);
            assertTrue(copy != null);

            From from = copy.getFrom();
            assertTrue(from != null);

            PartnerLink pLink = from.getBPELPartnerLink();
            assertTrue(pLink != null);

            String eprRef = from.getEndPointReference();
            assertTrue(eprRef.equals(PartnerLink.ATTR.PARTNER_ROLE));

            To to = copy.getTo();
            assertTrue(to != null);

            PartnerLink toPLink = to.getBPELPartnerLink();
            assertTrue(toPLink != null);
            
            assertTrue(((RPartnerLink) pLink).getAssociatedScope() == scope);
            assertTrue(((RPartnerLink) toPLink).getAssociatedScope() == process);             
        } 
        
        if (true) {
            Copy copy = assignActivity.getCopy(2);
            assertTrue(copy != null);
            
            From from = copy.getFrom();
            assertTrue(from != null);
            
            PartnerLink pLink = from.getBPELPartnerLink();
            assertTrue(pLink != null);
            
            String eprRef = from.getEndPointReference();
            assertTrue(eprRef.equals(PartnerLink.ATTR.MY_ROLE));
            
            To to = copy.getTo();
            assertTrue(to != null);
            
            PartnerLink toPLink = to.getBPELPartnerLink();
            assertTrue(toPLink != null);   

            assertTrue(((RPartnerLink) pLink).getAssociatedScope() == process);
            assertTrue(((RPartnerLink) toPLink).getAssociatedScope() == process); 
        }
        Utility.logExit(getClass().getSimpleName(), "testScopedPartnerLinkAssignModelTest"); 
    }
    
    public void testRuntimeInternalEPR() {
        Utility.logEnter(getClass().getSimpleName(), "testRuntimeInternalEPR");

        RBPELProcess process = loadBPELModel("partnerlink/PartnerLinkModelTest.bpel");

        int pLinksSize = process.getPartnerLinks().getPartnerLinksSize();
        assertTrue(pLinksSize == 3);

        RPartnerLink pLink = (RPartnerLink) process.getPartnerLinks().getPartnerLink(0);

        RuntimePartnerLink rPLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
        System.out.println(rPLink.getServiceRef().toString());

        QName serviceName = QName.valueOf("{http://enterprise.netbeans.org/bpel/BPELModelJUnitTest/PartnerLinkModelTest}PartnerLink3");
        assertTrue (serviceName.equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getService()));
        assertTrue("newWSDLPortTypeRole_partnerRole".equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getEndPoint()));
        
        BPELProcessOrScope scope = (BPELProcessOrScope) HelperFunc.getScopeByName(process, "Scope1");

        pLinksSize = scope.getPartnerLinks().getPartnerLinksSize();
        pLink = (RPartnerLink) scope.getPartnerLinks().getPartnerLink(0);

        rPLink = new RuntimePartnerLinkImpl(pLink, BPELHelper.getGUID());
        System.out.println(rPLink.getServiceRef().toString());

        serviceName = QName.valueOf("{http://enterprise.netbeans.org/bpel/BPELModelJUnitTest/PartnerLinkModelTest}PartnerLink4");
        assertTrue (serviceName.equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getService()));
        assertTrue("newWSDLPortTypeRole_partnerRole".equals(((RuntimePartnerLink.InternalEPR) rPLink.getServiceRef()).getEndPoint()));

        Utility.logExit(getClass().getSimpleName(), "testRuntimeInternalEPR"); 
    }
    
    public void testLiteralEPR() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralEPR");

        RBPELProcess process = loadBPELModel("partnerlink/literalEPR/SynchronousSample.bpel");
        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "Assign2");
        assertTrue(assignActivity != null);
        From from = assignActivity.getCopy(0).getFrom();
        Literal lit = from.getLiteral();
        String litString = lit.getValue();
        assertTrue(com.sun.jbi.engine.bpel.core.bpel.util.Utility.isEmpty(litString));
        
        Element element = lit.getEII();
        String elementVal = createXmlString(element);
        
        Element elementToTest = getRootElement("bpel/partnerlink/literalEPR/literalElementTest.xml");
        String elementToTestVal = createXmlString(elementToTest);
        
        DocumentBuilderFactory docFactory =  DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        XMLUnit.setTestDocumentBuilderFactory(docFactory);
        XMLUnit.setControlDocumentBuilderFactory(docFactory);
        XMLUnit.setIgnoreWhitespace(true);
        
        Diff diff = XMLUnit.compare(elementToTestVal, elementVal);
        assertTrue(diff.similar());
        
        Utility.logExit(getClass().getSimpleName(), "testLiteralEPR"); 
    }
    
    
    public void testLiteralValue() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralValue");

        RBPELProcess process = loadBPELModel("partnerlink/literalEPR/Literal.bpel");
        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "Assign1");
        assertTrue(assignActivity != null);
        From from = assignActivity.getCopy(0).getFrom();
        Literal lit = from.getLiteral();
        String litString = lit.getValue();
        assertFalse(com.sun.jbi.engine.bpel.core.bpel.util.Utility.isEmpty(litString));
        
        Element element = lit.getEII();
        assertTrue(element == null);
        Utility.logExit(getClass().getSimpleName(), "testLiteralValue"); 
    }    

    /**
     * Convert the Element to a string representing XML
     * 
     * @param source 
     *            The Source object
     * @return The string representing XML
     */
    private static String createXmlString(Node node) {
        String xmlString = "";
        try {
            DOMSource source = new DOMSource(node);
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);

            Transformer transformer = transformer = TransformerFactory.newInstance().newTransformer();

            transformer.transform(source, result);

            xmlString = writer.toString();

        } catch (TransformerException ex) {
            fail(ex.getMessage());
        }
        return xmlString;
    }  
    
    
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
}