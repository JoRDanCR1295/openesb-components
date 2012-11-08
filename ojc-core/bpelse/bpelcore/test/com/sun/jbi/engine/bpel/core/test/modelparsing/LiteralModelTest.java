package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.io.InputStream;
import java.io.StringWriter;
import java.net.URL;
import java.util.logging.Logger;

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
import com.sun.bpel.model.From;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.Utility;

public class LiteralModelTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(LiteralModelTest.class.getName());

    public LiteralModelTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LiteralModelTest.class);

        return suite;
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
        assertTrue("ns3".equals(element.lookupPrefix("http://j2ee.netbeans.org/wsdl/stockQuote")));
        assertTrue("http://j2ee.netbeans.org/wsdl/stockQuote".equals(element.lookupNamespaceURI("ns3")));

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

    public void testLiteralEPR2() throws Exception {
        // This test was added when we found a minor bug with the literal element parsing.
        Utility.logEnter(getClass().getSimpleName(), "testLiteralEPR");

        RBPELProcess process = loadBPELModel("partnerlink/literalEPR/SynchronousSample2.bpel");
        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "PrefixInsideLiteral");
        assertTrue(assignActivity != null);
        From from = assignActivity.getCopy(0).getFrom();
        Literal lit = from.getLiteral();
        String litString = lit.getValue();
        assertTrue(com.sun.jbi.engine.bpel.core.bpel.util.Utility.isEmpty(litString));

        Element element = lit.getEII();
        assertTrue("http://j2ee.netbeans.org/wsdl/stockQuote".equals(element.lookupNamespaceURI("PrefixInsideLiteral")));

        String elementVal = createXmlString(element);

        Element elementToTest = getRootElement("bpel/partnerlink/literalEPR/literalElementTest2.xml");
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
        assertTrue("hello".equals(litString));

        Element element = lit.getEII();
        assertTrue(element == null);
        Utility.logExit(getClass().getSimpleName(), "testLiteralValue");
    }

    /**
     * Tests that the element in the literal tag is parsed correctly when the BPEL
     * has a default xmlns declaration.
     * @throws Exception
     */
    public void testLiteralXmlWithNoWhiteSpace() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testLiteralDefaultXmlns");

        RBPELProcess process = null;
		try {
			process = loadBPELModel("literal/AlertMef.bpel");
		} catch (RuntimeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

        Assign assignActivity = (Assign) HelperFunc.getActivity(process, "PrepareCallLocalStore");
        assertTrue(assignActivity != null);
        From from = assignActivity.getCopy(6).getFrom();
        Literal lit = from.getLiteral();

        Element element = lit.getEII();
        String elementVal = createXmlString(element);

        Element elementToTest = getRootElement("bpel/literal/AlertMef.xml");
        String elementToTestVal = createXmlString(elementToTest);

        DocumentBuilderFactory docFactory =  DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        XMLUnit.setTestDocumentBuilderFactory(docFactory);
        XMLUnit.setControlDocumentBuilderFactory(docFactory);
        XMLUnit.setIgnoreWhitespace(true);

        Diff diff = XMLUnit.compare(elementToTestVal, elementVal);
        assertTrue(diff.similar());

        Utility.logExit(getClass().getSimpleName(), "testLiteralDefaultXmlns");
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