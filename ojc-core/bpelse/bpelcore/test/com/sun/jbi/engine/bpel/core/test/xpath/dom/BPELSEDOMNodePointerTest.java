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
 * @(#)BPELSEDOMNodePointerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.xpath.dom;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Level;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import junit.framework.TestCase;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.JXPathContextReferenceImpl;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;

import com.sun.jbi.engine.bpel.core.bpel.xpath.dom.BPELSEDOMPointerFactory;

public class BPELSEDOMNodePointerTest extends TestCase {

    DocumentBuilderFactory factory = null;
    DocumentBuilder builder = null;
    
    public static void main(String[] args) {
        junit.textui.TestRunner.run(BPELSEDOMNodePointerTest.class);
    }

    public BPELSEDOMNodePointerTest(String arg0) throws ParserConfigurationException {
        super(arg0);
        factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        builder = factory.newDocumentBuilder();   
    }

    protected void setUp() throws Exception {
        super.setUp();
    }
    
    public void testBooleanTest1() throws Exception {
        URL sampleXSDurl = BPELSEDOMNodePointerTest.class.getResource("simple.xsd");
        String sampleXSD = sampleXSDurl.getPath();
        URL sampleXmlurl = BPELSEDOMNodePointerTest.class.getResource("simple.xml");
        String sampleXml = sampleXmlurl.getPath();
        Document document = getDocument(sampleXSD, sampleXml);
        SchemaTypeSystem schemaTypeSystem = getSchemaTypeSystem(sampleXSD);
        
        JXPathContext ctx = getJXpathContext();
        
        initialize(ctx, document, schemaTypeSystem);
        
        // register NS
        ctx.registerNamespace("ns0", "http://localhost/myXpathTest");
        
        assertEquals ("true", evaluate(ctx, "(1) or (2) or (null)"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:month = 'January'"));
        assertEquals ("true", evaluate(ctx, "true()"));
        assertEquals ("false", evaluate(ctx, "false()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:year = 1983"));
        assertEquals ("false", evaluate(ctx, "boolean($var1/ns0:booleanFalseData)"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData"));
        assertEquals ("true", evaluate(ctx, "boolean($var1/ns0:booleanTrueData)"));
        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanFalseData"));
        assertEquals ("false", evaluate(ctx, "boolean($var1/ns0:booleanFalseData)"));
        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanTrueData = false()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData = true()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanFalseData = false()"));

        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanFalseData = true()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData or true()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanFalseData or true()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData and true()"));
        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanFalseData and false()"));
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData and $var1/ns0:booleanTrueData"));        
        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanTrueData and $var1/ns0:booleanFalseData"));        
        assertEquals ("true", evaluate(ctx, "$var1/ns0:booleanTrueData or $var1/ns0:booleanFalseData"));        
        assertEquals ("false", evaluate(ctx, "$var1/ns0:booleanFalseData or $var1/ns0:booleanFalseData"));        

        assertEquals ("true", evaluate(ctx, "boolean('some string')"));
        assertEquals ("true", evaluate(ctx, "'some string' = true()"));
        assertEquals ("false", evaluate(ctx,  "'some string' = false()"));
    }
    
    public void testBooleanTest2() throws Exception {
        URL sampleXSDurl = BPELSEDOMNodePointerTest.class.getResource("simple.xsd");
        String sampleXSD = sampleXSDurl.getPath();
        URL sampleXmlurl = BPELSEDOMNodePointerTest.class.getResource("simple.xml");
        String sampleXml = sampleXmlurl.getPath();
        Document document = getDocument(sampleXSD, sampleXml);
        SchemaTypeSystem schemaTypeSystem = getSchemaTypeSystem(sampleXSD);
        
        JXPathContext ctx = getJXpathContext();
        initialize(ctx, document, schemaTypeSystem);
        ctx.registerNamespace("ns0", "http://localhost/myXpathTest");
        assertEquals ("true", evaluate(ctx, "boolean($var1)"));
    }
    
    public void testBooleanTest3() throws Exception {
        URL sampleXSDurl = BPELSEDOMNodePointerTest.class.getResource("simple.xsd");
        String sampleXSD = sampleXSDurl.getPath();
        URL sampleXmlurl = BPELSEDOMNodePointerTest.class.getResource("simple.xml");
        String sampleXml = sampleXmlurl.getPath();
        Document document = getDocument(sampleXSD, sampleXml);
        SchemaTypeSystem schemaTypeSystem = getSchemaTypeSystem(sampleXSD);
        
        JXPathContext ctx = getJXpathContext();
        initialize(ctx, document, schemaTypeSystem);
        ctx.registerNamespace("ns0", "http://localhost/myXpathTest");
        assertEquals ("false", evaluate(ctx, " boolean($var1/ns0:booleanElement)"));
    }
    
    private String evaluate(JXPathContext ctx, String expression) {
        String result  = (String) ctx.getValue(expression, String.class);
        return result;
        //System.out.println("[" + expression + "] => " + result);        
    }
    
    private JXPathContext getJXpathContext() {
        //JXPathContext ctx = JXPathContext.newContext(null);
        BPELSEDOMPointerFactory nodePointerFactory = new BPELSEDOMPointerFactory();
        JXPathContextReferenceImpl.addNodePointerFactory(nodePointerFactory);
        JXPathContext ctx = JXPathContext.newContext(null); 
        return ctx;
    }
    
    private void initialize(JXPathContext ctx, Document document, SchemaTypeSystem schemaTypeSystem) {
        //ctx.getVariables().declareVariable("var1", document);
        Element element = document.getDocumentElement();
        ctx.getVariables().declareVariable("var1", element);
        
        SchemaField SchemaField = getGlobalElementType(schemaTypeSystem, element);
        
        element.setUserData("schemaType", SchemaField, null);
    }
    
    private SchemaField getGlobalElementType(SchemaTypeSystem schemaTypeSystem, Node node) {
        javax.xml.namespace.QName qName = new javax.xml.namespace.QName(node.getNamespaceURI(),
                node.getLocalName());
        return Utility.getGlobalElement(schemaTypeSystem, qName);
    }
    
    private SchemaTypeSystem getSchemaTypeSystem(String sampleXSD) throws Exception {
        Collection errors = new ArrayList();
        SchemaTypeSystem schemaTypeSystem = null;
        File sampleXmlFile = new File(sampleXSD);
        
        XmlObject schemaObject = null;
        
        
        try {
            if (sampleXSD != null) {
                schemaObject = XmlObject.Factory.parse(sampleXmlFile, new XmlOptions().setErrorListener(errors).setLoadLineNumbers());
            }
            XmlObject[] schemas = {schemaObject};

            schemaTypeSystem = XmlBeans.compileXsd(schemas, XmlBeans.getBuiltinTypeSystem(),
                    new XmlOptions().setErrorListener(errors));

            /*for (Object key : schmeaTypeMap.keySet()) {
                Object value = schmeaTypeMap.get(key);
                System.out.println(key + " => " + value);
            }*/
        } catch (Exception e) {
            System.out.print("Exception : " + e.getMessage());
            throw e;
        }
        return schemaTypeSystem;
    }
    
    private Document getDocument(String sampleXSD, String sampleXml) {
        Document document = null;
        try {
            // set the schema on the document
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(new File(sampleXSD));
            factory.setSchema(schema);

            // set error handler
            builder.setErrorHandler(new MyErrorHandler());
            document = builder.parse(new File(sampleXml));

        } catch (Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        return document;
    }
}

class MyErrorHandler implements ErrorHandler {
    // This method is called in the event of a recoverable error
    public void error(SAXParseException e) {
        log(Level.SEVERE, "Error", e);
    }
    
    // This method is called in the event of a non-recoverable error
    public void fatalError(SAXParseException e) {
        log(Level.SEVERE, "Fatal Error", e);
    }
    
    // This method is called in the event of a warning
    public void warning(SAXParseException e) {
        log(Level.WARNING, "Warning", e);
    }
    
    // Dump a log record to a logger
    private void log(Level level, String message, SAXParseException e) {
        // Get details
        int line = e.getLineNumber();
        int col = e.getColumnNumber();
        String publicId = e.getPublicId();
        String systemId = e.getSystemId();
        
        // Append details to message
        message = message + ": " + e.getMessage() + ": line=" + line + ", col=" + col + ", PUBLIC="
        + publicId + ", SYSTEM=" + systemId;
        
        // Log the message
        System.out.println("Error: " + level + message);
    }
}
