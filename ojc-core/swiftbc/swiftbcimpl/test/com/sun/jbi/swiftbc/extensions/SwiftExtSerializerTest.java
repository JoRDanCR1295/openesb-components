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
 * @(#)SwiftExtSerializerTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import junit.framework.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.io.PrintWriter;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.util.StringUtils;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;


/**
 *
 * @author Sun Microsystems Inc.
 */
public class SwiftExtSerializerTest extends TestCase {
    Class parentType = null;
    Definition def = null;
    ExtensionRegistry extReg = null;
    SwiftExtSerializer instance = null;
    QName bindingElementType = null;
    QName bindingOperationElementType = null;
    QName addressElementType = null;
    QName messageElementType = null;
    QName protocolpropertiesElementType = null;
    Document doc = null;
    WSDLFactory wsdlFactory = null;
    WSDLReader reader = null;
    
    String outputFolder = null;
    String expectedFolder = null;
    
    /**
     * 
     * @param testName 
     */
    public SwiftExtSerializerTest(String testName) {
        super(testName);
    }
    
    /**
     * 
     * @throws java.lang.Exception 
     */
    protected void setUp() throws Exception {
        instance = new SwiftExtSerializer();
        extReg = new ExtensionRegistry();
        bindingElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", Constants.ELEM_BINDING);
        bindingOperationElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", Constants.ELEM_OPERATION);
        addressElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "address");
        messageElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "message");
        protocolpropertiesElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "protocolproperties");
        outputFolder = "test/com/sun/jbi/swiftbc/extensions/output/";
        expectedFolder = "test/com/sun/jbi/swiftbc/extensions/expected/";
        BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(new File("test/com/sun/jbi/swiftbc/packaging/wsdls/TestSwift.wsdl")), "UTF-8"));
        wsdlFactory = WSDLFactory.newInstance();
        reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(new CatalogResolver(new CatalogManager()));
        def = reader.readWSDL(new File("test/com/sun/jbi/swiftbc/packaging/wsdls/TestSwift.wsdl").getAbsolutePath());
        try {
            InputSource is = new InputSource(br);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db;
            synchronized(dbf) {
                dbf.setNamespaceAware(true);
                db = dbf.newDocumentBuilder();
            }
            doc = db.parse(is);
            
        } catch(Exception e) {
            fail("Something went wrong during parsing TestSwift.wsdl, cannot proceed");
        }
        
        if (doc == null) {
            fail("Something went wrong during parsing TestSwift.wsdl, cannot proceed");
        }
    }
    
    /**
     * 
     * @throws java.lang.Exception 
     */
    protected void tearDown() throws Exception {
    }
    
    /**
     * 
     * @return 
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(SwiftExtSerializerTest.class);
        
        return suite;
    }
    
    /**
     * Test of marshall method, of class com.sun.jbi.swiftbc.extensions.SwiftExtSerializer.
     */
    public void testMarshall() throws Exception {
        /*
        System.out.println("Testing marshall");
        ExtensibilityElement elem = null;
        String outputFileName = null;
        String expectedOutputFileName = null;
        PrintWriter pw = null;
        
        // 1. testing Swift:binding extensibility element
        elem = new SwiftBinding();
        outputFileName = outputFolder + "TestSwiftBindingElement.xml";
        expectedOutputFileName = expectedFolder + "SwiftBindingElement.xml";
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();
        
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());
        
        // 2. testing Swift:operation extensibility element
        elem = new SwiftOperation();
        outputFileName = outputFolder + "TestSwiftOperationElement.xml";
        expectedOutputFileName = expectedFolder + "SwiftOperationElement.xml";
        
        elem.setRequired(Boolean.TRUE);
        
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();
        
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());
        
        // 3. testing Swift:message extensibility element
        outputFileName = outputFolder + "TestSwiftMessageElement.xml";
        expectedOutputFileName = expectedFolder + "SwiftMessageElement.xml";
        
        elem = (new  SAGObjectFactoryFactory()).getObjectFactory().getNewMessage();
        //        ((SwiftMessage)elem).setPart("part1");
        ((SwiftMessage)elem).setEncodingStyle("swiftencoder-1.0");
        
        
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();
        
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());
        
        // 5. testing Swift:address extensibility element
        elem = new SwiftAddress();
        outputFileName = outputFolder + "TestSwiftAddressElement.xml";
        expectedOutputFileName = expectedFolder + "SwiftAddressElement.xml";
        elem.setRequired(Boolean.TRUE);
        ((SwiftAddress)elem).setSwiftServerLocationURL("swift://localhost:48002");
        ((SwiftAddress)elem).setTransportProtocolName("tcp-ip");
        
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();
        
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());
        
        // 5. testing Swift:protocolproperties extensibility element
        elem = new SwiftProtocolProperties();
        outputFileName = outputFolder + "TestSwiftProtocolPropertiesElement.xml";
        expectedOutputFileName = expectedFolder + "SwiftProtocolPropertiesElement.xml";
        ((SwiftProtocolProperties)elem).setSoftwareVendorOrganization("Sun Microsystems, Inc.");
        ((SwiftProtocolProperties)elem).setSoftwareCertifiedVersionOrReleaseNumber("6.0");
        ((SwiftProtocolProperties)elem).setSoftwareProductName("Sun Swift BC");
        ((SwiftProtocolProperties)elem).setSoftwareBinaryID("6.0");
        ((SwiftProtocolProperties)elem).setSoftwareProductInformation("It is a Swift Binding component");
        ((SwiftProtocolProperties)elem).setSoftwareInstallDate("011220062556");
        
        
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();
        
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());
        
        
        
        System.out.println("Successfully tested marshal");
        */
    }
    
    /**
     * Test of unmarshall method, of class com.sun.jbi.swiftbc.extensions.SwiftExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        /*        System.out.println("Testing unmarshall");
        
        Element xelem = null;
        ExtensibilityElement expResult = null;
        ExtensibilityElement result = null;
        
        // 1. testing Swift:binding element
        xelem = getElement(doc, "swift:binding");
        result = instance.unmarshall(null, bindingElementType, xelem, null, extReg);
        assertTrue(result instanceof SwiftBinding);
        
        // 2. testing Swift:operation element
        xelem = getElement(doc, "swift:operation");
        if (xelem != null) {
            result = instance.unmarshall(null, bindingOperationElementType, xelem, null, extReg);
            assertTrue(result instanceof SwiftOperation);
            SwiftOperation oper = (SwiftOperation)result;
        } else {
            fail("Something went wrong during parsing TestSwift.wsdl, cannot proceed");
        }
        
        // 3. testing file:message element
        xelem = getElement(doc, "swift:message");
        
        if (xelem != null) {
            result = instance.unmarshall(null, messageElementType, xelem, null, extReg);
            assertTrue(result instanceof SwiftMessage);
            SwiftMessage swiftMessage = (SwiftMessage)result;
            assertEquals("swiftencoder-1.0", swiftMessage.getEncodingStyle());
            //            assertEquals("part1", swiftMessage.getPart());
            
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
        
        // 5. testing Swift:address element
        xelem = getElement(doc, "Swift:address");
        if (xelem != null) {
            result = instance.unmarshall(null, addressElementType, xelem, null, extReg);
            assertTrue(result instanceof SwiftAddress);
            SwiftAddress address = (SwiftAddress)result;
            assertEquals("swift://localhost:48002", address.getSwiftServerLocationURL());
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
        
        // 6. testing Swift:address element
        xelem = getElement(doc, "swift:protocolproperties");
        if (xelem != null) {
            result = instance.unmarshall(null, protocolpropertiesElementType, xelem, null, extReg);
            assertTrue(result instanceof SwiftProtocolProperties);
            SwiftProtocolProperties protocolproperties = (SwiftProtocolProperties)result;
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
        
        
        System.out.println("Successfully tested unmarshal");
        */
    }
    
    
    private String getFileContents(String fileName) {
        StringBuffer output = new StringBuffer();
        try {
            FileReader reader = new FileReader(fileName);
            char[] buff = new char[512];
            int len = reader.read(buff);
            while (len > 0) {
                output.append(buff, 0, len);
                len = reader.read(buff);
            }
        } catch (Exception e) {
            fail("Failed to retrieve content from " + fileName  + ".");
        }
        
        return output.toString();
    }
    
    private Element getElement(Node aNode, String elementName) {
        Element theOne = null;
        if (aNode.getNodeName().equalsIgnoreCase(elementName)) {
            return (Element) aNode;
        }
        
        NodeList children = aNode.getChildNodes();
        for (int ii = 0; ii < children.getLength(); ii++) {
            Node child = children.item(ii);
            if (child.getNodeName().equalsIgnoreCase(elementName)) {
                theOne = (Element) child;
                break;
            } else {
                theOne = getElement(child, elementName);
                if (theOne != null) {
                    break;
                }
            }
        }
        return theOne;
    }
    
    /**
     * 
     * @param args 
     */
    public static void main(String args[]){
        try{
            SwiftExtSerializerTest test = new SwiftExtSerializerTest("myTest");
            test.setName("name");
            test.setUp();
            test.testMarshall();
            test.testUnmarshall();
        }catch(Exception e){
            e.printStackTrace();
        }
    }
    
}
