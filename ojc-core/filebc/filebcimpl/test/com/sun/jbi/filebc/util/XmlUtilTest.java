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
 * @(#)XmlUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.filebc.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import junit.framework.*;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

/**
 *
 * @author sweng
 */
public class XmlUtilTest extends TestCase {
    public XmlUtilTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    /**
     * Test of createDocument method, of class com.sun.jbi.filebc.util.XmlUtil.
     */
    public void testCreateDocument() throws Exception {
        System.out.println("Testing createDocument");
        
        Document result = XmlUtil.createDocument(true);
        assertTrue(result instanceof Document);
        
        result = XmlUtil.createDocument(true, new InputSource(new StringReader("<Foo>something</Foo>")));
        assertTrue(result instanceof Document);
        System.out.println("Successfully tested createDocument");
    }
    
    /**
     * Test of createDocumentFromXML method, of class com.sun.jbi.filebc.util.XmlUtil.
     */
    public void testCreateDocumentFromXML() throws Exception {
        System.out.println("Testing createDocumentFromXML");
        
        boolean namespaceAware = true;
        String xml = "<Foo> my test </Foo>";
        
        Document expResult = null;
        Document result = XmlUtil.createDocumentFromXML(namespaceAware, xml);
        Element node = result.getDocumentElement();
        assertEquals("Foo", node.getNodeName());
        Node child = node.getFirstChild();
        assertTrue(child instanceof Text);
        assertEquals(" my test ", ((Text)child).getNodeValue());
        assertTrue(result instanceof Document);
        
        System.out.println("Successfully tested createDocumentFromXML");
    }
    
    /**
     * Test of getText method, of class com.sun.jbi.filebc.util.XmlUtil.
     */
    public void testGetText() {
        System.out.println("Testing getText");
        
        try {
            InputStream is = new FileInputStream(new File("test/com/sun/jbi/filebc/util/testInput/TestGetText.xml"));
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.parse(is);
            Node node = document.getDocumentElement();

            String result = XmlUtil.getText((Node)node);
            assertEquals(" Hello World!!!\n  \n  \n  \n  \n  \n  \n", result);
        } catch (Exception e) {
            fail("Failed to test getText");
        }
        
        System.out.println("Successfull tested getText");
    }
    
    /**
     * Test of transformToBytes method, of class com.sun.jbi.filebc.util.XmlUtil.
     */
    public void testTransformToBytes() throws Exception {
        System.out.println("Testing transformToBytes");
        
        InputStream is = new FileInputStream(new File("test/com/sun/jbi/filebc/util/testInput/TestGetText.xml"));
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(is);
        Node node = document.getDocumentElement();
        
        // test with omit xml declaration flag true
        byte[] expResult = ("<helloWorld xmlns=\"urn:Foo\"> Hello World!!!\r\n" +
                            "  <string>input0</string>\r\n" + 
                            "  <byte>a</byte>\r\n" + 
                            "  <short>5</short>\r\n" +
                            "  <int>12345</int>\r\n" +
                            "  <long>12345</long>\r\n" +
                            "  <float>2.22</float>\r\n" +
                            "</helloWorld>\r\n").getBytes();
        byte[] result = XmlUtil.transformToBytes(node, "utf-8", true);
        for (int ii = 0; ii < expResult.length; ii++) {
            assertTrue(expResult[ii] == result[ii]);
        }
        
        expResult = ("<?xml version=\"1.0\" encoding=\"utf-8\"?>\r\n" + 
                    "<helloWorld xmlns=\"urn:Foo\"> Hello World!!!\r\n" +
                    "  <string>input0</string>\r\n" + 
                    "  <byte>a</byte>\r\n" + 
                    "  <short>5</short>\r\n" +
                    "  <int>12345</int>\r\n" +
                    "  <long>12345</long>\r\n" +
                    "  <float>2.22</float>\r\n" +
                    "</helloWorld>\r\n").getBytes();
        result = XmlUtil.transformToBytes(node, "utf-8", false);
        for (int ii = 0; ii < expResult.length; ii++) {
            assertTrue(expResult[ii] == result[ii]);
        }
        
        result = XmlUtil.transformToBytes(node, "utf-8", false, "xml");
        for (int ii = 0; ii < expResult.length; ii++) {
            assertTrue(expResult[ii] == result[ii]);
        }
        
        expResult = ("Hello World!!!").getBytes();
        node = document.createTextNode("Hello World!!!");
        
        result = XmlUtil.transformToBytes(node, "utf-8", false, "text");
        for (int ii = 0; ii < expResult.length; ii++) {
            assertTrue(expResult[ii] == result[ii]);
        }
        
        System.out.println("Successfully tested transformToBytes");
    }
    
    /**
     * Test of transformToString method, of class com.sun.jbi.filebc.util.XmlUtil.
     */
    public void testTransformToString() throws Exception {
        System.out.println("Testing transformToString");
        
        InputStream is = new FileInputStream(new File("test/com/sun/jbi/filebc/util/testInput/TestGetText.xml"));
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(is);
        Node node = document.getDocumentElement();
        
        // test with omit xml declaration flag true
        String expResult = "<helloWorld xmlns=\"urn:Foo\"> Hello World!!!\r\n" +
                            "  <string>input0</string>\r\n" + 
                            "  <byte>a</byte>\r\n" + 
                            "  <short>5</short>\r\n" +
                            "  <int>12345</int>\r\n" +
                            "  <long>12345</long>\r\n" +
                            "  <float>2.22</float>\r\n" +
                            "</helloWorld>\r\n";
        String result = XmlUtil.transformToString(node, "utf-8", true);
        System.out.println("expResult is [" + expResult + "]");
        System.out.println("result is [" + result + "]"); 
        assertEquals(expResult, result);
        
        expResult = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r\n" + 
                    "<helloWorld xmlns=\"urn:Foo\"> Hello World!!!\r\n" +
                    "  <string>input0</string>\r\n" + 
                    "  <byte>a</byte>\r\n" + 
                    "  <short>5</short>\r\n" +
                    "  <int>12345</int>\r\n" +
                    "  <long>12345</long>\r\n" +
                    "  <float>2.22</float>\r\n" +
                    "</helloWorld>\r\n";
        result = XmlUtil.transformToString(node, "utf-8", false);
        assertEquals(expResult, result);
        
        result = XmlUtil.transformToString(node, "utf-8", false, "xml");
        assertEquals(expResult, result);
        
        expResult = "Hello World!!!";
        node = document.createTextNode("Hello World!!!");
        
        result = XmlUtil.transformToString(node, "utf-8", false, "text");
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested transformToString");
    }
    
}
