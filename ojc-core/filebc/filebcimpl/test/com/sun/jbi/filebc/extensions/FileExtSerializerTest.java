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
 * @(#)FileExtSerializerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import junit.framework.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 *
 * @author sweng
 */
public class FileExtSerializerTest extends TestCase {

    Class parentType = null;
    Definition def = null;
    ExtensionRegistry extReg = null;
    FileExtSerializer instance = null;
    QName bindingElementType = null;
    QName bindingOperationElementType = null;
    QName addressElementType = null;
    QName messageElementType = null;
    Document doc = null;
    WSDLFactory wsdlFactory = null;
    WSDLReader reader = null;
    String outputFolder = null;
    String expectedFolder = null;

    public FileExtSerializerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new FileExtSerializer();
        extReg = new ExtensionRegistry();
        bindingElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", Constants.ELEM_BINDING);
        bindingOperationElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", Constants.ELEM_OPERATION);
        addressElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", "address");
        messageElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", "message");
        outputFolder = "test/com/sun/jbi/filebc/extensions/output/";
        expectedFolder = "test/com/sun/jbi/filebc/extensions/expected/";
        BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(new File("test/com/sun/jbi/filebc/packaging/wsdls/TestFile.wsdl")), "UTF-8"));
        wsdlFactory = WSDLFactory.newInstance();
        reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(new CatalogResolver(new CatalogManager()));
        def = reader.readWSDL(new File("test/com/sun/jbi/filebc/packaging/wsdls/TestFile.wsdl").getAbsolutePath());
        try {
            InputSource is = new InputSource(br);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db;
            synchronized (dbf) {
                dbf.setNamespaceAware(true);
                db = dbf.newDocumentBuilder();
            }
            doc = db.parse(is);

        } catch (Exception e) {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }

        if (doc == null) {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FileExtSerializerTest.class);

        return suite;
    }

    /**
     * Test of marshall method, of class com.sun.jbi.filebc.extensions.FileExtSerializer.
     */
    public void testMarshall() throws Exception {
        System.out.println("Testing marshall");
        ExtensibilityElement elem = null;
        String outputFileName = null;
        String expectedOutputFileName = null;
        PrintWriter pw = null;

        // 1. testing file:binding extensibility element
        elem = new FileBinding();
        outputFileName = outputFolder + "TestFileBindingElement.xml";
        expectedOutputFileName = expectedFolder + "FileBindingElement.xml";
        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();

        System.out.println("======== expected output is [" + getFileContents(expectedOutputFileName) + "] and actual output is [" + getFileContents(outputFileName) + "]");
        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());

        // 2. testing file:operation extensibility element
        elem = new FileOperation();
        outputFileName = outputFolder + "TestFileOperationElement.xml";
        expectedOutputFileName = expectedFolder + "FileOperationElement.xml";

        elem.setRequired(Boolean.TRUE);

        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();

        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());

        // 3. testing file:messag extensibility element
        outputFileName = outputFolder + "TestFileMessageElement.xml";
        expectedOutputFileName = expectedFolder + "FileMessageElement.xml";

        elem = new FileMessage();
        ((FileMessage) elem).setFileName("FileBCInput.txt");
        ((FileMessage) elem).setFileNameIsPattern(Boolean.TRUE);
        ((FileMessage) elem).setPollingInterval(new Long(9999));
        ((FileMessage) elem).setMultipleRecordsPerFile(Boolean.TRUE);
        ((FileMessage) elem).setProtectDirectory("protect");

        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();

        assertEquals(getFileContents(expectedOutputFileName).trim(), getFileContents(outputFileName).trim());

        // 5. testing file:address extensibility element
        elem = new FileAddress();
        outputFileName = outputFolder + "TestFileAddressElement.xml";
        expectedOutputFileName = expectedFolder + "FileAddressElement.xml";
        elem.setRequired(Boolean.TRUE);
        ((FileAddress) elem).setFileDirectory("/tmp/testing");

        pw = new PrintWriter(new FileOutputStream(new File(outputFileName)));
        instance.marshall(null, null, elem, pw, def, extReg);
        pw.flush();

        String tmp = getFileContents(expectedOutputFileName).trim();
        System.out.println("=====>" + tmp);
        String tmp2 = getFileContents(outputFileName).trim();
        System.out.println("=====>" + tmp2);
        assertEquals(tmp, tmp2);

        System.out.println("Successfully tested marshal");
    }

    /**
     * Test of unmarshall method, of class com.sun.jbi.filebc.extensions.FileExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        System.out.println("Testing unmarshall");

        Element xelem = null;
        ExtensibilityElement expResult = null;
        ExtensibilityElement result = null;

        // 1. testing file:binding element
        xelem = getElement(doc, "file:binding");
        result = instance.unmarshall(null, bindingElementType, xelem, null, extReg);
        assertTrue(result instanceof FileBinding);

        // 2. testing file:operation element
        xelem = getElement(doc, "file:operation");
        if (xelem != null) {
            result = instance.unmarshall(null, bindingOperationElementType, xelem, null, extReg);
            assertTrue(result instanceof FileOperation);
            FileOperation oper = (FileOperation) result;
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }

        // 3. testing file:message element
        xelem = getElement(doc, "file:message");

        if (xelem != null) {
            result = instance.unmarshall(null, messageElementType, xelem, null, extReg);
            assertTrue(result instanceof FileMessage);
            FileMessage fileMessage = (FileMessage) result;
            assertEquals("FileBCInput.txt", fileMessage.getFileName());
            assertTrue(fileMessage.getFileNameIsPattern());
            assertEquals(9999, fileMessage.getPollingInterval().longValue());
            assertTrue(fileMessage.getMultipleRecordsPerFile());
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }

        // 5. testing file:address element
        xelem = getElement(doc, "file:address");
        if (xelem != null) {
            result = instance.unmarshall(null, addressElementType, xelem, null, extReg);
            assertTrue(result instanceof FileAddress);
            FileAddress address = (FileAddress) result;
            assertEquals("/tmp/testing", address.getFileDirectory());
        } else {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
        System.out.println("Successfully tested unmarshal");
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
            fail("Failed to retrieve content from " + fileName + ".");
        }

        return output.toString();
    }

    public void testRemove() throws Exception {
        String result = instance.removeExtraEscapeCharacter("\\\\\\");
        assertEquals("\\\\\\", result);

        result = instance.removeExtraEscapeCharacter("\\r\\n\\t\\f\\b");
        assertEquals("\r\n\t\f\b", result);
    }
}
