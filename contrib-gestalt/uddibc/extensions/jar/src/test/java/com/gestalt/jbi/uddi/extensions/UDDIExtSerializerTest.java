/**
 *   uddi-binding-component-extensions - Extensions for the UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.extensions;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.PrintWriter;
import java.io.StringWriter;

import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


public class UDDIExtSerializerTest extends TestCase {
    private static final String WSDL_FILE = "/uddiTestWsdl.wsdl";
    private ExtensionRegistry extReg = new ExtensionRegistry();

    public void testMarshall() throws Exception {
        UDDIExtSerializer serializer = new UDDIExtSerializer();
        WSDLReader reader = WSDLFactory.newInstance().newWSDLReader();
        Definition def = reader.readWSDL(this.getClass().getResource(WSDL_FILE)
                                             .getPath());

        UDDIBinding binding = new UDDIBinding();
        StringWriter expected = new StringWriter();
        PrintWriter pw = new PrintWriter(expected);
        serializer.marshall(null, null, binding, pw, def, extReg);
        pw.flush();

        assertEquals("<uddi:binding/>", expected.toString().trim());

        UDDIAddress address = new UDDIAddress();
        address.setInquiryUri("http://uddi.inquiry.com");
        address.setPublishUri("http://uddi.publish.com");
        expected = new StringWriter();
        pw = new PrintWriter(expected);
        serializer.marshall(null, null, address, pw, def, extReg);
        pw.flush();

        assertEquals("<uddi:address inquiryUri=\"http://uddi.inquiry.com\" " +
            "publishUri=\"http://uddi.publish.com\"/>",
            expected.toString().trim());

        pw.close();
    }

    public void testUnmarshall() throws Exception {
        UDDIExtSerializer serializer = new UDDIExtSerializer();
        Document wsdl = getWsdlDocument();

        Element element = getElement(wsdl, "uddi:binding");
        ExtensibilityElement result = serializer.unmarshall(null,
                UDDIBinding.QNAME_BINDING, element, null, extReg);
        assertTrue(result instanceof UDDIBinding);

        element = getElement(wsdl, "uddi:address");
        assertNotNull("Couldn't get address from wsdl", element);
        result = serializer.unmarshall(null, UDDIAddress.QNAME_ADDRESS,
                element, null, extReg);
        assertTrue(result instanceof UDDIAddress);

        UDDIAddress address = (UDDIAddress) result;
        assertEquals("http://test.uddi.microsoft.com/inquire",
            address.getInquiryUri());
        assertEquals("http://localhost/publish", address.getPublishUri());

        element = getElement(wsdl, "uddi:operation");
        assertNotNull("Couldn't get operation from wsdl", element);
        result = serializer.unmarshall(null, UDDIOperation.QNAME_OPERATION,
                element, null, extReg);
        assertTrue(result instanceof UDDIOperation);

        UDDIOperation operation = (UDDIOperation) result;
        assertEquals(UDDIOperation.OperationName.search.toString(),
            operation.getOperationName().toString().trim());

        element = getElement(wsdl, "uddi:output");
        result = serializer.unmarshall(null,
                UDDIOperationOutput.QNAME_OPERATION_OUTPUT, element, null,
                extReg);
        assertTrue(result instanceof UDDIOperationOutput);

        element = getElement(wsdl, "uddi:input");
        assertNotNull("Couldn't get input from wsdl", element);
        result = serializer.unmarshall(null,
                UDDIOperationInput.QNAME_OPERATION_INPUT, element, null, extReg);
        assertTrue(result instanceof UDDIOperationInput);

        UDDIOperationInput input = (UDDIOperationInput) result;
        assertEquals("part1", input.getBusinessName());
        assertEquals("part2", input.getServiceName());
    }

    private Document getWsdlDocument() throws Exception {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(this.getClass().getResource(WSDL_FILE).getFile());

        if (doc == null) {
            fail("Something went wrong while parsing the wsdl, cannot proceed");
        }

        return doc;
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
}
