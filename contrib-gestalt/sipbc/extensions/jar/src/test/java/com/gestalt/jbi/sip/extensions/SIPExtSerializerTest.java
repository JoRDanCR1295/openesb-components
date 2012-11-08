/**
 *   sip-binding-component-extensions - Extensions for the SIP Binding Component
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
package com.gestalt.jbi.sip.extensions;

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


public class SIPExtSerializerTest extends TestCase {
    private static final String WSDL_FILE = "/sipTestWsdl.wsdl";
    private ExtensionRegistry extReg = new ExtensionRegistry();

    public void testMarshall() throws Exception {
        SIPExtSerializer serializer = new SIPExtSerializer();
        WSDLReader reader = WSDLFactory.newInstance().newWSDLReader();
        Definition def = reader.readWSDL(this.getClass().getResource(WSDL_FILE)
                                             .getPath());

        SIPBinding binding = new SIPBinding();
        StringWriter expected = new StringWriter(512);
        PrintWriter pw = new PrintWriter(expected);
        serializer.marshall(null, null, binding, pw, def, extReg);
        pw.flush();

        assertEquals("<sip:binding/>", expected.toString().trim());

        SIPAddress address = new SIPAddress();
        address.setProxydomain("localhost");
        address.setProxyport(5060);
        address.setProxytimeout(5000);
        address.setUsername("Alice");
        address.setPassword("password");
        expected = new StringWriter(512);
        pw = new PrintWriter(expected);
        serializer.marshall(null, null, address, pw, def, extReg);
        pw.flush();

        assertEquals("<sip:address proxyDomain=\"localhost\" " +
            "proxyPort=\"5060\" proxyTimeout=\"5000\" username=\"Alice\" " +
            "password=\"password\"/>", expected.toString().trim());

        pw.close();
    }

    public void testUnmarshall() throws Exception {
        SIPExtSerializer serializer = new SIPExtSerializer();
        Document wsdl = getWsdlDocument();

        Element element = getElement(wsdl, "sip:binding");
        ExtensibilityElement result = serializer.unmarshall(null,
                SIPBinding.QNAME_BINDING, element, null, extReg);
        assertTrue(result instanceof SIPBinding);

        element = getElement(wsdl, "sip:address");
        assertNotNull("Couldn't get address from wsdl", element);
        result = serializer.unmarshall(null, SIPAddress.QNAME_ADDRESS, element,
                null, extReg);
        assertTrue(result instanceof SIPAddress);

        SIPAddress address = (SIPAddress) result;
        assertEquals("pass", address.getPassword());
        assertEquals("10.9.5.147", address.getProxydomain());
        assertTrue(5060 == address.getProxyport());
        assertTrue(5000 == address.getProxytimeout());
        assertEquals("movielistingbot", address.getUsername());

        element = getElement(wsdl, "sip:operation");
        assertNotNull("Couldn't get operation from wsdl", element);
        result = serializer.unmarshall(null, SIPOperation.QNAME_OPERATION,
                element, null, extReg);
        assertTrue(result instanceof SIPOperation);

        SIPOperation operation = (SIPOperation) result;
        assertEquals(SIPOperation.OperationName.sendRequest.toString(),
            operation.getOperationName().toString().trim());

        element = getElement(wsdl, "sip:input");
        assertNotNull("Couldn't get input from wsdl", element);
        result = serializer.unmarshall(null,
                SIPOperationInput.QNAME_OPERATION_INPUT, element, null, extReg);
        assertTrue(result instanceof SIPOperationInput);

        SIPOperationInput input = (SIPOperationInput) result;
        assertEquals("part1", input.getRequestMethod());
        assertEquals("part2", input.getContent());
        assertEquals("part3", input.getRemoteUri());
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
