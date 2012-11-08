/**
 *   xmpp-binding-component-extensions - Extensions for the XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.xmpp.extensions;

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


public class XMPPExtSerializerTest extends TestCase {
    private static final int SERVER_PORT = 5222;
    private static final String WSDL_FILE = "/xmppTestWsdl.wsdl";
    private ExtensionRegistry extReg = new ExtensionRegistry();

    public void testMarshall() throws Exception {
        XMPPExtSerializer serializer = new XMPPExtSerializer();
        WSDLReader reader = WSDLFactory.newInstance().newWSDLReader();
        Definition def = reader.readWSDL(this.getClass().getResource(WSDL_FILE)
                                             .getPath());

        XMPPBinding binding = new XMPPBinding();
        binding.setTlsEnabled(true);
        binding.setSaslEnabled(false);

        StringWriter expected = new StringWriter();
        PrintWriter pw = new PrintWriter(expected);
        serializer.marshall(null, null, binding, pw, def, extReg);
        pw.flush();

        assertEquals("<xmpp:binding tlsEnabled=\"true\" " +
            "saslEnabled=\"false\"/>", expected.toString().trim());

        XMPPAddress address = new XMPPAddress();
        address.setDomain("localhost");
        address.setPassword("password");
        address.setPort(SERVER_PORT);
        address.setResource("resource");
        address.setUsername("Alice");
        address.setGroup("room@chat.mil.com");
        expected = new StringWriter();
        pw = new PrintWriter(expected);
        serializer.marshall(null, null, address, pw, def, extReg);
        pw.flush();

        assertEquals("<xmpp:address domain=\"localhost\" username=\"Alice\" " +
            "password=\"password\" resource=\"resource\" port=\"5222\" group=\"room@chat.mil.com\"/>",
            expected.toString().trim());

        pw.close();
    }

    /**
     * Test of unmarshall method, of class com.sun.jbi.filebc.extensions.FileExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        XMPPExtSerializer serializer = new XMPPExtSerializer();
        Document wsdl = getWsdlDocument();

        Element element = getElement(wsdl, "xmpp:binding");
        ExtensibilityElement result = serializer.unmarshall(null,
                XMPPBinding.QNAME_BINDING, element, null, extReg);
        assertTrue(result instanceof XMPPBinding);

        element = getElement(wsdl, "xmpp:address");
        assertNotNull("Couldn't get address from wsdl", element);
        result = serializer.unmarshall(null, XMPPAddress.QNAME_ADDRESS,
                element, null, extReg);
        assertTrue(result instanceof XMPPAddress);

        XMPPAddress address = (XMPPAddress) result;
        assertEquals("b-0498", address.getDomain());
        assertEquals("bob", address.getPassword());
        assertTrue(SERVER_PORT == address.getPort());
        assertEquals("resource", address.getResource());
        assertEquals("bob", address.getUsername());
        assertEquals("room@chat.bar.com", address.getGroup());

        element = getElement(wsdl, "xmpp:operation");
        assertNotNull("Couldn't get operation from wsdl", element);
        result = serializer.unmarshall(null, XMPPOperation.QNAME_OPERATION,
                element, null, extReg);
        assertTrue(result instanceof XMPPOperation);

        XMPPOperation op = (XMPPOperation) result;
        assertEquals("Operation was incorrect",
            XMPPOperation.OperationName.sendMessage, op.getOperationName());

        element = getElement(wsdl, "xmpp:output");
        result = serializer.unmarshall(null,
                XMPPOperationOutput.QNAME_OPERATION_OUTPUT, element, null,
                extReg);
        assertTrue(result instanceof XMPPOperationOutput);

        XMPPOperationOutput output = (XMPPOperationOutput) result;
        assertEquals("part1", output.getJabberId());
        assertEquals("part2", output.getPacketId());
        assertEquals("part3", output.getMessage());

        element = getElement(wsdl, "xmpp:input");
        assertNotNull("Couldn't get input from wsdl", element);
        result = serializer.unmarshall(null,
                XMPPOperationInput.QNAME_OPERATION_INPUT, element, null, extReg);
        assertTrue(result instanceof XMPPOperationInput);

        XMPPOperationInput input = (XMPPOperationInput) result;
        assertEquals("part1", input.getJabberId());
        assertEquals("part2", input.getPacketId());
        assertEquals("part3", input.getMessage());
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
