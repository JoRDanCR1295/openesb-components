/**
 *   xmpp-binding-component - XMPP Binding Component
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
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.custommonkey.xmlunit.XMLTestCase;

import java.io.File;
import java.io.StringWriter;

import java.net.URL;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import static com.gestalt.jbi.xmpp.component.XMPPConsumerHandler.PacketType;

public class XMPPConsumerHandlerTest extends XMLTestCase {
    private static final String WSDL_RESOURCE = "/XmppForecastSearchBotWSDL.wsdl";
    private static final QName SERVICE_NAME = new QName("http://j2ee.netbeans.org/wsdl/XmppForecastSearchBotWSDL",
            "XmppForecastSearchBotWSDLService");
    private static final String ENDPOINT = "XmppForecastSearchBotWSDLPort";
    private static final String SIMPLE_RESPONSE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/XmppForecastSearchBotWSDL\" type=\"tns:XmppForecastSearchBotWSDLOperationRequest\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part>from</jbi:part><jbi:part>123</jbi:part><jbi:part>body</jbi:part></jbi:message>";
    private static final String COMPLEX_RESPONSE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/XmppForecastSearchBotWSDL\" type=\"tns:XmppForecastSearchBotWSDLOperationRequest\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part>from</jbi:part><jbi:part>123</jbi:part><jbi:part><tag xmlns=\"somens\"><anotherTag>body</anotherTag></tag></jbi:part></jbi:message>";

    public void testSimpleBody() throws Exception {
        String doc = "body";
        assertXMLEqual(SIMPLE_RESPONSE, wrapMessage(doc,PacketType.message));
    }

    public void testComplexBody() throws Exception {
        String doc = "<tag xmlns=\"somens\"><anotherTag>body</anotherTag></tag>";
        assertXMLEqual(COMPLEX_RESPONSE,wrapMessage(doc,PacketType.message));
    }

    public void testPresence() throws Exception {
        String doc = "body";
        
        assertXMLEqual(SIMPLE_RESPONSE, wrapMessage(doc,PacketType.presence));
    }

    private String wrapMessage(String body, PacketType type ) throws Exception {
        URL x = this.getClass().getResource(WSDL_RESOURCE);
        assertNotNull(x);

        File f = new File(x.getFile());

        XMPPConsumerHandler test = new XMPPConsumerHandler(createEndpoint(
                    SERVICE_NAME, null, ENDPOINT, null, f, null, null));
        XMPPOperationInput input = new XMPPOperationInput();
        input.setJabberId("frompart");
        input.setPacketId("idpart");
        input.setMessage("messagepart");
        input.setPacketType("");

        Document doc = test.wrapMessage(input, "from", body, "123",
                type);

        return docToString(doc);
    }

    private String docToString(Document doc) throws Exception {
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(doc);
        StringWriter sw = new StringWriter();
        StreamResult result = new StreamResult(sw);
        transformer.transform(source, result);

        return sw.toString();
    }

    protected XMPPEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        XMPPWSDLDeployer dep = new XMPPWSDLDeployer(null);

        return new XMPPEndpoint(serviceName, interfaceName, endpointName, role,
            dep.getWsdlDocument(wsdl), dep.getWsdlDefinition(wsdl),
            serviceUnit, false);
    }
}
