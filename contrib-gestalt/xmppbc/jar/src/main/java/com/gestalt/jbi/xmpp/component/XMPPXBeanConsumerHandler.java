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

import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.xml.sax.InputSource;

import java.io.File;
import java.io.StringReader;

import java.net.URL;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;


public class XMPPXBeanConsumerHandler extends AbstractXMPPConsumerHandler {
    private static final String MESSAGE_TAG = "message";
    private static final String PRESENCE_TAG = "presence";
    private static final String FROM = "jabberId";
    private static final String ID = "from";
    private static final String NS = "http://com.gestalt.jbi/xmpp";

    public XMPPXBeanConsumerHandler(Endpoint endpoint) {
        super(endpoint);
    }

    protected Document wrapMessage(XMPPOperationInput input, String from,
        String body, String packetId, PacketType type)
        throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        /*
        todo possibly validate against schema 
        URL file = this.getClass().getResource("/xmpp.xsd");

        if (null != file) {
            Schema schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
                                         .newSchema(new File(file.getFile()));
            factory.setSchema(schema);
            factory.setValidating(true);
        }
        */

        DocumentBuilder builder = factory.newDocumentBuilder();
        Document response = builder.newDocument();

        Element top;

        switch (type) {
        case message:
            top = response.createElementNS(NS, MESSAGE_TAG);

            break;

        case presence:
            top = response.createElementNS(NS, PRESENCE_TAG);

            break;

        default:
            throw new UnsupportedOperationException("Unsupported packet type: " +
                type);
        }

        top.setAttribute(FROM, from);

        Element bodyEl = response.createElementNS(NS, "body");

        try {
            Document document = builder.parse(new InputSource(
                        new StringReader(body)));
            Element content = document.getDocumentElement();
            bodyEl.appendChild(response.importNode(content, true));
        } catch (Exception e) {
            bodyEl.setTextContent(body);
        }

        top.appendChild(bodyEl);

        response.appendChild(top);

        return response;
    }
}
