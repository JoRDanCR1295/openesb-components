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
import com.gestalt.jbi.nmr.NmrWrapperUtils;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;

import org.w3c.dom.Document;

import java.util.List;
import java.util.logging.Logger;

import javax.wsdl.*;

import javax.xml.namespace.QName;


public class XMPPConsumerHandler extends AbstractXMPPConsumerHandler {
    private static Logger log = Logger.getLogger(XMPPConsumerHandler.class.getName());

    public XMPPConsumerHandler(Endpoint arg0) {
        super(arg0);
    }

    protected Document wrapMessage(XMPPOperationInput input, String from,
        String body, String packetId, PacketType type) {
        Service service = endpoint.getWsdlDefinition()
                                  .getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName())
                                         .getLocalPart());
        PortType portType = port.getBinding().getPortType();

        Operation op = (Operation) portType.getOperations().get(0);

        NmrWrapperUtils wrapper = wrapParts(input, op, from, body, packetId,
                type);

        return wrapper.getResult();
    }

    /**
     * Takes the given XMPPOpertionInput and wraps it into a jbi:message
     * @param input - XMPPOperationInput
     * @param op - Operation
     * @param from - Who the message is from
     * @param body - The body of the message
     * @param packetId - The packetId for this message
     * @param type - The PacketType (e.g. presence or message).
     * @return - NmrWrapperUtils - wrapped jbi:message
     */
    private NmrWrapperUtils wrapParts(XMPPOperationInput input, Operation op,
        String from, String body, String packetId, PacketType type) {
        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(op.getInput().getMessage().getQName(), null);

        List<Part> parts = (List<Part>) op.getInput().getMessage()
                                          .getOrderedParts(null);

        switch (type) {
        case presence:
            log.fine("Presence Message: ");

            for (Part part : parts) {
                log.fine("Part is: " + part.getName());

                if (part.getName().equals(input.getJabberId())) {
                    wrapper.addPart(null, from);
                } else if (part.getName().equals(input.getPacketId())) {
                    wrapper.addPart(null, packetId);
                } else if (part.getName().equals(input.getMessage())) {
                    wrapper.addPart(null, body);
                } else if (part.getName().equals(input.getPacketType())) {
                    wrapper.addPart(null, PacketType.presence.toString());
                }
            }

            break;

        case message:
            log.fine("Message message: ");
            log.fine("Parts size: " + parts.size());

            for (Part part : parts) {
                if (part.getName().equals(input.getJabberId())) {
                    wrapper.addPart(null, from);
                } else if (part.getName().equals(input.getPacketId())) {
                    wrapper.addPart(null, packetId);
                } else if (part.getName().equals(input.getMessage())) {
                    try {
                        wrapper.addComplexType(body);
                    } catch (Throwable e) {
                        wrapper.addPart(null, body);
                    }
                } else if (part.getName().equals(input.getPacketType())) {
                    wrapper.addPart(null, PacketType.message.name());
                }
            }

            break;
        }

        return wrapper;
    }
}
