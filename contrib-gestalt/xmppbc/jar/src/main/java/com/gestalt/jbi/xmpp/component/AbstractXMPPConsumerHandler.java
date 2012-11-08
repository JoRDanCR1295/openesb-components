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

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.XMPPError;

import org.w3c.dom.Document;

import java.util.logging.Level;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

public abstract class AbstractXMPPConsumerHandler
        extends AbstractMessageExchangeHandler {

    private Message message;

    public AbstractXMPPConsumerHandler(Endpoint arg0) {
        super(arg0);
    }

    protected void processDone() {
        try {
            sendMessage(null, null, null, null, "");
        } catch (Exception e) {
            log.log(Level.WARNING, e.getMessage());
        }
    }

    protected void processError(Exception exception) {
        if (exception != null) {
            log.log(Level.WARNING, "Unknown error - " + exception.getMessage());
        }
    }

    protected void processFault(Fault fault) {
        // TODO a sender should error with a 400 series - 500 is for everything
        // else
        // TODO convert the fault content into a string
        try {
            sendMessage((String) fault.getProperty(
                    Properties.XMPP_FROM.toString()),
                    (String) fault.getProperty(
                    Properties.XMPP_MESSAGE_ID.toString()), Message.Type.error,
                    new XMPPError(XMPPError.Condition.resource_constraint), null);
        } catch (Exception e) {
            log.log(Level.WARNING, e.getMessage());
        }
    }

    protected void processMessage() {
    }

    private XMPPConnection getXMPPConnection() {
        return ((XMPPEndpoint) endpoint).getXmppConnection();
    }

    protected void sendMessage(String to, String id, Message.Type type,
            XMPPError error, String payload) throws Exception {
        //TODO:  Need to figure out where the MessageListener is involved.
        Chat chat = getXMPPConnection().getChatManager().createChat(to, null);
        Message message = new Message();

        if (Message.Type.error.equals(type)) {
            message.setType(type);
            message.setError(error);
        }

        message.setPacketID(id);
        message.setBody(payload);
        log.log(Level.INFO,
                "XMPP Consumer about to send message[" + to + " : " + id + " : " +
                payload + "]");
        chat.sendMessage(message);
    }

    @SuppressWarnings("unchecked")
    protected void process(String to, String from, String packetId,
            String body, PacketType type) {
        log.log(Level.INFO,
                "XMPP Consumer processing message[" + to + " : " + from + " : " +
                packetId + " : " + body + "]");

        try {
            final QName fullServiceName = endpoint.getServiceName();
            final String endpointName = endpoint.getEndpointName();
            final ServiceEndpoint serviceEndpoint = ((AbstractComponentLifeCycle) endpoint.getServiceUnit().getComponent().getLifeCycle()).getComponentContext().getEndpoint(fullServiceName,
                    endpointName);

            final QName operation = ((QName[]) endpoint.getOperations().keySet().toArray(new QName[]{}))[0];

            //todo why is this in here when op is assigned again later?
            //            Map<Object, PortType> map = (Map<Object, PortType>) endpoint.getWsdlDefinition()
            //                                                                        .getPortTypes();
            //            PortType pt = map.get(map.keySet().iterator().next());
            //            Operation op = (Operation) pt.getOperations().get(0);
            MessageExchange exchange;

            exchange = channel.createExchangeFactoryForService(serviceEndpoint.getServiceName()).createInOnlyExchange();
            exchange.setEndpoint(serviceEndpoint);
            exchange.setOperation(operation);

            NormalizedMessage in = exchange.createMessage();

            XMPPOperation xmppOperation = ((XMPPEndpoint) endpoint).getOperations().get(operation);
            XMPPOperationInput xmppOperationInput = ((XMPPEndpoint) endpoint).getXMPPOperationInput(xmppOperation);

            try {
                Document message = wrapMessage(xmppOperationInput, from, body,
                        packetId, type);
                in.setContent(new DOMSource(message));
            } catch (Exception e) {
                sendError(e);

                return;
            }

            exchange.setMessage(in, "in");
            log.log(Level.INFO, "XMPP Consumer sending message to jbi service");
            sendSync(exchange);
        } catch (Exception e) {
            log.log(Level.WARNING, e.getMessage());

            // TODO look at the spec again to make sure this is a 400 and not a
            // 500
            try {
                sendMessage(from, packetId, Message.Type.error,
                        new XMPPError(XMPPError.Condition.unexpected_request),
                        e.getMessage());
            } catch (Exception e1) {
                log.log(Level.WARNING, e1.getMessage());
            }
        }
    }

    public void setMessage(Message message) {
        this.message = message;
    }

    public void run() {
        processMessage(message);
    }

    private void processMessage(Message message) {
        process(message.getTo(), message.getFrom(), message.getPacketID(),
                message.getBody(), PacketType.message);
    }

    public void processPresenceNotification(Presence presence) {
        process(null, presence.getFrom(), presence.getPacketID(),
                presence.getStatus(), PacketType.presence);
    }

    public void processStatusNotification(String user, String roomName,
            String action) {
        process(null, user, null,
                buildStatusMessage(user, action, roomName).toString(),
                PacketType.presence);
    }

    private StringBuilder buildStatusMessage(String user, String action,
            String roomName) {
        StringBuilder builder = new StringBuilder();
        builder.append(user);
        builder.append(" has ");
        builder.append(action);
        builder.append(" ");
        builder.append(roomName);

        return builder;
    }

    protected abstract Document wrapMessage(XMPPOperationInput input,
            String from, String body, String packetId, PacketType type)
            throws Exception;

    protected static enum Properties {

        XMPP_FROM, XMPP_MESSAGE_ID;
    }

    protected static enum PacketType {

        presence, message;
    }
}
