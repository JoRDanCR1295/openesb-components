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
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.nmr.NmrWrapperUtils;
import com.gestalt.jbi.xmpp.component.smack.GroupChatManager;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationOutput;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.XMPPError;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;

import java.io.StringReader;
import java.io.StringWriter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jbi.messaging.*;
import javax.jbi.messaging.Fault;

import javax.wsdl.*;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


/**
 * Implementation of IProviderHandler for use with Servicemix.  Since Servicemix components send
 * non JBI wrapped messages, we have to be able to handle any XML.  We have defined the attributes
 * that are required so that we can associate the values with the appropriate operation attributes.
 */
public class XMPPXBeanProviderHandler extends AbstractMessageExchangeHandler
    implements IProviderHandler {
    private Message message;
    private String BODY = "body";

    /**
     * Constructor
     *
     * @param arg0 -
     *             Endpoint
     */
    public XMPPXBeanProviderHandler(Endpoint arg0) {
        super(arg0);
    }

    /**
     * Handles the processing of a Done message
     */
    protected void processDone() {
        try {
            OperationExecutor.sendMessage(null, null, null, "", false, this);
        } catch (Exception e) {
            log.log(Level.WARNING, e.getMessage());
        }
    }

    /**
     * Process Error messages
     *
     * @param exception
     */
    protected void processError(Exception exception) {
        if (exception != null) {
            log.log(Level.WARNING, "Unknown error - " + exception.getMessage());
        }
    }

    /**
     * Process Fault messages
     *
     * @param fault
     */
    protected void processFault(Fault fault) {
        // TODO a sender should error with a 400 series - 500 is for everything
        // else
        // TODO convert the fault content into a string
        try {
            OperationExecutor.sendMessage((String) fault.getProperty(
                    Properties.XMPP_FROM.toString()), Message.Type.error,
                new XMPPError(XMPPError.Condition.interna_server_error), null,
                false, this);
        } catch (Exception e) {
            log.log(Level.WARNING, e.getMessage());
        }
    }

    /**
     * Process the incoming MessageExchange
     */
    protected void processMessage() {
        String pattern = exchange.getPattern().toString().trim();

        if (MEP.IN_OUT.toString().equals(pattern)) {
            processInOut((InOut) exchange, (XMPPEndpoint) endpoint);
        } else if (MEP.IN_ONLY.toString().equals(pattern)) {
            processInOnly((InOnly) exchange);
        } else {
            log.warning("XMPP Binding only supports INOUT or INONLY");
        }
    }

    /**
     * Process an InOut Message Exchange.
     * @param inOut - The InOut Message Exchange.
     * @param xmppEndpoint - The XMPPEndpoint.
     */
    private void processInOut(InOut inOut, XMPPEndpoint xmppEndpoint) {
        try {
            log.log(Level.INFO,
                "XMPP Provider processing an incoming in out message");

            NormalizedMessage in = inOut.getInMessage();
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);

            QName nmOpQName = inOut.getOperation();
            XMPPOperation operation = getXMPPOperation(nmOpQName);
            if ( operation == null ) {
                log.fine("XMPP Operation is NULL for NM operation " + nmOpQName);
            }

            String content = result.getWriter().toString();
            Map<String, String> map = parseXML(content);
            String response = OperationExecutor.performOperation(operation.getOperationName(),
                    map, this);

            process(response);
        } catch (Exception e) {
            e.printStackTrace();
            log.log(Level.WARNING, e.getMessage());
            sendError(e);
        }
    }

    /**
     * Process the the "out" part of the InOut MEP
     * @param body - Response to send back.  True if we successfully created
     * or destroyed a group, False otherwise.
     */
    @SuppressWarnings("unchecked")
    private void process(String body) {
        log.log(Level.INFO,
            "XMPP Provider continuing processing of message[" + body + "]");

        try {
            NormalizedMessage out = null;

            try {
                out = exchange.createMessage();
            } catch (MessagingException e) {
                sendError(e);

                return;
            }

            QName type = null;
            Operation op = null;

            try {
                Service service = endpoint.getWsdlDefinition()
                                          .getService(endpoint.getServiceName());
                Port port = service.getPort(QName.valueOf(
                            endpoint.getEndpointName()).getLocalPart());
                PortType portType = port.getBinding().getPortType();

                op = (Operation) portType.getOperations().get(0);
                type = op.getOutput().getMessage().getQName();
            } catch (Exception e) {
                log.log(Level.WARNING, e.getMessage());
            }

            NmrWrapperUtils wrapper = new NmrWrapperUtils();
            wrapper.init(type, null);

            XMPPOperation xmppOperation = ((XMPPEndpoint) endpoint).getOperations()
                                           .get(exchange.getOperation());
            XMPPOperationOutput xmppOperationOutput = ((XMPPEndpoint) endpoint).getXMPPOperationOutput(xmppOperation);

            List<Part> parts = (List<Part>) op.getOutput().getMessage()
                                              .getOrderedParts(null);

            for (Part part : parts) {
                if (part.getName().equals(xmppOperationOutput.getMessage())) {
                    try {
                        wrapper.addComplexType(body);
                    } catch (Throwable e) {
                        wrapper.addPart(null, body);
                    }
                }
            }

            try {
                out.setContent(new DOMSource(wrapper.getResult()));
            } catch (Exception e) {
                sendError(e);

                return;
            }

            exchange.setMessage(out, "out");
            send();
        } catch (Exception e) {
            e.printStackTrace();
            // TODO look at the spec again to make sure this is a 400 and not a
            // 500
            log.log(Level.WARNING, e.getMessage());
            sendError(e);
        }
    }

    /**
     * Prociess an InOnly MEP
     * @param inOnly - The InOnly message exchange.
     */
    private void processInOnly(InOnly inOnly) {
        try {
            NormalizedMessage in = inOnly.getInMessage();
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();

            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);

            String content = result.getWriter().toString();
            Map<String, String> map = parseXML(content);

            QName x = new QName(inOnly.getEndpoint().getEndpointName());

            String operationName = null;

            if (null != map.get(XMPPAttributes.OPERATION_NAME)) {
                operationName = map.get(XMPPAttributes.OPERATION_NAME);
            }
            /* todo on Servicemix why is this returning the endpoint name?
            else if (null != inOnly.getOperation()) {
                operationName = inOnly.getOperation().getLocalPart();
            }
            */
            else if (null != endpoint.getOperations()) {
                operationName = ((XMPPEndpoint) endpoint).getXMPPOperations()
                                 .get(x).getOperationName().toString();
            }
            else {
                throw new Exception("No operation specified");
            }

            OperationExecutor.performOperation(XMPPOperation.OperationName.valueOf(
                    operationName), map, this);
            sendDone();
        } catch (Exception e) {
            log.log(Level.SEVERE, "processInOnly faulted", e);
        }
    }

    /**
     * returns XMPPOperation object by looking up the operation localname in
     * the operation map maintained by the endpoint.
     *
     * @param nmOperationName
     * @return
     */
    private XMPPOperation getXMPPOperation(QName nmOperationName) {
        XMPPOperation xmppOp = null;
        QName xmppOpQName = null;
        // map contains key as a opQName without the namespace.
        Map<QName, XMPPOperation> xmppOpMap = ((XMPPEndpoint) endpoint).getXMPPOperations();
        // get the xmpp op qname.
        if (nmOperationName != null ) {
            xmppOpQName = new QName(nmOperationName.getLocalPart());
        }
        xmppOp = xmppOpMap.get(xmppOpQName);

        return xmppOp;
    }

    /**
     * Parse the XML and set the name value pairs in the map.
     * @param content - The content of the Normalized Message.
     * @return - Map<String, String> of name value pairs for the given attributes in the content.
     */
    private Map<String, String> parseXML(String content) {
        Map<String, String> map = new HashMap<String, String>();
        Document doc = null;

        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            InputSource input = new InputSource(new StringReader(content));
            doc = builder.parse(input);
        } catch (Exception e) {
            log.warning("Error creating Document: " + e.getMessage());
        }

        String jabberId = extractData(doc, XMPPAttributes.JABBER_ID);
        String message = extractData(doc, XMPPAttributes.MESSAGE);
        String groupChat = extractData(doc, XMPPAttributes.GROUP_CHAT);
        String groupList = extractData(doc, XMPPAttributes.GROUP_LIST);
        String userList = extractData(doc, XMPPAttributes.USER_LIST);
        String roomName = extractData(doc, XMPPAttributes.ROOM_NAME);
        String operationName = extractData(doc, XMPPAttributes.OPERATION_NAME);
        String alias = ((XMPPEndpoint) endpoint).getXMPPAddress().getUsername();

        if (jabberId != null) {
            map.put(XMPPAttributes.JABBER_ID, jabberId);
        }

        if (message != null) {
            map.put(XMPPAttributes.MESSAGE, message);
        }

        if (groupChat != null) {
            map.put(XMPPAttributes.GROUP_CHAT, groupChat);
        }

        if (groupList != null) {
            map.put(XMPPAttributes.GROUP_LIST, groupList);
        }

        if (userList != null) {
            map.put(XMPPAttributes.USER_LIST, userList);
        }

        if (roomName != null) {
            map.put(XMPPAttributes.ROOM_NAME, roomName);
        }

        if (operationName != null) {
            map.put(XMPPAttributes.OPERATION_NAME, operationName);
        }

        map.put("alias", alias);

        return map;
    }

    /**
     * Takes the content and extracts the value out of the xml for the given attribute.
     * The expected xml looks like the following:
     *
     *<code>
        <message jabberId='bob@localhost/spark' operationName='sendMessage' groupChat='' groupList='' userList = '' roomName=''>
            <body>my message goes here</body>
        </message>
     *</code>
     *
     * @param doc - The Normalized Message Content.
     * @param attribute - The attribute to search for (e.g. jabberId)
     * @return - The value of the extrated data (e.g. cgallemore@localhost/Spark).
     */
    private String extractData(Document doc, String attribute) {
        String message = "message";
        NodeList list = doc.getElementsByTagName(message);
        Node node = list.item(0);

        if ((null != node) && node.getNodeName().equalsIgnoreCase(message)) {
            if (attribute.equalsIgnoreCase(message)) {
                NodeList n = node.getChildNodes();

                for (int i = 0; i < n.getLength(); i++) {
                    if (n.item(i).getNodeName().equals(BODY)) {
                        Node bodyNode = n.item(i);

                        if (bodyNode.getNodeType() == Node.ELEMENT_NODE) {
                            try {
                                DOMSource source = new DOMSource(bodyNode);
                                StringWriter stringWriter = new StringWriter();
                                StreamResult result = new StreamResult(stringWriter);
                                Transformer transformer = TransformerFactory.newInstance()
                                                                            .newTransformer();
                                transformer.transform(source, result);

                                String content = result.getWriter().toString();

                                return getBodyContent(content);
                            } catch (Exception e) {
                                log.warning("Error getting Content");
                            }
                        }
                    }
                }
            } else {
                NamedNodeMap map = node.getAttributes();
                Node attributeValue = map.getNamedItem(attribute);

                if (attributeValue != null) {
                    return attributeValue.getNodeValue();
                }
            }
        }

        return null;
    }

    private String getBodyContent(String content) {
        String data = null;
        String pattern1 = "<\\w*?" + BODY + ">";
        String pattern2 = "</\\w*?" + BODY + ">";
        Pattern p1 = Pattern.compile(pattern1);
        Pattern p2 = Pattern.compile(pattern2);
        Matcher m1 = p1.matcher(content);
        Matcher m2 = p2.matcher(content);

        if (m1.find() && m2.find()) {
            System.out.println("found match");
            data = content.substring(m1.end(), m2.start());
        }

        return data;
    }

    /**
     * Gets the GroupChatManager for the given XMPPEndpoint
     *
     * @return - GroupChatManager
     */
    public GroupChatManager getGroupChatManager() {
        return ((XMPPEndpoint) endpoint).getGroupChatManager();
    }

    /**
     * Gets the XMPPConnection provided by XMPPEndpoint.
     *
     * @return XMPPConnection
     */
    public XMPPConnection getXMPPConnection() {
        return ((XMPPEndpoint) endpoint).getXmppConnection();
    }

    //    /**
    //     * Creates a new XMPP Message
    //     *
    //     * @param to      -
    //     *                Who the message is to
    //     * @param type    -
    //     *                Message.Type
    //     * @param error   -
    //     *                XMPPError
    //     * @param payload -
    //     *                Content of the Message
    //     * @param store   -
    //     * @return - Message - an XMPP Message
    //     */
    //    private Message getMessage(String to, Message.Type type, XMPPError error,
    //        String payload, boolean store) {
    //        Message message = new Message();
    //
    //        if (Message.Type.error.equals(type)) {
    //            message.setType(type);
    //            message.setError(error);
    //        }
    //
    //        message.setBody(payload);
    //
    //        if (store) {
    //            XMPPEndpoint.registerWaitingHandler(message.getPacketID(), to, this);
    //        }
    //
    //        return message;
    //    }
    //
    //    /**
    //     * Sends a message to another client
    //     *
    //     * @param to      -
    //     *                Who the message is to
    //     * @param type    -
    //     *                Message Type
    //     * @param error   -
    //     *                XMPPError
    //     * @param payload -
    //     *                Content of the message.
    //     * @param store   -
    //     * @throws Exception
    //     */
    //    protected void sendMessage(String to, Message.Type type, XMPPError error,
    //        String payload, boolean store) throws Exception {
    //        Chat chat = getXMPPConnection().getChatManager().createChat(to, null);
    //        Message message = getMessage(to, type, error, payload, store);
    //        log.log(Level.INFO,
    //            "XMPP Provider about to send message[" + to + " : " +
    //            message.getPacketID() + " : " + payload + "]");
    //        chat.sendMessage(message);
    //    }
    public void setMessage(Message message) {
        this.message = message;
    }

    public Message getMessage() {
        return message;
    }
    private static enum Properties {XMPP_FROM, XMPP_MESSAGE_ID;
    }
}
