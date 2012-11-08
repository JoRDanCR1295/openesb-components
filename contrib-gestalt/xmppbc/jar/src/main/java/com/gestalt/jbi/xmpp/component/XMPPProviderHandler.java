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
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationOutput;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.XMPPError;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xmlpull.mxp1.MXParser;

import org.xmlpull.v1.XmlPullParser;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.io.StringWriter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

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


public class XMPPProviderHandler extends AbstractMessageExchangeHandler
    implements IProviderHandler {
    public static final String WRAPPER_MESSAGE = "jbi:message";
    public static final String WRAPPER_PART = "jbi:part";
    private QName messageType;
    private Message message;
    private List<String> wrappedParts = new ArrayList<String>();

    /**
     * Constructor
     *
     * @param arg0 -
     *            Endpoint
     */
    public XMPPProviderHandler(Endpoint arg0) {
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
            processInOnly((InOnly) exchange, (XMPPEndpoint) endpoint);
        } else {
            log.warning("XMPP Binding only supports INOUT or INONLY");
        }
    }

    /**
     * Handles the processing of an InOut MEP
     *
     * @param inOut -
     *            InOut Message Exchange
     * @param xmppEndpoint -
     *            the XMPPEndpoint
     */
    protected void processInOut(InOut inOut, XMPPEndpoint xmppEndpoint) {
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

            XMPPOperationInput input = ((XMPPEndpoint) endpoint).getXMPPOperationInput(operation);

            parseContentXML(result.getWriter().toString());

            Definition wsdl = endpoint.getWsdlDefinition();
            javax.wsdl.Message wsdlMessageDefinition = wsdl.getMessage(messageType);
            Map<String, String> map = getPartValues(wsdlMessageDefinition, input);
            addJabberIdFromNMProperty(map, inOut, in);
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
     * Processes an InOnly MEP
     *
     * @param inOnly -
     *            InOnly MEP
     * @param xmppEndpoint -
     *            XMPPEndpoint
     */
    protected void processInOnly(InOnly inOnly, XMPPEndpoint xmppEndpoint) {
        try {
            log.log(Level.INFO,
                "XMPP Provider processing an incoming in only message");

            NormalizedMessage in = inOnly.getInMessage();
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);

            QName nmOpQName = inOnly.getOperation();
            XMPPOperation operation = getXMPPOperation(nmOpQName);
            if ( operation == null ) {
                log.fine("XMPP Operation is NULL for NM operation " + nmOpQName);
            }
            XMPPOperationInput input = ((XMPPEndpoint) endpoint).getXMPPOperationInput(operation);

            String content = result.getWriter().toString();
            log.info("InOnly content is: " + content);
            parseContentXML(content);

            Definition wsdl = endpoint.getWsdlDefinition();
            javax.wsdl.Message wsdlMessageDefinition = wsdl.getMessage(messageType);
            Map<String, String> map = getPartValues(wsdlMessageDefinition, input);
            addJabberIdFromNMProperty(map, inOnly, in);
            OperationExecutor.performOperation(operation.getOperationName(),
                map, this);
            sendDone();
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Error: " + e);
            log.log(Level.WARNING, e.getMessage());
            sendError(e);
        }
    }

    private void addJabberIdFromNMProperty(Map<String, String> xmppAttrs, MessageExchange mx, NormalizedMessage nm) {
        String jabberId = null;
        Object value = null;
        if ( mx != null ) {
            value = (String) mx.getProperty(XMPPAttributes.NM_PROP_JABBER_ID);
        }
        // override mx property with nm property
        if ( nm != null ) {
            value = (String) nm.getProperty(XMPPAttributes.NM_PROP_JABBER_ID);
        }
        if ( value != null ) {
            if ( value instanceof String ) {
                jabberId = (String)value;
            } else {
                jabberId = value.toString();
            }
        }
        if ( jabberId != null && xmppAttrs != null) {
            xmppAttrs.put(XMPPAttributes.NM_JABBER_ID, jabberId);
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

    private Map<String, String> getPartValues(javax.wsdl.Message wsldMessage,
        XMPPOperationInput input) {
        Map<String, String> map = new HashMap<String, String>();
        String message = getPartValue(input.getMessage(), wsldMessage);
        String jabberId = getPartValue(input.getJabberId(), wsldMessage);
        String staticJabberId = input.getStaticJabberId();
        String groupChat = getPartValue(input.getGroupChat(), wsldMessage);
        String groupList = getPartValue(input.getGroupList(), wsldMessage);
        String userList = getPartValue(input.getUserList(), wsldMessage);
        String roomName = getPartValue(input.getRoomName(), wsldMessage);
        String alias = ((XMPPEndpoint) endpoint).getXMPPAddress().getUsername();

        if (jabberId != null) {
            map.put(XMPPAttributes.JABBER_ID, jabberId);
        }

        if (staticJabberId != null) {
            map.put(XMPPAttributes.STATIC_JABBER_ID, staticJabberId);
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

        map.put(XMPPAttributes.ALIAS, alias);

        return map;
    }

    /**
     * Gets the GroupChatManager for the given XMPPEndpoint
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

    public void setMessage(Message message) {
        this.message = message;
    }

    public Message getMessage() {
        return message;
    }

    /**
     * Gets the part values that are the input from the WSDL
     *
     * @param searchName -
     *            part name to search for
     * @param message -
     *            WSDL Message.
     * @return String - the value of the part.
     */
    @SuppressWarnings("unchecked")
    private String getPartValue(String searchName, javax.wsdl.Message message) {
        List<Part> orderedParts = (List<Part>) message.getOrderedParts(null);

        for (int i = 0; i < orderedParts.size(); i++) {
            Part part = (Part) orderedParts.get(i);

            if (part.getName().equals(searchName)) {
                return wrappedParts.get(i);
            }
        }

        return null;
    }

    /**
     * @param value
     * @param xpp
     * @return QName
     */
    private QName resolveQName(String value, XmlPullParser xpp) {
        String[] parts = value.split(":");

        if (parts.length > 1) {
            return new QName(xpp.getNamespace(parts[0]), parts[1], parts[0]);
        }

        return new QName(null, value, null);
    }

    protected List<String> addPartsXML(String buffer) {
        List<String> parts = new ArrayList<String>();

        try {
            DocumentBuilder builder = DocumentBuilderFactory.newInstance()
                                                            .newDocumentBuilder();
            Document doc = builder.parse(new ByteArrayInputStream(
                        buffer.getBytes()));
            parts = getParts(doc);
        } catch (Exception e) {
            log.throwing(getClass().getName(), "addPartsXML", e);
        }

        return parts;
    }

    /**
     * Searches the given Document for all parts in the message.
     * @param doc - The jbi message.
     * @return - List of all jbi parts.
     */
    private List<String> getParts(Document doc) {
        List<String> list = new ArrayList<String>();
        Element jbiMessageWrapper = doc.getDocumentElement();

        if (jbiMessageWrapper.getTagName().equalsIgnoreCase(WRAPPER_MESSAGE)) {
            NodeList childNodesI = jbiMessageWrapper.getChildNodes();

            for (int i = 0, I = childNodesI.getLength(); i < I; i++) {
                Node nodeI = childNodesI.item(i);

                if (nodeI.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }

                Element jbiPartWrapper = (Element) nodeI;

                if (!jbiPartWrapper.getTagName().equalsIgnoreCase(WRAPPER_PART)) {
                    continue;
                }

                NodeList childNodesJ = jbiPartWrapper.getChildNodes();

                for (int j = 0, J = childNodesJ.getLength(); j < J; j++) {
                    Node nodeJ = childNodesJ.item(j);

                    if (nodeJ.getNodeType() == Node.TEXT_NODE) {
                        list.add(nodeJ.getTextContent());

                        break;
                    } else if (nodeJ.getNodeType() == Node.ELEMENT_NODE) {
                        try {
                            DOMSource source = new DOMSource(nodeJ);
                            StringWriter swWriter = new StringWriter();
                            StreamResult result = new StreamResult(swWriter);
                            Transformer transformer = TransformerFactory.newInstance()
                                                                        .newTransformer();
                            transformer.transform(source, result);
                            list.add(swWriter.getBuffer().toString());
                        } catch (Exception e) {
                            log.warning(e.getMessage());
                        }
                    }
                }
            }
        }

        return list;
    }

    /**
     * Parses the XML content of an incoming message
     *
     * @param content
     */
    private void parseContentXML(String content) {
        parseMessageType(content);

        for (String x : addPartsXML(content)) {
            wrappedParts.add(x);
        }
    }

    private void parseMessageType(String content) {
        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(content));

            boolean done = false;
            messageType = null;

            while (!done) {
                int eventType = xpp.next();

                if (eventType == XmlPullParser.START_TAG) {
                    if ("message".equals(xpp.getName())) {
                        String type = xpp.getAttributeValue(null, "type");
                        messageType = resolveQName(type, xpp);
                    }
                } else if (eventType == XmlPullParser.END_DOCUMENT) {
                    done = true;
                }
            }
        } catch (Exception e) {
            log.throwing(getClass().getName(), "parseMessageType", e);
        }
    }
    private static enum Properties {XMPP_FROM,
        XMPP_MESSAGE_ID;
    }
}
