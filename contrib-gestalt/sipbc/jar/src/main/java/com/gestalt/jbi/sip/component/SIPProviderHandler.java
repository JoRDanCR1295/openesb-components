/**
 *   sip-binding-component - SIP Binding Component
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
package com.gestalt.jbi.sip.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.sip.SIPConnection;
import com.gestalt.jbi.sip.extensions.SIPOperation;
import com.gestalt.jbi.sip.extensions.SIPOperationInput;
import com.sun.jbi.internationalization.Messages;

import org.xmlpull.mxp1.MXParser;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;


/**
 * @author : csturtz
 */
public class SIPProviderHandler extends AbstractMessageExchangeHandler {
    private static final Logger log = Messages.getLogger(SIPProviderHandler.class);
    private static Messages messages = Messages.getMessages(SIPProviderHandler.class);
    SIPConnection sipConnection = null;
    private QName messageType;
    private List<String> wrappedParts = new ArrayList<String>();

    public SIPProviderHandler(Endpoint endpoint) {
        super(endpoint);
        sipConnection = ((SIPEndpoint) endpoint).getSipConnection();
    }

    protected void processError(Exception exception) {
        // To change body of implemented methods use File | Settings | File
        // Templates.
    }

    protected void processDone() {
        // To change body of implemented methods use File | Settings | File
        // Templates.
    }

    protected void processMessage() {
        String pattern = exchange.getPattern().toString().trim();
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the message Exchange "
                    + exchange.getExchangeId() + ";the pattern is " + pattern);
        }
        if (MEP.IN_ONLY.toString().equalsIgnoreCase(pattern)) {
            processInOnly((InOnly) exchange, (SIPEndpoint) endpoint);
        } else {
            log.log(Level.WARNING, messages.getString("SIPBC-W00429.SIPBCSupportsINONLYOnly"));
        }
    }

    protected void processFault(Fault fault) {
        // To change body of implemented methods use File | Settings | File
        // Templates.
    }

    /**
     * This method is called when we receive an IN_ONLY message off the NMR.
     *
     * @param inOnly
     * @param endpoint
     */
    private void processInOnly(InOnly inOnly, SIPEndpoint endpoint) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing an IN_ONLY Message");
        }

        boolean success = false;

        QName operationQName = inOnly.getOperation();
        SIPOperation operation = endpoint.getSIPOperations().get(operationQName);
        SIPOperation.OperationName operationName = operation.getOperationName();

        SIPOperationInput input = endpoint.getSIPOperationInput(operation);
        Message wsdlMessageDefinition = null;

        try {
            NormalizedMessage in = inOnly.getInMessage();
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);
            parseContent(result.getWriter().toString());

            Definition wsdl = endpoint.getWsdlDefinition();
            wsdlMessageDefinition = wsdl.getMessage(messageType);

            if (wsdlMessageDefinition == null) {
                throw new Exception("Error processing SIP Provider InOnly");
            }
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00430.exceptionProcessingTheINONLYMessage"),e);
            sendError(e);
        }

        if (operationName.equals(SIPOperation.OperationName.sendRequest)) {
            success = processSendRequest(input, wsdlMessageDefinition);
        } else if (operationName.equals(SIPOperation.OperationName.sendResponse)) {
            success = processSendResponse(input, wsdlMessageDefinition);
        } else {
            log.severe(
                "Error the processing SIP Provider's InOnly - Unsupported Operation: " +
                operationName);
            sendError(new Exception("Unsupported Operation: " + operationName));
        }

        try {
            if (success) {
                sendDone();
            } else {
                sendError(new Exception("Error performing the requested operation"));
            }
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00431.errorSendingTheDONEMessage"), e);
            sendError(e);
        }
    }

    /**
     * When a message is received from the NMR with operation name equal to
     * 'sendRequest', this method will be called.
     *
     * @param input
     * @param wsdlMessageDefinition
     * @return
     */
    private boolean processSendRequest(SIPOperationInput input,
        Message wsdlMessageDefinition) {
        String requestMethod = getPartValue(input.getRequestMethod(),
                wsdlMessageDefinition);
        String content = getPartValue(input.getContent(), wsdlMessageDefinition);
        String remoteUri = getPartValue(input.getRemoteUri(),
                wsdlMessageDefinition);

        return sipConnection.sendRequest(requestMethod, remoteUri, content);
    }

    /**
     * When a message is received from the NMR with operation name equal to
     * 'sendResponse', this method will be called.
     *
     * @param input
     * @param wsdlMessageDefinition
     * @return
     */
    private boolean processSendResponse(SIPOperationInput input,
        Message wsdlMessageDefinition) {
        String responseMethod = getPartValue(input.getResponseMethod(),
                wsdlMessageDefinition);
        Integer responseStatus = Integer.valueOf(getPartValue(
                    input.getResponseStatus(), wsdlMessageDefinition));
        String remoteUri = getPartValue(input.getRemoteUri(),
                wsdlMessageDefinition);
        String content = getPartValue(input.getContent(), wsdlMessageDefinition);

        return sipConnection.sendResponse(responseMethod, remoteUri,
            responseStatus, content);
    }

    private void parseContent(String content) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE, "Denormalizing Message: " + content);
        }
        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(content));
            parseContent(xpp);
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00432.exceptionParsingTheContent"),e);
        }
    }

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

    private void parseContent(XmlPullParser xpp)
        throws XmlPullParserException, IOException {
        boolean done = false;
        messageType = null;
        wrappedParts.clear();

        while (!done) {
            int eventType = xpp.next();

            if (eventType == XmlPullParser.START_TAG) {
                if ("message".equals(xpp.getName())) {
                    String type = xpp.getAttributeValue(null, "type");
                    messageType = resolveQName(type, xpp);
                } else if ("part".equals(xpp.getName())) {
                    String partText = xpp.nextText();
                    wrappedParts.add(partText);
                }
            } else if (eventType == XmlPullParser.END_DOCUMENT) {
                done = true;
            }
        }
    }

    private QName resolveQName(String value, XmlPullParser xpp) {
        String[] parts = value.split(":");

        if (parts.length > 1) {
            return new QName(xpp.getNamespace(parts[0]), parts[1], parts[0]);
        }

        return new QName(null, value, null);
    }
}
