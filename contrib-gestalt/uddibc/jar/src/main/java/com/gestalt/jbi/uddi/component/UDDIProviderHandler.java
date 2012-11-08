/**
 *   uddi-binding-component - UDDI Binding Component
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
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.nmr.NmrWrapperUtils;
import com.gestalt.jbi.uddi.component.uddi.UDDIUtils;
import com.gestalt.jbi.uddi.extensions.UDDIOperation;
import com.gestalt.jbi.uddi.extensions.UDDIOperationInput;

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
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import javax.wsdl.*;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


public class UDDIProviderHandler extends AbstractMessageExchangeHandler {
    protected static final Logger log = Logger.getLogger(UDDIProviderHandler.class.getName());
    public static final String SERVICE_LIST = "ServiceList";
    public static final String SERVICE = "Service";
    private QName messageType;
    private List<String> wrappedParts = new ArrayList<String>();
    private UDDIUtils uddiUtils;

    public UDDIProviderHandler(Endpoint endpoint) {
        super(endpoint);
    }

    @Override
    protected void processDone() {
    }

    @Override
    protected void processError(Exception ex) {
    }

    @Override
    protected void processFault(Fault fault) {
    }

    @Override
    protected void processMessage() {
        String pattern = exchange.getPattern().toString().trim();

        if (MEP.IN_OUT.toString().equals(pattern)) {
            processInOut((InOut) exchange, (UDDIEndpoint) endpoint);
        } else {
            log.warning("UDDI Binding supports INOUT only");
        }
    }

    private void processInOut(InOut inOut, UDDIEndpoint uddiEndpoint) {
        if (log.isLoggable(Level.FINE)) {
            log.fine("Processing InOut message " + inOut.toString());
        }

        String businessName = null;
        String serviceName = null;

        NormalizedMessage in = inOut.getInMessage();

        try {
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);

            if (log.isLoggable(Level.FINE)) {
                log.fine("Incoming Request Content: " +
                    result.getWriter().toString());
            }

            //TODO use ojc-component-common
            QName operationName = inOut.getOperation();
            log.fine("OperationName is: " + operationName);

            UDDIOperation operation = ((UDDIEndpoint) endpoint).getUDDIOperations()
                                       .get(operationName);
            log.fine("Operation is: " + operation);

            UDDIOperationInput input = ((UDDIEndpoint) endpoint).getUDDIOperationInput(operation);
            log.fine("Operation Input is: " + input);
            log.fine("Business Name part is: " + input.getBusinessName());
            log.fine("Service Name part is: " + input.getServiceName());

            parseContent(result.getWriter().toString());
            log.fine("Final message type: " + messageType);
            log.fine("Wrapped Part Values: " + wrappedParts);

            Definition wsdl = endpoint.getWsdlDefinition();
            Message wsdlMessageDefinition = wsdl.getMessage(messageType);
            log.fine("WSDL Message: " +
                wsdlMessageDefinition.getOrderedParts(null));

            businessName = getPartValue(input.getBusinessName(),
                    wsdlMessageDefinition);
            serviceName = getPartValue(input.getServiceName(),
                    wsdlMessageDefinition);
        } catch (Exception e) {
            e.printStackTrace();
            sendError(e);

            return;
        }

        List<String> services = null;

        try {
            log.fine("Business name=" + businessName + "; Service name=" +
                serviceName);
            services = uddiUtils.lookupService(serviceName, businessName,
                    uddiEndpoint.getInquiryProxy());
            log.fine("Query returned: " + services);
        } catch (Exception e) {
            e.printStackTrace();
            sendError(e);

            return;
        }

        NormalizedMessage out = null;

        try {
            out = exchange.createMessage();
        } catch (MessagingException e) {
            sendError(e);

            return;
        }

        QName messageType = null;
        String replyPartType = null;
        Service service = endpoint.getWsdlDefinition()
                                  .getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName())
                                         .getLocalPart());
        PortType portType = port.getBinding().getPortType();

        Operation op = (Operation) portType.getOperations().get(0);
        messageType = op.getOutput().getMessage().getQName();

        // determine what the reply message should look like
        log.fine("Parts: " + op.getOutput().getMessage().getParts());

        Object[] objects = op.getOutput().getMessage().getParts().values()
                             .toArray();

        if (objects.length != 0) {
            Part replyPart = (Part) objects[0];
            replyPartType = replyPart.toString();
            log.fine("reply part type is:" + replyPartType);
        } else {
            throw new RuntimeException(
                "UDDI WSDL does not contain a reply message part");
        }

        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(messageType, null);

        if (!services.isEmpty() && (null != services)) {
            // if the wsdl is asking for a wsaext:EndpointReferenceList
            if (replyPartType.contains("EndpointReferenceList")) {
                log.fine("returning multiple addresses");

                UDDINormalizer normalizer = new UDDINormalizer();
                normalizer.createConsumerContent(messageType, services, wrapper);
            } else {
                // else return the url as a simple sting
                log.fine("returning single address");
                wrapper.addPart(null, services.get(0));
            }
        } else {
            wrapper.addEmptyPart();
        }

        try {
            out.setContent(new DOMSource(wrapper.getResult()));
        } catch (Exception e) {
            sendError(e);

            return;
        }

        try {
            inOut.setOutMessage(out);
            send();
        } catch (MessagingException e) {
            sendError(e);
        }
    }

    @SuppressWarnings("unchecked")
    private String getPartValue(String searchName, Message message) {
        List<Part> orderedParts = (List<Part>) message.getOrderedParts(null);

        for (int i = 0; i < orderedParts.size(); i++) {
            Part part = (Part) orderedParts.get(i);

            if (part.getName().equals(searchName)) {
                return wrappedParts.get(i);
            }
        }

        return null;
    }

    private void parseContent(String content) {
        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(content));
            parseContent(xpp);
        } catch (Exception e) {
            log.throwing(getClass().getName(), "parseContent", e);
        }
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

    public UDDIUtils getUddiUtils() {
        return uddiUtils;
    }

    public void setUddiUtils(UDDIUtils uddiUtils) {
        this.uddiUtils = uddiUtils;
    }
}
