/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HL7Normalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.Source;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.hl7bc.extensions.HL7Message;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderException;
import static com.sun.jbi.hl7bc.util.XmlUtil.*;
import com.sun.jbi.hl7bc.I18n;

/**
 * This is the class that normalizes a hl7 bc input message
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class HL7Normalizer {

    private Transformer mTrans;

    private WrapperBuilder wrapperBuilder;

    public HL7Normalizer() throws Exception {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (WrapperProcessingException ex) {
            throw new Exception(I18n.msg("E0171: Exception when creating wrapper builder in HL7Normalizer, e=[{0}]",
                    ex.getMessage()), ex);
        }

        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(I18n.msg("E0172: Exception when creating transformer in HL7Normalizer, e=[{0}]",
                    ex.getMessage()), ex);
        }
    }

    /**
     * Given hl7 payload and HL7Message extensibility element information, this method converts the
     * hl7 payload to NormalizedMessage using the Encoder configured for the HL7Message part
     * 
     * @param exchange The MessageExchange.
     * @param operationName The HL7 Operation Name
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param hl7Message The HL7Message extensibility element
     * @param data The hl7 payload
     * @throws Exception upon error.
     */

    public NormalizedMessage normalize(MessageExchange exchange,
                                       QName operationName,
                                       Endpoint endpoint,
                                       HL7Message hl7Message,
                                       String data) throws Exception {
        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

        /**
         * Locate the operation we are interested in. There may be multiple operations by the same
         * name (operation overloading) and the WSDL spec does not allow it. The uniqueness should
         * be guaranteed by the examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time being, we will assume
         * that we don't have operation overloading.
         */
        Message wsdlMessage = null;
        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                /**
                 * In case of outbound case, get the message from operation's output Otherwise get
                 * the message from operation's input
                 */
                if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND
                        && (op.getInput() != null && op.getOutput() != null)) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new Exception(I18n.msg(
                    "E0168: Encountered invalid message type definition for HL7 BC operation '{0}'.", operationName));
        }

        // WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
        wrapperBuilder.initialize(null, wsdlMessage, null);

        return buildMessagePayload(exchange, wsdlMessage, partMappings, wrapperBuilder, hl7Message, data);
    }

    /**
     * Given hl7 payload and HL7Message extensibility element information, this method converts the
     * hl7 payload to javax.xml.transform.Source using the Encoder configured for the HL7Message
     * part
     * 
     * @param operationName The HL7 Operation Name
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param hl7Message The HL7Message extensibility element
     * @param data The hl7 payload
     * @throws Exception upon error.
     */
    public Source normalize(QName operationName, Endpoint endpoint, HL7Message hl7Message, String data)
            throws Exception {
        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

        /**
         * Locate the operation we are interested in. There may be multiple operations by the same
         * name (operation overloading) and the WSDL spec does not allow it. The uniqueness should
         * be guaranteed by the examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time being, we will assume
         * that we don't have operation overloading.
         */
        Message wsdlMessage = null;
        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                /**
                 * In case of outbound case, get the message from operation's output Otherwise get
                 * the message from operation's input
                 */
                if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND
                        && (op.getInput() != null && op.getOutput() != null)) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new Exception(I18n.msg(
                    "E0168: Encountered invalid message type definition for HL7 BC operation '{0}'.", operationName));
        }

        wrapperBuilder.initialize(null, wsdlMessage, null);

        return buildMessagePayloadSource(wsdlMessage, partMappings, wrapperBuilder, hl7Message, data);
    }

    private Source buildMessagePayloadSource(Message wsdlMessage,
                                             Map partMappings,
                                             WrapperBuilder wrapperBuilder,
                                             HL7Message hl7Message,
                                             String data) throws SAXException, IOException, Exception {

        /**
         * Assumption: one message part for each hl7 operation only. Otherwise, the "part" attribute
         * for hl7:message element is required. This assumption will be enforced as part of WSDL
         * validation at design time.
         */
        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();

        if (!(hl7Message instanceof HL7Message)) {
            throw new Exception(I18n.msg("E0173: Invalid Extensibility Element found: [{0}]", hl7Message));
        }

        String use = hl7Message.getUseType();
        String partName = hl7Message.getPart();

        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        // It is safe to assume all WSDL validation are done by now.
        if (use.equals(HL7Message.ATTR_USE_TYPE_ENCODED)) {
            Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
            if (encoder == null) {
                throw new Exception(
                        I18n.msg("E0170: hl7:message attribute 'encodingStyle' is not well defined while attempting to process encoded data."));
            }

            // Decode raw data and add message part
            Source source = encoder.decodeFromBytes(data.getBytes());
            Element element = getRootElement(source);
            wrapperBuilder.addPart(partName, element);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                document = createDocumentFromXML(true, new String(data));
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(partName, element);
            }
        }
        return new DOMSource(wrapperBuilder.getResult());
    }

    private NormalizedMessage buildMessagePayload(MessageExchange exchange,
                                                  Message wsdlMessage,
                                                  Map partMappings,
                                                  WrapperBuilder wrapperBuilder,
                                                  HL7Message hl7Message,
                                                  String data) throws SAXException, IOException, Exception {
        NormalizedMessage normalizedMessage = exchange.createMessage();

        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();
        String use = hl7Message.getUseType();
        String partName = hl7Message.getPart();

        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
            if (aPart == null) {
                throw new Exception(I18n.msg("E0169: Failed to locate message part for part name {0}.", partName));
            }
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        // It is safe to assume all WSDL validation are done by now.
        if (use.equals(HL7Message.ATTR_USE_TYPE_ENCODED)) {
            Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
            if (encoder == null) {
                throw new Exception(
                        I18n.msg("E0170: hl7:message attribute 'encodingStyle' is not well defined while attempting to process encoded data."));
            }
            // Decode raw data and add message part
            Source source = encoder.decodeFromString(data);
            Element element = getRootElement(source);
            wrapperBuilder.addPart(partName, element);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                document = createDocumentFromXML(true, new String(data));
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(partName, element);
            }
            /*
             * else { // must be "type" otherwise there would've been a WSDL validation error // We
             * may still be dealing with XML node QName typename = aPart.getTypeName(); if
             * (!WSDLUtilities.isBuiltInType(typename)) { document =
             * XmlUtil.createDocumentFromXML(true, new String(data)); Element element =
             * document.getDocumentElement(); wrapperBuilder.addPart(partName, element); } else { //
             * treat it as Text node document =
             * DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument(); // Note that
             * we need to treat text based and binary data differently // For now, we support text
             * based data only Text textNode = document.createTextNode(new String(data));
             * wrapperBuilder.addPart(partName, new NodeListImpl(textNode)); } }
             */
        }
        normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));

        return normalizedMessage;
    }

    private Element getRootElement(Source source) throws Exception {
        Element root = null;
        if (source instanceof DOMSource) {
            Node sourceNode = ((DOMSource) source).getNode();
            if (sourceNode instanceof Element) {
                root = (Element) sourceNode;
            } else if (sourceNode instanceof Document) {
                root = ((Document) sourceNode).getDocumentElement();
            }
        } else if (source instanceof SAXSource) {
            // convert Source to DOMResult
            try {
                DOMResult result = transformToDOMResult(mTrans, source);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (Exception e) {
                throw new Exception(I18n.msg("E0174: Error occured while transforming the normalized message, e=[{0}]",
                        e.getLocalizedMessage()));
            }
        } else {
            throw new Exception(
                    I18n.msg("E0175: Encountered an invalid Source object - only DOMSource is supported in HL7 BC for message transformation."));
        }

        return root;
    }
}
