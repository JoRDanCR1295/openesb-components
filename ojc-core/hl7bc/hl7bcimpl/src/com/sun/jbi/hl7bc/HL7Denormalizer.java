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
 * @(#)HL7Denormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.TransformerException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.encoder.Encoder;
import com.sun.jbi.hl7bc.Endpoint.EndpointMessageType;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7Message;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import static com.sun.jbi.hl7bc.util.XmlUtil.*;
import com.sun.jbi.hl7bc.I18n;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This is the class that denormalizes a Normalized Message for HL7 BC.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class HL7Denormalizer {

    private static final Logger mLog = Logger.getLogger(HL7Denormalizer.class.getName());

    private WrapperParser wrapperParser;

    /**
     * Given a NormalizedMessage and HL7Message extensibility element information, this method
     * converts the NormalizedMessage to the HL7 message/payload using the Encoder configured for
     * the HL7Message part
     * 
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param operationName The HL7 Operation Name
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param hl7Message The HL7Message extensibility element
     * @throws Exception upon error.
     */

    public String denormalize(NormalizedMessage normalizedMessage,
                              QName operationName,
                              Endpoint endpoint,
                              HL7Message hl7Message) throws Exception {
        DOMResult result = new DOMResult();
        Source src = normalizedMessage.getContent();
        if (src != null) {
            try {
                TransformerFactory fact = TransformerFactory.newInstance();
                Transformer transformer = fact.newTransformer();
                transformer.transform(src, result);
            } catch (TransformerFactoryConfigurationError ex) {
                throw new Exception(I18n.msg("E0327: Exception when creating transformer in HL7Denormalizer, e=[{0}]",
                        ex.getLocalizedMessage()), ex);
            } catch (TransformerException transExe) {
                throw new Exception(
                        I18n.msg(
                                "E0328: Exception occured in HL7Denormalizer when transforming the XML Source to a Result , e=[{0}]",
                                transExe.getLocalizedMessage()), transExe);
            }
        }
        Node node = result.getNode();
        if (node == null) {
            throw new Exception(I18n.msg("E0165: Invalid content in the received normalized message: null."));
        }
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }
        WrapperParser wrapperParser = null;
        try {
            wrapperParser = HelperFactory.createParser();
        } catch (WrapperProcessingException ex) {
            throw new Exception(I18n.msg("E0329: Exception when creating wrapper parser in HL7Denormalizer, e=[{0}]",
                    ex.getLocalizedMessage()), ex);
        }

        wrapperParser.parse(normalizedDoc, endpoint.getDefinition());

        return getMessagePartPayload(normalizedMessage, endpoint, operationName, hl7Message, wrapperParser);
    }

    private String getMessagePartPayload(NormalizedMessage normalizedMessage,
                                         Endpoint endpoint,
                                         QName operationName,
                                         HL7Message hl7Message,
                                         WrapperParser wrapperParser) throws Exception {
        String result = null;

        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();

        // locate the WSDL message
        Message wsdlMessage = null;
        Map operations = endpoint.getHL7Operations();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        HL7Operation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = hl7Message.getUseType();
        String partName = hl7Message.getPart();
        QName hl7qOperationName = new QName(portType.getQName().getNamespaceURI(), operationName.getLocalPart());
        if (operations.get(hl7qOperationName) == null) {
            throw new Exception(I18n.msg("E0166: Encountered an invalid binding operation name '{0}' for HL7 BC.",
                    operationName));
        }
        mep = (String) operationMeps.get(hl7qOperationName);
        if (mep == null || mep.equals(EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(I18n.msg(
                    "E0167: Encountered invalid message exchange pattern for HL7 BC operation '{0}'.", operationName));
        }

        if (mep.equals(EndpointMessageType.IN_OUT)) {
            inout = true;
        }

        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                if (inout) {
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
        // get the message part
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

        NodeList parts = wrapperParser.getPartNodes(partName);
        Node aNode = (Node) parts.item(0);

        if (use.equals(HL7Message.ATTR_USE_TYPE_ENCODED)) {

            String partMappingKey = wsdlMessage.getQName() + partName;
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer("Part mapping key is " + partMappingKey);
                mLog.finer("Available part mapping keys are: " + partMappings.keySet());
            }

            // Locate the encoder
            Encoder encoder = (Encoder) partMappings.get(partMappingKey);
            if (encoder == null) {
                throw new Exception(
                        I18n.msg("E0170: hl7:message attribute 'encodingStyle' is not well defined while attempting to process encoded data."));
            }
            // Encode DOM source to raw data format
            Source source = new DOMSource(aNode);
            result = new String(encoder.encodeToBytes(source));
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                result = transformToString(aNode, "UTF-8", true, "no", "xml");
            } /*
             * else { // must be "type" otherwise there would've been a WSDL validation error //
             * We may still be dealing with XML node QName typename = aPart.getTypeName(); if
             * (!WSDLUtilities.isBuiltInType(typename)) { result =
             * XmlUtil.transformToBytes(aNode, "UTF-8", true, "xml"); } else { // treat it as
             * Text node result = XmlUtil.transformToBytes(aNode, "UTF-8", true, "text"); } }
             */
        }

        return result;
    }
}
