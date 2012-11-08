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
 */

/*
 * @(#)MQNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import java.io.UnsupportedEncodingException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import com.sun.encoder.Encoder;
import com.sun.jbi.mqbc.extensions.MQBCBody;
import com.sun.jbi.mqbc.extensions.MQBCHeader;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extensions.MQFault;
import com.sun.jbi.mqbc.extensions.MessageDescriptors;
import com.sun.jbi.mqbc.extensions.XmlSchemaDataTypes;
import com.sun.jbi.mqbc.extservices.MQBCExtServiceException;
import com.sun.jbi.mqbc.extservices.MQCharacterSets;
import com.sun.jbi.mqbc.extservices.MQXDecode;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.util.Base64;
import com.sun.jbi.mqbc.util.HexBinary;
import com.sun.jbi.mqbc.util.WSDLUtilities;
import com.sun.jbi.mqbc.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import net.java.hulp.measure.Probe;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

public class MQNormalizer {

    private final EventLogger mLogger
            = new EventLogger(Logger.getLogger(getClass().getName()),
            EventManagementFrameworkAlerter.alerter);

    public MQNormalizer() {
    }


    private Message findInputMessage(Endpoint endpoint, QName operationName) {
        Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Message message = null;
        for (Object o : portType.getOperations()) {
            Operation op = (Operation) o;
            if (op.getName().equals(operationName.toString())
                    || op.getName().equals(operationName.getLocalPart())) {
                /**
                 * Since we are not handling Solicit-response or 
                 * Notification type of MQ BC operations, it is
                 * safe to assume that the message type for mq recv
                 * is always associated with the Operation Input.
                 */
                message = op.getInput().getMessage();
                break;
            }
        }
        return message;
    }

    public NormalizedMessage normalize(com.sun.jbi.mqbc.extservices.ExtServiceMQMessage mqMsg,
                                       NormalizedMessage messageContainer,
                                       QName operationFullQName,
                                       Endpoint endpoint,
                                       boolean useInputMessage
    )
            throws
            NormalizationException {
        
        Source normalizedSource =
                normalize(mqMsg, operationFullQName, endpoint, useInputMessage);
        try {
            messageContainer.setContent(normalizedSource);
        } catch (MessagingException e) {
            throw new NormalizationException(I18n.msg(
                    "3025: An error occured while normalizing the message."),
                    e);
        }
        return messageContainer;
    }

    public Source normalize(com.sun.jbi.mqbc.extservices.ExtServiceMQMessage mqMessage,
                            QName operationFullQName,
                            Endpoint endpoint,
                            boolean useInputMessage
    )
            throws
            NormalizationException {

        Probe processProbe = Probe.fine(getClass(), "Message Normalization");
        
        MQBCOperation operation = (MQBCOperation)endpoint.getMQOperations().get(operationFullQName);
        if (operation == null) {
            throw new NormalizationException(I18n.msg(
                    "3020: Cannot normalize message for operation {0}"
                            + " because the operation is unknown to the endpoint {1}."
                            + " Recheck service configuration.",
                    operationFullQName.toString(), endpoint.getEndpointName()));
        }

        MQBCBody mqBody = (useInputMessage
                ? operation.getMQOperationInput().getMQMessage()
                : operation.getMQOperationOutput().getMQMessage());
        if (mqBody == null) {
            throw new NormalizationException(I18n.msg(
                    "3021: Cannot normalize message because"
                            + " the operation {0} does not have MQ binding information."
                            + " Recheck service configuration.",
                    operationFullQName.toString()));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Endpoint {0} understands operation {1}.",
                    endpoint.getEndpointName(),
                    operationFullQName.toString()));
        }
            
            
        try {
            
            
        Message wsdlMessage = findInputMessage(endpoint, operationFullQName);
        if (wsdlMessage == null) {
            throw new NormalizationException(I18n.msg(
                    "3022: Cannot normalize message for endpoint {0} operation {1}"
                    + " because the definition for its input message cannot be found."
                    + " Recheck service configuration.",
                    endpoint.getEndpointName(), operationFullQName.toString()));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Input message type: {0}",
                    wsdlMessage.getQName().toString()));
        }
        
        // Preparation
        String partName = mqBody.getMQMessageBody();
        WrapperBuilder wrapperBuilder;
        byte[] bytePayload;
        Encoder encoder = null;
        try {
            // Normalized message builder
            wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, wsdlMessage, null);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message builder initialized"));
            }
            
            // Message content
            bytePayload = new byte[mqMessage.getMessageLength()] ;
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message builder initialized"));
            }
            
            // Message content encoder
            String messageUse = mqBody.getMQMessageUse();
            if (MQBCBody.MESSAGE_USE_TYPE_ENCODED.equals(messageUse)) {
                Map partMappings = endpoint.getMessagePartEncoderMapping();
                String messageName = wsdlMessage.getQName().toString();
                encoder = (Encoder) partMappings.get(messageName + partName);
                if (encoder == null) {
                    throw new NormalizationException(I18n.msg(
                            "3003: Required encoder for message {0} part {1} not found."
                                    + " Recheck service configuration.",
                            messageName, partName));
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Encoder needed and found: {0}",
                                encoder.toString()));
                    }
                }
            }
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3024: An error occured while preparing to normalize the message."),
                    e);
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Normalizing a message for endpoint {0}, operation {1} ...",
                    endpoint.getEndpointName(),
                    operationFullQName.toString()));
        }
        
        try {
            mqMessage.readFully(bytePayload);
            
            // Normalize body
            if (encoder != null) {
                addPartString(partName,
                        bytePayload,
                        wrapperBuilder,
                        wsdlMessage,
                        operationFullQName,
                        encoder);
            } else {
                // message use is 'literal'
                // Discern between Text and Byte content
                if (MQBCBody.MESSAGE_TYPE_TEXT_MESSAGE.equals(mqBody.getMQMessageType())) {
                    String stringPayload = readStringPayload(bytePayload,
                            mqMessage.getMsgHeader().getCharacterSet());
                    addPartString(partName,
                            stringPayload,
                            wrapperBuilder,
                            wsdlMessage,
                            operationFullQName);
                } else {
                    addPartBytes(partName,
                            bytePayload,
                            wrapperBuilder,
                            wsdlMessage,
                            operationFullQName);
                }
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message body normalized."));
            }
            
            // Normalize headers (optional)
            MQBCHeader[] headers = operation.getMQOperationInput().getMQHeaders();
            if (headers.length == 0) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("No headers to process."));
                }
            } else {
                for (MQBCHeader header : headers) {
                    Object descriptorData = mqMessage.getMsgHeader()
                            .getDescriptorData(header.getDescriptor());
                    if (descriptorData != null) {
                        addPart(wrapperBuilder,
                                descriptorData,
                                header.getDescriptor(),
                                header.getPartName(),
                                wsdlMessage,
                                operationFullQName);
                    }
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Message headers normalized."));
                }
            }
            
            Document doc = wrapperBuilder.getResult();
            return new DOMSource(doc);
            
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3025: An error occured while normalizing the message."),
                    e);
        }
            
            
        } finally {
            processProbe.end();
        }
    }
    
    public void normalizeFault(Fault faultMessage,
                               MQBCExtServiceException exception,
                               QName opName,
                               Endpoint endpoint)
            throws
            NormalizationException {

        Probe processProbe = Probe.fine(getClass(), "Message Normalization");
        
        // Extract reason code and text
        int reasonCode;
        String reasonText;
        if (!exception.isMQException()) {
            processProbe.end();
            throw new NormalizationException(I18n.msg(
                    "3041: Cannot normalize MQ fault from a non-MQ exception."
            )
            );
        } else {
            reasonCode = exception.reasonCode;
            reasonText = clean(MQXDecode.decode(reasonCode));
        }

        // Find fault definition for operation
        Message faultSpec = findInputMessage(endpoint, opName);
        if (faultSpec == null) {
            processProbe.end();
            throw new NormalizationException(I18n.msg(
                    "3042: Cannot normalize fault for endpoint {0}"
                            + " operation {1} because the definition for"
                            + " its message cannot be found."
                            + " Recheck service configuration.",
                    endpoint.getEndpointName(),
                    opName.toString()
            )
            );
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Fault message type: {0}",
                    faultSpec.getQName().toString()
            )
            );
        }

        // Map values to fault message parts
        MQBCOperation operation =
                (MQBCOperation) endpoint.getMQOperations().get(opName);
        if (operation == null) {
            processProbe.end();
            throw new NormalizationException(I18n.msg(
                    "3020: Cannot normalize message for operation {0}"
                            + " because the operation is unknown to"
                            + " the endpoint {1}."
                            + " Recheck service configuration.",
                    opName.toString(),
                    endpoint.getEndpointName()
            )
            );
        }
        MQFault mqFault = operation.getMQOperationFault();
        String codePartName = clean(mqFault.getReasonCodePart());
        String textPartName = clean(mqFault.getReasonTextPart());

        // Create normalized fault message
        WrapperBuilder wrapperBuilder;
        try {
            
            // Init builder
            wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, faultSpec, null);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message builder initialized"));
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg(
                        "Normalizing a fault message for endpoint {0}, operation {1} ...",
                        endpoint.getEndpointName(),
                        opName.toString()
                )
                );
            }
            
            // Reason code
            if (!"".equals(codePartName)) {
                addPartString(codePartName,
                        String.valueOf(reasonCode),
                        wrapperBuilder,
                        faultSpec,
                        opName
                );
            }
            
            // Reason text
            if (!"".equals(textPartName)) {
                addPartString(textPartName,
                        reasonText,
                        wrapperBuilder,
                        faultSpec,
                        opName
                );
            }
            
            faultMessage.setContent(new DOMSource(wrapperBuilder.getResult()));

        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3024: An error occured while preparing to normalize the message."
            ), e
            );
        } finally {
            processProbe.end();
        }
    }

    private String readStringPayload(byte[] bytePayload, int characterSet)
            throws UnsupportedEncodingException {
        
        assert bytePayload != null;
        assert characterSet > 0;
        
        String charset = MQCharacterSets.toJavaCharset(characterSet);
        if (charset != null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Reading character payload; length {0},"
                        + " MQ characterSet {1}, Java character set {2}",
                        bytePayload.length,
                        characterSet,
                        charset));
            }
            return new String(bytePayload, charset);
        } else {
            throw new UnsupportedEncodingException(I18n.msg(
                    "3026: MQ characterSet {0} has no known mapping to Java character sets.",
                    characterSet));
        }
    }

    private void addPartString(String partName,
                               byte[] bytePayload,
                               WrapperBuilder wrapperBuilder,
                               Message wsdlMessage,
                               QName operationName,
                               Encoder encoder) throws NormalizationException {
        assert bytePayload != null;
        assert wrapperBuilder != null;
        assert wsdlMessage != null;
        assert encoder != null;
        
        Part aPart;

        if (!isEmpty(partName)) {
            aPart = findPart(wsdlMessage, partName);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3002: Specified message body part ''{0}'' does not exist"
                                + " in the message {1} defined for this operation {2}."
                                + " Recheck service configuration.", partName,
                        wsdlMessage.toString(), operationName.toString()));
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg(
                        "Part name unspecified, normalizing the first message part."));
            }
            aPart = firstPart(wsdlMessage);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3001: The message definition {0} for this"
                                + " operation {1} has no parts!"
                                + " Recheck service configuration.",
                        wsdlMessage.getQName().toString(),
                        operationName.toString()));
            }
        }

        try {
            Source source = encoder.decodeFromBytes(bytePayload);
            Element element = getRootElement(source);
            wrapperBuilder.addPart(partName, element);
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPartString(String partName,
                               String stringPayload,
                               WrapperBuilder wrapperBuilder,
                               Message wsdlMessage,
                               QName operationName)
            throws NormalizationException {
        assert stringPayload != null;
        assert wrapperBuilder != null;
        assert wsdlMessage != null;
        
        Part aPart;

        if (!isEmpty(partName)) {
            aPart = findPart(wsdlMessage, partName);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3002: Specified message body part ''{0}'' does not exist"
                                + " in the message {1} defined for this operation {2}."
                                + " Recheck service configuration.", partName,
                        wsdlMessage.toString(), operationName.toString()));
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg(
                        "Part name unspecified, normalizing the first message part."));
            }
            aPart = firstPart(wsdlMessage);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3001: The message definition {0} for this"
                                + " operation {1} has no parts!"
                                + " Recheck service configuration.",
                        wsdlMessage.getQName().toString(),
                        operationName.toString()));
            }
        }

        try {
            addPart(wrapperBuilder, aPart, stringPayload);
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPart(WrapperBuilder wrapperBuilder,
                         Object data,
                         MessageDescriptors descriptor,
                         String partName,
                         Message wsdlMessage,
                         QName operationName) throws NormalizationException {
        
        assert wrapperBuilder != null;
        assert data != null;
        assert descriptor != null;
        assert partName != null;
        assert wsdlMessage != null;
        assert operationName != null;
        
        Part part = null;

        if (!isEmpty(partName)) {
            part = findPart(wsdlMessage, partName);
        }
        
        if (part == null) {
            throw new NormalizationException(I18n.msg(
                    "3002: Specified message body part ''{0}'' does not exist"
                            + " in the message {1} defined for this operation {2}."
                            + " Recheck service configuration.", partName,
                    wsdlMessage.toString(), operationName.toString()));
        }

        // Leniency is applied to data that that is supposed to map to
        // xsd:string: if its corresponding part is an element rather than
        // a type, we'll attempt to parse the data against the element instead
        // of outright rejecting it.
        if (part.getElementName() != null) {
            if (!(data instanceof String)) {
                StringBuffer choices = new StringBuffer();
                for (QName type : descriptor.getRepresentations()) {
                    choices.append(type.toString());
                    choices.append("\n");
                }
                throw new NormalizationException(I18n.msg(
                        "3039: Descriptor ''{0}'' cannot be mapped to part {1}"
                                + " because the part is an element ({2})."
                                + " The descriptor may only be mapped to" 
                                + " any of these types:\n{3}",
                        descriptor.name(),
                        part.getName(),
                        part.getElementName().toString(),
                        choices.toString()));
            }
            // Here is the fall-thru for string data, to attempt parse against element
            
        } else if (!descriptor.isRepresentableAs(part.getTypeName())) {
            StringBuffer choices = new StringBuffer();
            for (QName type : descriptor.getRepresentations()) {
                choices.append(type.toString());
                choices.append("\n");
            }
            throw new NormalizationException(I18n.msg(
                    "3040: Descriptor ''{0}'' cannot be mapped to part {1}"
                            + " because the part is a type ({2})."
                            + " The descriptor may only be mapped to" 
                            + " any of these types:\n{3}",
                    descriptor.name(),
                    part.getName(),
                    part.getTypeName().toString(),
                    choices.toString()));
        }

        try {
            if (data instanceof byte[]) {
                addPart(wrapperBuilder, part, (byte[]) data);
            } else if (data instanceof Integer) {
                addPart(wrapperBuilder, part, (Integer) data);
            } else if (data instanceof String) {
                addPart(wrapperBuilder, part, (String) data);
            } else if (data instanceof Calendar) {
                addPart(wrapperBuilder, part, (Calendar) data);
            } else {
                addPart(wrapperBuilder, part, data.toString());
            }
        } catch (NormalizationException e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPartBytes(String partName,
                              byte[] bytePayload,
                              WrapperBuilder wrapperBuilder,
                              Message wsdlMessage,
                              QName operationName)
            throws NormalizationException {

        assert bytePayload != null;
        assert wrapperBuilder != null;
        assert wsdlMessage != null;
        assert operationName != null;
        
        Part aPart;

        if (!isEmpty(partName)) {
            aPart = findPart(wsdlMessage, partName);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3002: Specified message body part ''{0}'' does not exist"
                                + " in the message {1} defined for this operation {2}."
                                + " Recheck service configuration.", partName,
                        wsdlMessage.toString(), operationName.toString()));
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg(
                        "Part name unspecified, normalizing the first message part."));
            }
            aPart = firstPart(wsdlMessage);
            if (aPart == null) {
                throw new NormalizationException(I18n.msg(
                        "3001: The message definition {0} for this"
                                + " operation {1} has no parts!"
                                + " Recheck service configuration.",
                        wsdlMessage.getQName().toString(),
                        operationName.toString()));
            }
        }

        try {
            addPart(wrapperBuilder, aPart, bytePayload);
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPart(WrapperBuilder wrapperBuilder,
                         Part part,
                         String stringPayload) throws NormalizationException {
        try {
            if (part.getElementName() != null) {
                Document document = XmlUtil.createDocumentFromXML(true, stringPayload);
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(part.getName(), element);
            } else {
                QName typename = part.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename)) {
                    Document document = XmlUtil.createDocumentFromXML(true, stringPayload);
                    Element element = document.getDocumentElement();
                    wrapperBuilder.addPart(part.getName(), element);
                } else {
                    // treat it as Text node
                    Document document =
                            DocumentBuilderFactory.newInstance()
                                    .newDocumentBuilder().newDocument();
                    Text textNode = document.createTextNode(stringPayload);
                    wrapperBuilder.addPart(part.getName(),
                            new NodeListImpl(textNode));
                }
            }
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPart(WrapperBuilder wrapperBuilder,
                         Part part,
                         Calendar dateTime) throws NormalizationException {
        
        try {
            String dateTimeString = XmlUtil.convertToDateTime(dateTime);
            Document document = DocumentBuilderFactory.newInstance()
                    .newDocumentBuilder().newDocument();
            Text textNode = document.createTextNode(dateTimeString);
            wrapperBuilder.addPart(part.getName(), new NodeListImpl(textNode));
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPart(WrapperBuilder wrapperBuilder,
                         Part part,
                         Integer integer) throws NormalizationException {

        try {
            String stringInteger = integer.toString();
            Document document = DocumentBuilderFactory.newInstance()
                    .newDocumentBuilder().newDocument();
            Text textNode = document.createTextNode(stringInteger);
            wrapperBuilder.addPart(part.getName(), new NodeListImpl(textNode));
        } catch (Exception e) {
            throw new NormalizationException(I18n.msg(
                    "3029: Failed to add content as a normalized message part"),
                    e);
        }
    }

    private void addPart(WrapperBuilder wrapperBuilder,
                         Part part,
                         byte[] bytePayload) throws NormalizationException {
        if (part.getElementName() != null) {
            throw new NormalizationException(I18n.msg(
                    "3037: Binary data cannot be mapped to part {0}"
                            + " because it is an element ({1})."
                            + " Binary data may only be mapped to {2}.",
                    part.getName(),
                    part.getElementName().toString(),
                    XmlSchemaDataTypes.BASE64.qname.toString()
                            + " or "
                            + XmlSchemaDataTypes.HEXBINARY.qname.toString()));
        } else {
            QName typeName = part.getTypeName();
            String encodedPayload;
            if (XmlSchemaDataTypes.BASE64.qname.equals(typeName)) {
                encodedPayload = Base64.encodeBytes(bytePayload, Base64.DONT_BREAK_LINES);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg(
                            "Encoded binary content as base64Binary"));
                }
            } else if (XmlSchemaDataTypes.HEXBINARY.qname.equals(typeName)) {
                encodedPayload = HexBinary.encodeBytes(bytePayload);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Encoded binary content as hexBinary"));
                }
            } else {
                throw new NormalizationException(I18n.msg(
                        "3038: Binary data cannot be mapped to part {0}"
                                + " because it is a type ({1})."
                                + " Binary data may only be mapped to {2}.",
                        part.getName(),
                        typeName.toString(),
                        XmlSchemaDataTypes.BASE64.qname.toString()
                                + " or "
                                + XmlSchemaDataTypes.HEXBINARY.qname.toString()));
            }
            try {
                Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                Text textNode = document.createTextNode(encodedPayload);
                wrapperBuilder.addPart(part.getName(), new NodeListImpl(textNode));
            } catch (Exception e) {
                throw new NormalizationException(I18n.msg(
                        "3029: Failed to add content as a normalized message part"),
                        e);
            }
        }
    }

    private Part firstPart(Message wsdlMessage) {
        assert wsdlMessage != null;
        Collection parts = wsdlMessage.getParts().values();
        Part aPart = null;
        if (parts.size() > 0) {
            aPart = (Part) parts.iterator().next();
        }
        return aPart;
    }

    private Part findPart(Message wsdlMessage, String partName) {
        assert partName != null;
        assert wsdlMessage != null;
        return wsdlMessage.getPart(partName);
    }

    private boolean isEmpty(String value) {
        return value == null || "".equals(value.trim());
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
        } else {
            // convert Source to DOMResult
            DOMResult result = XmlUtil.transformToDOMResult(source);
            root = ((Document) result.getNode()).getDocumentElement();
        }
        
        return root;
    }
    
    private String clean(String dirty) {
        if (dirty == null) {
            return "";
        } else {
            return dirty.trim();
        }
    }
}
