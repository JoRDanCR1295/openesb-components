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
 * @(#)MQDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import com.sun.encoder.Encoder;
import com.sun.jbi.mqbc.extensions.MQBCBody;
import com.sun.jbi.mqbc.extensions.MQBCHeader;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extensions.MessageDescriptors;
import com.sun.jbi.mqbc.extensions.XmlSchemaDataTypes;
import com.sun.jbi.mqbc.extservices.ExtServiceMQMessage;
import com.sun.jbi.mqbc.extservices.MQCharacterSets;
import com.sun.jbi.mqbc.extservices.MQClientAgent;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.util.Base64;
import com.sun.jbi.mqbc.util.ConversionException;
import com.sun.jbi.mqbc.util.DateTimeFormatException;
import com.sun.jbi.mqbc.util.HexBinary;
import com.sun.jbi.mqbc.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import net.java.hulp.measure.Probe;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class MQDenormalizer {
    private final EventLogger mLogger
            = new EventLogger(Logger.getLogger(getClass().getName()),
            EventManagementFrameworkAlerter.alerter);
    private final TransformerFactory mTransformFactory;
    private DocumentBuilder mDocumentBuilder;
    
    /** Creates a new instance of MQDenormalizer */
    public MQDenormalizer() {
        mTransformFactory = TransformerFactory.newInstance();
    }
    
    private void initializeDocumentBuilder()
            throws
            ParserConfigurationException {
        
        if (mDocumentBuilder == null) {
            DocumentBuilderFactory docBuilderFactory =
                    DocumentBuilderFactory.newInstance();
            mDocumentBuilder = docBuilderFactory.newDocumentBuilder();
        }
    }

    private Document createDocument(NormalizedMessage msg)
            throws TransformerException {
        DOMResult result = new DOMResult();
        Source content = msg.getContent();
        if (content != null) {
            // Transformer for single-thread use only.
            Transformer transformer = mTransformFactory.newTransformer();
            transformer.transform(content, result);
        }
        Node node = result.getNode();
        return (Document) (node instanceof Document
                ? node
                : ((Element) node).getOwnerDocument());
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
                message = op.getInput().getMessage();
                break;
            }
        }
        return message;
    }

    private Part findMessagePart(Message msgDef, String partName) {
        Part part = null;
        if (!isNothing(partName)) {
            part = msgDef.getPart(partName);
        } else {
            // there is only one part
            Collection parts = msgDef.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[parts.size()]);
            if (partArray.length > 0) {
                part = partArray[0];
            }
        }
        if (part != null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message part: ", part.getName()));
            }
        }
        return part;
    }

    private Object readContent(WrapperParser parser,
                               Part part,
                               Definition definition,
                               ExtServiceMQMessage.MsgHeader header
    )
            throws
            DenormalizationException {

        assert parser != null;
        assert part != null;
        assert definition != null;
        assert header != null;

        // Prepare part information
        NodeList nodesList;
        boolean isElement = part.getElementName() != null;
        QName kind = (isElement ? part.getElementName() : part.getTypeName());
        if (kind == null) {
            throw new DenormalizationException(I18n.msg(
                    "3016: Cannot evaluate the value of part ''{0}''."
                            + " The part has an indeterminate"
                            + " element or type.",
                    part.getName()
            )
            );
        }
        try {
            nodesList = parser.getPartNodes(part.getName());
        } catch (WrapperProcessingException e) {
            throw new DenormalizationException(e.getLocalizedMessage(), e);
        }
        
        // Determine target encoding
        String encoding =
                MQCharacterSets.toJavaCharset(header.getCharacterSet());
        if (encoding == null) {
            StringBuffer charsetEnumStrBuffer = new StringBuffer();
            for (Integer charset : MQCharacterSets.mqCharsets()) {
                charsetEnumStrBuffer.append(charset).append(" ");
            }
            throw new DenormalizationException(I18n.msg(
                    "3018: The message descriptor specifies an unknown"
                            + " character set ID: {0}. Only the following" 
                            + " IDs are known: {1}",
                    header.getCharacterSet(),
                    charsetEnumStrBuffer.toString()
            )
            );
        }
        
        // Extract data
        Object data;
        Map namespaces = definition.getNamespaces();
        assert namespaces != null;
        if (isElement) {
            data = readElement(kind, nodesList, namespaces, encoding);
        } else {
            // Part is a type.
            // There are some types that I recognized as special and require
            // special handling. For example, base64binary and hexBinary data
            // is character data that must be decoded into binary data.
            data = readType(kind, nodesList, namespaces, encoding);
            if (data == null) {
                // No known special handling.
                // Marshal it blob-like instead.
                // This applies to complexTypes and simpleTypes both.
                // Any data that is irreconcilable to the intended structures,
                // I leave the complaining to any subsequent message parsing
                // that cares to.
                data = readType(nodesList, namespaces, encoding);
            }
        }
        return data;
    }

    private String readElement(QName elementName,
                               NodeList nodesList,
                               Map namespaces,
                               String encoding
    )
            throws
            DenormalizationException {

        assert elementName != null;
        assert nodesList != null;
        assert namespaces != null;
        assert encoding != null;

        // Looking for a single element to extract.
        // The element must match the part's.
        // Parts with multiple top-level elements are not a supported
        // construction; extraneous elements are discarded.
        Node node = null;
        for (int i = 0; i < nodesList.getLength(); ++i) {
            node = nodesList.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                QName nodeName = qnameOf(node, namespaces);
                if (elementName.equals(nodeName)) {
                    break;
                }
            }
        }

        String content = null;

        if (node != null) {
            try {
                content = XmlUtil.transformToString(node, encoding, true);
            } catch (Exception e) {
                throw new DenormalizationException(I18n.msg(
                        "3007: An error occured in parsing the message."
                ), e
                );
            }
        }

        return content;
    }

    private String readType(NodeList nodesList,
                            Map namespaces,
                            String encoding
    )
            throws
            DenormalizationException {

        assert nodesList != null;
        assert namespaces != null;
        assert encoding != null;
        
        String content = null;
        
        // Without further exploration of the structure of the content
        // of the part in regard, I take everything of the part
        // and transform it.
        if (nodesList.getLength() > 0) {
            try {
                initializeDocumentBuilder();
                Document document = mDocumentBuilder.newDocument();
                DocumentFragment fragment = document.createDocumentFragment();
                for (int i = 0, len = nodesList.getLength(); i < len; ++i) {
                    Node item = nodesList.item(i);
                    item = document.adoptNode(item); // changes nodesList's length!
                    if (item == null) {
                        item = document.importNode(item, true);
                    }
                    fragment.appendChild(item);
                }
                content = XmlUtil.transformToString(fragment, encoding, true);
            } catch (Exception e) {
                throw new DenormalizationException(I18n.msg(
                        "3007: An error occured in parsing the message."
                ), e
                );
            }
        }
        
        return content;
    }
    
    private Object readType(QName typeName,
                            NodeList nodesList,
                            Map namespaces,
                            String encoding
    )
            throws
            DenormalizationException {

        assert typeName != null;
        assert nodesList != null;
        assert namespaces != null;
        assert encoding != null;
        
        Object content;
        
        switch (XmlSchemaDataTypes.type(typeName)) {
            case BASE64:
                content = Base64.decode(readType(nodesList,
                        namespaces,
                        encoding
                )
                );
                break;
            
            case HEXBINARY:
                content = HexBinary.decodeString(readType(nodesList,
                        namespaces,
                        encoding
                )
                );
                break;
            
            case DATETIME:
                try {
                    // Parse to validate format.
                    String blob = readType(nodesList, namespaces, encoding);
                    // Return value discarded.
                    XmlUtil.convertToCalendar(blob);
                    // no exception; format is good.
                    content = blob;
                } catch (DateTimeFormatException e) {
                    throw new DenormalizationException(e.getLocalizedMessage(),
                            e
                    );
                }
                break;

            default:
                content = null;
        }
        
        return content;
    }
    
    private byte[] readContentAsBytes(WrapperParser parser,
                                      Part part,
                                      Encoder encoder)
            throws DenormalizationException {
        
        assert encoder != null;
        
        try {
            NodeList nl = parser.getPartNodes(part.getName());
            Node aNode = nl.item(0);
            return encoder.encodeToBytes(new DOMSource(aNode));
        } catch (Exception e) {
            throw new DenormalizationException(I18n.msg(
                    "3000: Failed to read contents from normalized message"),
                    e);
        }
    }

    /**
     * Read part data from a normalized message and translate it to a MQ message
     * descriptor value.
     *
     * @param parser Normalized message parser.
     * @param part Normalized message part whose data is in regard.
     * @param namespaces A Map<String, String> of namespaces where prefixes are
     *        keys and namespace URIs are values.  The map is used to identify
     *        the element specified for the part (if it has one, rather than a
     *        type) when the element is not fully qualified at its local scope.
     *        The map is optional: may be null or empty.
     * @param descriptor MQ message descriptor for which the data is intended.
     *
     * @return An object whose type is compatible to the target message
     *         descriptor. The class of the object is such that the object is
     *         equal to or a descendant of the class returned by {@link
     *         com.sun.jbi.mqbc.extensions.MessageDescriptors#getNativeRepresentation()}.
     *         If no data is found in the specified part for the specified
     *         descriptor, null is returned.
     * @throws DenormalizationException if any errors occur in attempting to
     *         parse or translate a value to the descriptor.
     */
    private Object readDescriptorData(WrapperParser parser,
                                      Part part,
                                      Map namespaces,
                                      MessageDescriptors descriptor)
            throws DenormalizationException {
        
        assert parser != null;
        assert part != null;
        assert descriptor != null;
        
        try {
            Object result;
            
            // A part that represents an element is intended to contain
            // character data.  The MQ message descriptor to which it
            // corresponds must be a string-type or they are not reconcilable.
            if (part.getElementName() != null) {
                if (!descriptor.isRepresentableAs(part.getElementName())
                        && !descriptor.isRepresentableAs(XmlSchemaDataTypes.STRING.qname)) {
                    throw new DenormalizationException(I18n.msg(
                            "3014: Cannot interpret ''{0}'' content for descriptor ''{1}''."
                                    + " The content is not representable.",
                            part.getElementName().toString(),
                            descriptor.name()));
                }
                result = parseElement(parser, part, namespaces);
                
            } else if (part.getTypeName() != null) {
                if (!descriptor.isRepresentableAs(part.getTypeName())) {
                    throw new DenormalizationException(I18n.msg(
                            "3014: Cannot interpret ''{0}'' content for descriptor ''{1}''."
                                    + " The content is not representable.",
                            part.getTypeName().toString(),
                            descriptor.name()));
                }
                result = parseType(parser, part, descriptor);
                
            } else {
                throw new DenormalizationException(I18n.msg(
                        "3015: Cannot evaluate part ''{0}'' for descriptor ''{1}''." 
                                + " The part has an indeterminate element or type.",
                        part.getName(),
                        descriptor.name()));
            }
            
            return result;
            
        } catch (Exception e) {
            throw new DenormalizationException(I18n.msg(
                    "3000: Failed to read contents from normalized message"),
                    e);
        }
    }

    private void writeMessage(ExtServiceMQMessage message, byte[] content)
            throws
            DenormalizationException {
        try {
            message.clearMessage();
            message.write(content);
        } catch (IOException e) {
            throw new DenormalizationException(I18n.msg(
                    "3017: Failed to finalize MQ message"
            ), e
            );
        }
    }

    private void writeMessage(ExtServiceMQMessage message, String content)
            throws
            DenormalizationException {
        try {
            // writeString(String) will transcode for me using
            // MQMD.characterSet. Note that writeBytes(String) does not.
            message.clearMessage();
            message.writeString(content);
        } catch (IOException e) {
            throw new DenormalizationException(I18n.msg(
                    "3017: Failed to finalize MQ message"
            ), e
            );
        }
    }

    /**
     * Read a typed value from a normalized message part, and translate it to
     * a MQ message descriptor value.
     * 
     * @param parser Normalized message parser.
     * @param part Normalized message part whose data is in regard.
     * @param descriptor MQ message descriptor for which the data is intended.
     * 
     * @return An object whose type is compatible to the target message
     *         descriptor. The class of the object is such that the object is
     *         equal to or a descendant of the class returned by {@link
     *         com.sun.jbi.mqbc.extensions.MessageDescriptors#getNativeRepresentation()}.
     *         If no data is found in the specified part for the specified
     *         descriptor, null is returned.
     * @throws DenormalizationException if any errors occur in attempting to
     *         parse or translate a value to the descriptor.
     */
    private Object parseType(WrapperParser parser,
                             Part part,
                             MessageDescriptors descriptor)
            throws DenormalizationException {
        
        assert parser != null;
        assert part != null;
        assert descriptor != null;
        
        Object parsed = null;
        
        NodeList nodes;
        try {
            nodes = parser.getPartNodes(part.getName());
        } catch (WrapperProcessingException e) {
            throw new DenormalizationException(e.getLocalizedMessage(), e);
        }
        
        final int nodeCount = nodes.getLength();
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.getLogger().log(Level.FINER, I18n.msg("Parsing {0} nodes.",
                    nodeCount));
        }
        
        for (int i = 0; i < nodeCount; ++i) {
            Node node = nodes.item(i);
            int nodeType = node.getNodeType();
            String value = null;
            
            if (nodeType == Node.ELEMENT_NODE) {
                node.normalize();
                try {
                    value = XmlUtil.transformToString(node, "UTF-8", true, "xml");
                } catch (Exception e) {
                    throw new DenormalizationException(e.getLocalizedMessage(), e);
                }
            } else if (nodeType == Node.CDATA_SECTION_NODE) {
                node.normalize();
                value = node.getNodeValue();
            } else if (nodeType == Node.TEXT_NODE) {
                node.normalize();
                value = node.getNodeValue();
            } else {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.getLogger().log(Level.FINER, I18n.msg(
                            "Skipped unsupported node (#{0}) ''{1}''.",
                            i,
                            node.getNodeName()));
                }
            }
            
            if (value != null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.getLogger().log(Level.FINE, I18n.msg(
                            "Parsing node ''{0}'' for descriptor ''{1}'' (node #{2})",
                            node.getNodeName(),
                            descriptor.name(),
                            i));
                }
                try {
                    parsed = XmlUtil.parseValue(descriptor.getNativeRepresentation(),
                            part.getTypeName(),
                            value);
                } catch (ConversionException e) {
                    throw new DenormalizationException(I18n.msg(
                            "3013: Failed to translate data ''{0}''" 
                                    + " from type ''{1}'' to type ''{2}''",
                            value,
                            part.getTypeName().toString(),
                            descriptor.getNativeRepresentation().getName()), e);
                }
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.getLogger().log(Level.FINEST, I18n.msg(
                            "Node ''{0}'' translated to ''{1}'',  content:\\n{2}",
                            node.getNodeName(),
                            descriptor.getNativeRepresentation().getName(),
                            (parsed instanceof byte[]
                                    ? HexBinary.encodeBytes((byte[]) parsed)
                                    : parsed.toString())));
                }
                break;
            }
        }
        
        if (parsed == null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.getLogger().log(Level.FINE, I18n.msg(
                        "No content found among {0} parsed nodes.",
                        nodeCount));
            }
        }
        
        return parsed;
    }

    /**
     * Read an element value from a normalized message part.
     * 
     * @param parser Normalized message parser.
     * @param part Normalized message part whose data is in regard.
     * @param namespaces A Map<String, String> of namespaces where prefixes are
     *        keys and namespace URIs are values.  The map is used to identify
     *        the element specified for the part (if it has one, rather than a
     *        type) when the element is not fully qualified at its local scope.
     *        The map is optional: may be null or empty.
     * 
     * @return The element as a string, or null if the part does not have
     *         content that is reconcilable with the element specified for the
     *         part.
     * @throws DenormalizationException if any errors occur in attempting to
     *         parse or translate a value to the descriptor.
     */
    private String parseElement(WrapperParser parser, Part part, Map namespaces)
            throws DenormalizationException {
        assert parser != null;
        assert part != null;
        
        NodeList nodes;
        try {
            nodes = parser.getPartNodes(part.getName());
        } catch (WrapperProcessingException e) {
            throw new DenormalizationException(e.getLocalizedMessage(), e);
        }
        
        final int nodeCount = nodes.getLength();
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.getLogger().log(Level.FINER, I18n.msg("Parsing {0} nodes.",
                    nodeCount));
        }
        
        String parsed = null;
        for (int i = 0; i < nodeCount; ++i) {
            Node node = nodes.item(i);
            if (node.getNodeType() != Node.ELEMENT_NODE) {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.getLogger().log(Level.FINER, I18n.msg(
                            "Skipped unexpected non-element node (#{0}) ''{1}''.",
                            i,
                            node.getNodeName()));
                }
            } else {
                QName elementQname = qnameOf(node, namespaces);
                if (part.getElementName().equals(elementQname)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.getLogger().log(Level.FINE, I18n.msg(
                                "Found content match, element ''{0}'' for part ''{1}'' (node #{2})",
                                node.getNodeName(),
                                part.getName(),
                                i));
                    }
                    try {
                        parsed = XmlUtil.transformToString(node, "UTF-8", true, "xml");
                    } catch (Exception e) {
                        throw new DenormalizationException(e.getLocalizedMessage(), e);
                    }
                    if (mLogger.isLoggable(Level.FINEST)) {
                        mLogger.getLogger().log(Level.FINEST, I18n.msg(
                                "Part ''{0}'' matched content:\\n{1}",
                                part.getName(),
                                parsed));
                    }
                    break;
                }
            }
        }
        
        if (parsed == null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.getLogger().log(Level.FINE, I18n.msg(
                        "No matching content found among {0} parsed nodes.",
                        nodeCount));
            }
        }
        
        return parsed;
    }

    /**
     * Decides the fully qualified Name of a node.
     * 
     * <p/>If the node's name is fully qualified (i.e., the name is directly
     * fully qualified, or is prefixed with an identifier whose namespace
     * mapping is declared in the node's scope), that Name is used.
     * 
     * <p/>If the node's name is prefixed, a corresponding namespace
     * URI is resolved from the namespace map.  If no match is found, or a map
     * is not provided, then a fully qualified name for the node is created with
     * its namespace URI set to {@link javax.xml.XMLConstants#NULL_NS_URI}. Its
     * prefix is retained.
     * 
     * <p/>If the node's name is not prefixed, then a fully qualified name for
     * the node is created with its prefix set to
     * {@link javax.xml.XMLConstants#DEFAULT_NS_PREFIX} and its namespace URI
     * set to {@link javax.xml.XMLConstants#NULL_NS_URI}.
     * 
     * @param node The node in regard.
     * @param namespaces A Map<String, String> of namespaces, with prefixes
     *        as keys, and namespace URIs as values. Optional: may be null
     *        or empty.
     * 
     * @return A QName identifying the specified node.
     */
    private QName qnameOf(Node node, Map namespaces) {
        assert node != null;
        assert node.getNodeType() == Node.ELEMENT_NODE;
        
        String prefix = node.getPrefix();
        String namespace = node.getNamespaceURI();
        String localName = node.getLocalName();
        
        // if localName is not null, nodeName is qualified, in which case
        // use nodeName as the QName.
        if (localName != null) {
            return QName.valueOf(node.getNodeName());
        } else {
            localName = node.getNodeName();
        }
        
        // If namespace is not specified in the element scope, resolve against
        // the supplied namespace map.
        if (namespace == null) {
            if (prefix != null && namespaces != null) {
                for (Object prefixObj : namespaces.keySet()) {
                    if (prefix.equals(prefixObj)) {
                        Object namespaceUri = namespaces.get(prefixObj);
                        namespace = namespaceUri.toString();
                        break;
                    }
                }
            }
        }
        if (namespace != null) {
            return (prefix == null
                    ? new QName(namespace, localName)
                    : new QName(namespace, localName, prefix));
        } else {
            return (prefix == null
                    ? new QName(localName)
                    : new QName(XMLConstants.NULL_NS_URI,
                            localName,
                            prefix));
        }
    }

    private boolean isNothing(String str) {
        return str == null || str.trim().equals("");
    }

    private void denormalizeBody(ExtServiceMQMessage mqMessage,
                                 WrapperParser wrapperParser,
                                 Endpoint endpoint,
                                 Message messageDef,
                                 QName operationName,
                                 MQBCBody opInput)
            throws DenormalizationException {

        // Find the part in the specification that represents the message body.
        String messageBodyPartName = opInput.getMQMessageBody();
        Part messageBodyPart = findMessagePart(messageDef, messageBodyPartName);
        if (messageBodyPart == null) {
            if (messageBodyPartName == null) {
                throw new DenormalizationException(I18n.msg(
                        "3001: The message definition {0} for this"
                                + " operation {1} has no parts!"
                                + " Recheck service configuration.",
                        messageDef.getQName().toString(),
                        operationName.toString()));
            } else {
                throw new DenormalizationException(I18n.msg(
                        "3002: Specified message body part ''{0}'' does not exist"
                                + " in the message {1} defined for this operation {2}."
                                + " Recheck service configuration.",
                        messageBodyPartName, messageDef.toString(),
                        operationName.toString()));
            }
        }
        
        // Find the message body part in the normalized data itself.
        // Extract its content.
        final Object content;
        Map messagePartMap = endpoint.getMessagePartEncoderMapping();
        String use = opInput.getMQMessageUse();
        Encoder encoder = null;
        if (MQBCBody.MESSAGE_USE_TYPE_ENCODED.equals(use)) {
            String msgName = messageDef.getQName().toString();
            String partName = messageBodyPart.getName();
            encoder = (Encoder) messagePartMap.get(msgName + partName);
            if (encoder == null) {
                throw new DenormalizationException(I18n.msg(
                        "3003: Required encoder for message {0} part {1} not found."
                                + " Recheck service configuration.",
                        msgName, partName));
            }
            content = readContentAsBytes(wrapperParser, messageBodyPart, encoder);
        } else {
            content = readContent(wrapperParser,
                    messageBodyPart,
                    endpoint.getDefinition(),
                    mqMessage.getMsgHeader()
            );
        }

        // Convert the content into a MQ message
        assert byte[].class.isAssignableFrom(content.getClass())
                || String.class.isAssignableFrom(content.getClass());
        
        if (content instanceof String) {
            if ("TextMessage".equalsIgnoreCase(opInput.getMQMessageType())) {
                writeMessage(mqMessage, (String) content);
            } else {
                throw new DenormalizationException(I18n.msg(
                        "3005: Content not compatible with message input"
                                + " (MQ message type ''{0}'')."
                                + " Content is character data.",
                        opInput.getMQMessageType()
                )
                );
            }
        }
        else if (content instanceof byte[]) {
            
            // bytes produced by a com.sun.encoder.Encoder are written
            // as-is with no further translation.
            // MQ MessageType ("TextMessage" / "ByteMessage") is
            // not applicable in this case.
            if (encoder != null) {
                writeMessage(mqMessage, (byte[]) content);
            }
            
            // bytes produced any other way are subject to one more validation.
            // if the nature of the data + message specification yielded
            // binary data, the service specification must be expecting
            // binary data (MQ messageType = "ByteMessage") or the process
            // fails.
            // If messageType is "TextMessage", can't I "interpret" the
            // binary data by trying to decode it as character data using the
            // specified characterSet? Sure I can, but I won't, because that's
            // full of stupid. That's a logical inconsistency in the
            // service description.
            else {
                if ("ByteMessage".equalsIgnoreCase(opInput.getMQMessageType())) {
                    writeMessage(mqMessage, (byte[]) content);
                } else {
                    throw new DenormalizationException(I18n.msg(
                            "3004: Content not compatible with message input"
                                    + " (MQ message type ''{0}'')."
                                    + " Content is binary.",
                            opInput.getMQMessageType()
                    )
                    );
                }
            }
        } else {
            throw new DenormalizationException(I18n.msg(
                    "3019: Denormalization panic:"
                            + " Unsupported data type produced: {0}!",
                    content.getClass().getName()
            )
            );
        }
    }

    private void denormalizeHeader(ExtServiceMQMessage mqMessage,
                                   WrapperParser wrapperParser,
                                   Message messageDef,
                                   Map namespaces,
                                   QName operationName,
                                   MQBCHeader opInputHeader)
            throws DenormalizationException {

        // Find the part in the specification that corresponds to the header.
        String partName = opInputHeader.getPartName();
        Part part = findMessagePart(messageDef, partName);
        if (part == null) {
            if (partName == null) {
                throw new DenormalizationException(I18n.msg(
                        "3001: The message definition {0} for this"
                                + " operation {1} has no parts!"
                                + " Recheck service configuration.",
                        messageDef.getQName().toString(),
                        operationName.toString()));
            } else {
                throw new DenormalizationException(I18n.msg(
                        "3002: Specified message body part ''{0}'' does not exist"
                                + " in the message {1} defined for this operation {2}."
                                + " Recheck service configuration.",
                        partName,
                        messageDef.toString(),
                        operationName.toString()));
            }
        }

        // Find the part in the normalized data itself.
        MessageDescriptors descriptor = opInputHeader.getDescriptor();
        Object data = readDescriptorData(wrapperParser, part, namespaces, descriptor);
        if (data != null) {
            mqMessage.getMsgHeader().setDescriptorData(descriptor, data);
        }
    }

    public ExtServiceMQMessage denormalize(NormalizedMessage normalizedMessage,
                                           QName operationFullQName,
                                           Endpoint endpoint,
                                           MQClientAgent mqClntAgent)
            throws DenormalizationException {

        Probe processProbe = Probe.fine(getClass(), "Request Denormalization ");

        try {


        // Create document model and parse the normalized message.
        Document normalizedDoc;
        WrapperParser wrapperParser;
        try {
            normalizedDoc = createDocument(normalizedMessage);
            wrapperParser = HelperFactory.createParser();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Parser initialized"));
            }
        } catch (Exception e) {
            throw new DenormalizationException(I18n.msg(
                    "3006: An error occured while preparing to parse the message."),
                    e);
        }
        try {
            wrapperParser.parse(normalizedDoc, endpoint.getDefinition());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message parsed"));
            }
        } catch (WrapperProcessingException e) {
            throw new DenormalizationException(
                    I18n.msg("3007: An error occured in parsing the message."),
                    e);
        }

        MQBCOperation operation =
                (MQBCOperation) endpoint.getMQOperations().get(
                        operationFullQName);
        if (operation == null) {
            throw new DenormalizationException(I18n.msg(
                    "3008: Cannot denormalize message for operation {0}"
                            + " because the operation is unknown to the endpoint {1}."
                            + " Recheck service configuration.",
                    operationFullQName.toString(), endpoint.getEndpointName()));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Endpoint {0} understands operation {1}.",
                    endpoint.getEndpointName(),
                    operationFullQName.toString()));
        }
        
        Message wsdlMessage = findInputMessage(endpoint, operationFullQName);
        if (wsdlMessage == null) {
            throw new DenormalizationException(I18n.msg(
                    "3009: Cannot denormalize message for endpoint {0} operation {1}"
                    + " because the definition for its input message cannot be found."
                    + " Recheck service configuration.",
                    endpoint.getEndpointName(), operationFullQName.toString()));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Input message type: {0}",
                    wsdlMessage.getQName().toString()));
        }
        
        
        MQBCBody opInput = operation.getMQOperationInput().getMQMessage();
        if (opInput == null) {
            throw new DenormalizationException(I18n.msg(
                    "3010: Cannot denormalize message because"
                            + " the operation {0} does not have MQ binding information."
                            + " Recheck service configuration.",
                    operationFullQName.toString()));
        }
        
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Denormalizing a message for endpoint {0}, operation {1} ...",
                    endpoint.getEndpointName(),
                    operationFullQName.toString()));
        }
        
        // Allocate MQ message container
        ExtServiceMQMessage mqMsg;
        try {
            mqMsg = mqClntAgent.newMessage();
        } catch (Exception e) {
            throw new DenormalizationException(
                    I18n.msg("3011: Failed to initialize a new MQ message"), e);
        }

        // Transfer the header content (if any) from the normalized message to
        // an MQ message. This needs to be done before body denormalization
        // because there can be header data that is vital for
        // body denormalization (like character set encoding!)
        MQBCHeader[] headers = operation.getMQOperationInput().getMQHeaders();
        if (headers.length == 0) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("No headers to process."));
            }
        } else {
            for (MQBCHeader header : headers) {
                denormalizeHeader(mqMsg,
                        wrapperParser,
                        wsdlMessage,
                        endpoint.getDefinition().getNamespaces(),
                        operationFullQName,
                        header);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Message headers denormalized."));
                }
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message headers normalized."));
            }
        }
        
        // Transfer the body content from the normalized message to an MQ message.
        denormalizeBody(mqMsg, wrapperParser, endpoint, wsdlMessage,
                operationFullQName, opInput);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Message body denormalized."));
        }
        
        return mqMsg;


        } finally {
            processProbe.end();
        }
    }
}
