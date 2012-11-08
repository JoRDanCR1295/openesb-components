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
 * @(#)MQExtSerializer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.extservices.QueueAccessOptions;
import org.w3c.dom.Element;



/**
 */
public class MQExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    private final Logger mLogger = Logger.getLogger(getClass().getName());
    
    private static final long serialVersionUID = 1L;
    
    // Pattern for finding application variable tokens
    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private static final Pattern mPattern = Pattern.compile(ENV_VAR_REGEX);
    
    // environment variable configurations
    protected final Map<String, String[]> mEnvVariableMap =
            new HashMap<String, String[]>();
    
    MQExtSerializer() {
    }
    
    public MQExtSerializer(Map<String, String[]> envVariableMap) {
        this();
        mEnvVariableMap.putAll(envVariableMap);
    }
    
    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, MQConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, MQConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, MQConstants.QNAME_BINDING, MQBCBinding.class);
        
        registry.registerSerializer(BindingOperation.class, MQConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, MQConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, MQConstants.QNAME_OPERATION, MQBCOperation.class);
        
        registry.registerSerializer(BindingOperation.class, MQConstants.QNAME_REDELIVERY, this);
        registry.registerDeserializer(BindingOperation.class, MQConstants.QNAME_REDELIVERY, this);
        registry.mapExtensionTypes(BindingOperation.class, MQConstants.QNAME_REDELIVERY, MQBCRedelivery.class);
        
        registry.registerSerializer(Port.class, MQConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, MQConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, MQConstants.QNAME_ADDRESS, MQBCAddress.class);
        
        registry.registerSerializer(BindingInput.class, MQConstants.QNAME_MQMESSAGE, this);
        registry.registerDeserializer(BindingInput.class, MQConstants.QNAME_MQMESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, MQConstants.QNAME_MQMESSAGE, MQBCBody.class);
        
        registry.registerSerializer(BindingOutput.class, MQConstants.QNAME_MQMESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, MQConstants.QNAME_MQMESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, MQConstants.QNAME_MQMESSAGE, MQBCBody.class);
        
        registry.registerSerializer(BindingInput.class, MQConstants.QNAME_MQHEADER, this);
        registry.registerDeserializer(BindingInput.class, MQConstants.QNAME_MQHEADER, this);
        registry.mapExtensionTypes(BindingInput.class, MQConstants.QNAME_MQHEADER, MQBCHeader.class);
        
        registry.registerSerializer(BindingOutput.class, MQConstants.QNAME_MQHEADER, this);
        registry.registerDeserializer(BindingOutput.class, MQConstants.QNAME_MQHEADER, this);
        registry.mapExtensionTypes(BindingOutput.class, MQConstants.QNAME_MQHEADER, MQBCHeader.class);
        
        registry.registerSerializer(BindingOutput.class, MQConstants.QNAME_MQFAULT, this);
        registry.registerDeserializer(BindingOutput.class, MQConstants.QNAME_MQFAULT, this);
        registry.mapExtensionTypes(BindingOutput.class, MQConstants.QNAME_MQFAULT, MQFault.class);
    }
    
    public Map<String, String[]> getEnvVariableMap() {
        return Collections.unmodifiableMap(mEnvVariableMap);
    }
    
    public void marshall(Class parentType,
            QName elementType,
            ExtensibilityElement extension,
            PrintWriter pw,
            javax.wsdl.Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        boolean logFiner = mLogger.isLoggable(Level.FINER);
        if (logFiner) {
            mLogger.log(Level.FINER, I18n.msg("marshall"));
        }
        
        if (extension == null) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg(
                        "marshall: null extensibility element - abort"));
            }
            return;
        }
        
        final String namespace;
        final String prefix;
        String tempPrefix = def.getPrefix(MQConstants.NS_URI_MQ);
        if (tempPrefix == null) {
            // Definition does not account for our namespace.
            // Assign ourself an unused prefix, and add our
            // namespace in our declarations.
            prefix = findUnusedPrefix(def);
            namespace = MQConstants.NS_URI_MQ;
        } else {
            // Definition already defines our namespace; use the assigned prefix.
            prefix = tempPrefix;
            namespace = null; // don't remove; null is meaningful to Deflatables
        }
        
        // false = foreign element, or nested element whose serialization will
        // be taken care of by its parent.
        boolean marshalDirectly = true;
        
        if (extension instanceof MQBCBinding) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = binding"));
            }
        } else if (extension instanceof MQBCOperation) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = operation"));
            }
        } else if (extension instanceof MQBCRedelivery) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = redelivery"));
            }
        } else if (extension instanceof MQBCBody) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = body"));
            }
            
        } else if (extension instanceof MQBCHeader) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = header"));
            }
            
        } else if (extension instanceof MQBCAddress) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = address"));
            }
        } else if (extension instanceof MQFault) {
            if (logFiner) {
                mLogger.log(Level.FINER, I18n.msg("extension = fault"));
            }
        } else {
            marshalDirectly = false;
        }
        
        if (marshalDirectly) {
            if (extension instanceof Deflatable) {
                Deflatable deflatable = (Deflatable) extension;
                ContextImpl context = new ContextImpl();
                context.addContext("namespace", namespace);
                context.addContext("prefix", prefix);
                context.addContext(javax.wsdl.Definition.class, def);
                try {
                    deflatable.deflate(pw, context);
                } catch (Deflatable.DeflateException e) {
                    Throwable t = e.getCause();
                    if (t instanceof WSDLException) {
                        throw (WSDLException) t;
                    } else {
                        throw new WSDLException(WSDLException.OTHER_ERROR,
                                (t == null ? e.getLocalizedMessage() : t.getLocalizedMessage()),
                                (t == null ? e : t));
                    }
                }
            } else {
                throw new WSDLException(WSDLException.OTHER_ERROR,
                        I18n.msg("0905: Cannot marshal own extension ''{0}'':" 
                                + " not deflatable!",
                                extension.getClass().getName()));
            }
        }
    }
    
    
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.log(Level.FINER, I18n.msg("unmarshall"));
        }
        
        ExtensibilityElement returnValue = null;
        
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.log(Level.FINER,
                    I18n.msg("element = {0}", elementType.toString()));
        }
        
        if (MQConstants.QNAME_BINDING.equals(elementType)) {
            returnValue = new MQBCBinding();
        } else if (MQConstants.QNAME_REDELIVERY.equals(elementType)) {
            MQBCRedelivery mqRedelivery = new MQBCRedelivery();
            unmarshal(mqRedelivery, el);
            returnValue = mqRedelivery;
        } else if (MQConstants.QNAME_OPERATION.equals(elementType)) {
            MQBCOperation mqOperation = new MQBCOperation();
            unmarshal(mqOperation, el);
            returnValue = mqOperation;
        } else if (MQConstants.QNAME_MQMESSAGE.equals(elementType)) {
            MQBCBody mqMessage = new MQBCBody();
            unmarshal(mqMessage, el);
            returnValue = mqMessage;
        } else if (MQConstants.QNAME_MQHEADER.equals(elementType)) {
            returnValue = unmarshalMqHeader(el);
        } else if (MQConstants.QNAME_MQFAULT.equals(elementType)) {
            MQFault fault = new MQFault();
            unmarshal(fault, el);
            returnValue = fault;
        } else if (MQConstants.QNAME_ADDRESS.equals(elementType)) {
            MQBCAddress mqAddress = new MQBCAddress();
            unmarshal(mqAddress, el);
            returnValue = mqAddress;
        }
        
        return returnValue;
    }

    private void unmarshal(MQBCAddress mqAddress, Element el)
            throws WSDLException {
        
        String hostName=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_HOST_NAME);
        if (nonEmptyString(hostName)) {
            mqAddress.setHostName(hostName);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_HOST_NAME, hostName));
            }
        }
            
        String portNumber=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_PORT_NUMBER);
        if (nonEmptyString(portNumber)) {
            try {
                mqAddress.setPortNumber(Integer.parseInt(portNumber));
            } catch (NumberFormatException e) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                        e.getMessage(), e);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_PORT_NUMBER, portNumber));
            }
        }
            
        String qMgrName=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_QUEUE_MANAGER_NAME);
        if (nonEmptyString(qMgrName)) {
            mqAddress.setQueueManagerName(qMgrName);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_QUEUE_MANAGER_NAME, qMgrName));
            }
        }
            
        String channelName=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_CHANNEL_NAME);
        if (nonEmptyString(channelName)) {
            mqAddress.setChannelName(channelName);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_CHANNEL_NAME, channelName));
            }
        }
            
        String codedCharacterSetID=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_CODED_CHARACTER_SET_ID);
        if (nonEmptyString(codedCharacterSetID)) {
            mqAddress.setCodedCharacterSetID(codedCharacterSetID);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_CODED_CHARACTER_SET_ID,
                        codedCharacterSetID));
            }
        }
            
        String userID=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_USER_ID);
        if (nonEmptyString(userID)) {
            mqAddress.setUserName(userID);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_USER_ID, userID));
            }
        }
            
        String passWd=getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_PASSWORD);
        if (nonEmptyString(passWd)) {
            mqAddress.setPassword(passWd);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_PASSWORD, passWd));
            }
        }
            
        String cipherSuite = getAttrAndResolveEnvVar(el, MQBCAddress.ATTR_CIPHERSUITE);
        if (nonEmptyString(cipherSuite)) {
            mqAddress.setCipherSuite(cipherSuite);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_CIPHERSUITE,
                        cipherSuite));
            }
        }

        String sslPeerName = getAttrAndResolveEnvVar(el,
                MQBCAddress.ATTR_SSLPEERNAME);
        if (nonEmptyString(sslPeerName)) {
            mqAddress.setSslPeerName(sslPeerName);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCAddress.ATTR_SSLPEERNAME,
                        sslPeerName));
            }
        }
    }

    private void unmarshal(MQBCBody mqMessage, Element el)
            throws WSDLException {
        
        String mqSyncPoint=getAttrAndResolveEnvVar(el, MQBCBody.ATTR_SYNC_POINT);
        if (nonEmptyString(mqSyncPoint)) {
            mqMessage.setMQSyncPoint(Boolean.valueOf(mqSyncPoint));
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCBody.ATTR_SYNC_POINT, mqSyncPoint));
            }
        }
        String mqMessageType=getAttrAndResolveEnvVar(el, MQBCBody.ATTR_MQ_MESSAGE_TYPE);
        if (nonEmptyString(mqMessageType)) {
            mqMessage.setMQMessageType(mqMessageType);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCBody.ATTR_MQ_MESSAGE_TYPE, mqMessageType));
            }
        }
        String mqMessageUse=getAttrAndResolveEnvVar(el, MQBCBody.ATTR_MQ_MESSAGE_USE);
        if (nonEmptyString(mqMessageUse)) {
            mqMessage.setMQMessageUse(mqMessageUse);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCBody.ATTR_MQ_MESSAGE_USE, mqMessageUse));
            }
        }
        String mqMessageEncodingStyle=getAttrAndResolveEnvVar(el, MQBCBody.ATTR_MQ_ENCODING_STYLE);
        if (nonEmptyString(mqMessageEncodingStyle)) {
            mqMessage.setMQMsgBodyEncodingStyle(mqMessageEncodingStyle);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCBody.ATTR_MQ_ENCODING_STYLE, mqMessageEncodingStyle));
            }
        }
        // If it's a TextMessage type, the mqMessageBody attribute
        // references the message parts for the text payload
        // Parse for the mqMessage
        String mqMessageBody = getAttrAndResolveEnvVar(el, MQBCBody.ATTR_MQ_MESSAGE_BODY);
        if (nonEmptyString(mqMessageBody)) {
            mqMessage.setMQMessageBody(mqMessageBody);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCBody.ATTR_MQ_MESSAGE_BODY, mqMessageBody));
            }
        }
    }

    private void unmarshal(MQBCOperation mqOperation, Element el)
            throws WSDLException {
        
        String queueName = getAttrAndResolveEnvVar(el, MQBCOperation.ATTR_QUEUE_NAME);
        if (nonEmptyString(queueName)) {
            mqOperation.setQueueName(queueName);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCOperation.ATTR_QUEUE_NAME,
                        queueName));
            }
        }

        String queueOpenOptions = getAttrAndResolveEnvVar(el, MQBCOperation.ATTR_QUEUE_OPEN_OPTIONS);
        if (nonEmptyString(queueOpenOptions)) {
            QueueAccessOptions options = new QueueAccessOptions();
            options.parseOptions(queueOpenOptions);
            mqOperation.setQueueOpenOptions(options.getOptions());
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCOperation.ATTR_QUEUE_OPEN_OPTIONS,
                        queueOpenOptions));
            }
        }

        String pollingInterval = getAttrAndResolveEnvVar(el, MQBCOperation.ATTR_POLLINGINTERVAL);
        if (nonEmptyString(pollingInterval)) {
            try {
                mqOperation.setPollingInterval(Long.parseLong(pollingInterval));
            } catch (NumberFormatException e) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                        e.getMessage(),
                        e);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCOperation.ATTR_POLLINGINTERVAL,
                        pollingInterval));
            }
        }

        String transaction = getAttrAndResolveEnvVar(el, MQBCOperation.ATTR_TRANSACTION);
        mqOperation.setTransaction(false);
        if (nonEmptyString(transaction)) {
            if (transaction.compareToIgnoreCase("XATransaction") == 0) {
                mqOperation.setTransaction(true);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCOperation.ATTR_TRANSACTION,
                        transaction));
            }
        }
    }

    private void unmarshal(MQBCRedelivery mqRedelivery, Element el)
            throws WSDLException {
        
        String count = getAttrAndResolveEnvVar(el, MQBCRedelivery.ATTR_COUNT);
        if (nonEmptyString(count)) {
            try {
                mqRedelivery.setCount(Integer.parseInt(count));
            } catch (NumberFormatException e) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                        e.getMessage(),
                        e);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCRedelivery.ATTR_COUNT,
                        count));
            }
        }

        String delay = getAttrAndResolveEnvVar(el, MQBCRedelivery.ATTR_DELAY);
        if (nonEmptyString(delay)) {
            try {
                mqRedelivery.setDelay(Integer.parseInt(delay));
            } catch (NumberFormatException e) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                        e.getMessage(),
                        e);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCRedelivery.ATTR_DELAY,
                        delay));
            }
        }

        String target = getAttrAndResolveEnvVar(el, MQBCRedelivery.ATTR_TARGET);
        if (nonEmptyString(target)) {
            mqRedelivery.setTarget(target);
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                        MQBCRedelivery.ATTR_TARGET,
                        target));
            }
        }
    }

    private void unmarshal(MQFault fault, Element el) throws WSDLException {
        // Identify the descriptor
        String reasonCodePartName = DOMUtils.getAttribute(el,
                MQFault.ATTR_REASON_CODE
        );
        if (!nonEmptyString(reasonCodePartName)) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
                    I18n.msg(
                            "0906: Fault specification lacks mandatory reason code part."
                    )
            );
        }
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                    MQFault.ATTR_REASON_CODE,
                    reasonCodePartName));
        }
        
        // Message part
        String reasonTextPartName = DOMUtils.getAttribute(el,
                MQFault.ATTR_REASON_TEXT
        );
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                    MQFault.ATTR_REASON_TEXT, reasonTextPartName
            ));
        }
        
        fault.setReasonCodePart(reasonCodePartName);
        fault.setReasonTextPart(reasonTextPartName);
    }
    
    private MQBCHeader unmarshalMqHeader(Element el) throws WSDLException {
        boolean logFiner = mLogger.isLoggable(Level.FINER);
        if (logFiner) {
            mLogger.log(Level.FINER, I18n.msg("    unmarshalMqHeader"));
        }

        // Identify the descriptor
        String descriptorName = DOMUtils.getAttribute(el, MQBCHeader.ATTR_DESCRIPTOR);
        MessageDescriptors descriptor = null;
        if (nonEmptyString(descriptorName)) {
            // Case-insensitive descriptor search, for
            // robustness' sake, so use manual iteration instead of
            // Enum.valueOf.
            for (MessageDescriptors desc : MessageDescriptors.values()) {
                if (desc.name().equalsIgnoreCase(descriptorName)) {
                    descriptor = desc;
                    break;
                }
            }
        }
        if (descriptor == null) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
                    I18n.msg("0903: Invalid header specification:" 
                            + " descriptor ''{0}'' is not a recognized name.",
                            descriptorName));
        }
        if (logFiner) {
            mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                    MQBCHeader.ATTR_DESCRIPTOR,
                    descriptor.toString()));
        }
        
        // Message part
        String partName = DOMUtils.getAttribute(el, MQBCHeader.ATTR_PART);
        if (!nonEmptyString(partName)) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
                    I18n.msg("0904: Invalid header specification:" 
                            + " descriptor ''{0}'' does not specify an" 
                            + " input message part.",
                            descriptorName,
                            partName));
        }
        if (logFiner) {
            mLogger.log(Level.FINER, I18n.msg("  {0}: {1}",
                    MQBCHeader.ATTR_PART,
                    partName));
        }
        
        return new MQBCHeader(descriptor, partName);
    }
    
    private boolean nonEmptyString(String strToTest) {
        return (strToTest != null && !"".equals(strToTest.trim()));
    }
    
    private String findUnusedPrefix(Definition def) {
        StringBuffer buffer = new StringBuffer("mq");
        int prefixSuffix = 0;
        String prefix;
        while (true) {
            buffer.append(prefixSuffix);
            prefix = buffer.toString();
            if (def.getNamespace(prefix) == null) {
                break;
            }
            buffer.delete(2, buffer.length());
            prefix = null;
            if (prefixSuffix == Integer.MAX_VALUE) {
                break;
            }
            prefixSuffix += 1;
        }
        return prefix;
    }
    
    protected boolean hasMigrationEnvVarRef(String attrVal) throws Exception {
        return mPattern.matcher(attrVal).find();
    }
    
    protected String[] getEnvVariableNames(String attrName, String attrVal) throws Exception {
        if ( attrVal.indexOf("${}") != -1 ) {
            throw new Exception(I18n.msg(
                    "0901: Attribute {0} has an invalid variable reference, $\\{\\}.",
                    attrName));
        } else {
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER,
                        I18n.msg("Attribute {0} value {1}", attrName, attrVal));
            }
        }
        
        String tokenName;
        Matcher m = mPattern.matcher(attrVal);
        List<String> refs = new LinkedList<String>();
        while ( m.find() ) {
            tokenName = m.group(1);
            if (tokenName != null) {
                tokenName = tokenName.trim();
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, I18n.msg("    Found variable: {0}", tokenName));
            }
            refs.add(tokenName);
        }
        
        return refs.toArray(new String[refs.size()]);
    }

    private String getAttrAndResolveEnvVar(Element el, String attrName) throws WSDLException {
        String attrVal = DOMUtils.getAttribute(el, attrName);
        if (attrVal != null) {
            try {
                // We don't support recursive variable substitution,
                // e.g. variable ${foo} has the value "${foo}".
                // It's an undesirable "feature" that could be exploited
                // to create an infinite recursion. It is intentional that
                // the loop that follows will perform only one
                // substitution "pass" through the entire value.
                String[] vars = getEnvVariableNames(attrName, attrVal);
                for (String var : vars) {
                    String[] valAndType = mEnvVariableMap.get(var);
                    String varVal = valAndType[0];
                    if (varVal == null) {
                        throw new WSDLException(
                                WSDLException.CONFIGURATION_ERROR,
                                I18n.msg( "0902: The variable $\\{{0}\\} used in"
                                        + " the service WSDL does not have a defined value.",
                                        var));
                    } else {
                        attrVal = attrVal.replace("${" + var + "}", varVal);
                        if (mLogger.isLoggable(Level.FINER)) {
                            mLogger.log(Level.FINER, I18n.msg(
                                    "        Variable ''{0}'' dereferenced"
                                            + " to {1}", var, varVal));
                        }
                    }
                }
            } catch (WSDLException e) {
                throw e;
            } catch (Exception e) {
                throw new WSDLException(WSDLException.OTHER_ERROR, e.getMessage());
            }
        }
        return attrVal;
    }
}
