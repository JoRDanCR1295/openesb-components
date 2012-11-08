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
 * @(#)MSMQExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.extensions;

import java.io.Serializable;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.Map;
import java.util.StringTokenizer;

import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.Port;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import com.sun.jbi.internationalization.Messages;

/**
 * @author Sun Microsystems
 */
public class MSMQExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;

    public static final String ATTR_CONNECTION_MODE = "connectionMode";

    public static final String ATTR_DESTINATION = "destination";

    public static final String ATTR_SHARE_MODE = "shareMode";

    public static final String ATTR_ACCESS_MODE = "accessMode";

    public static final String ATTR_RETRY_INTERVAL = "retryInterval";

    public static final String ATTR_MESSAGE_PRIORITY = "messagePriority";

    public static final String ATTR_ACKNOWLEDEMENT = "acknowledgement";

    public static final String ATTR_MSGLOOKUPID = "msgLookupID";

    public static final String ATTR_PART = "part";

    public static final String ATTR_MESSAGE_TYPE = "messageType";

    public static final String ATTR_USE_TYPE = "use";

    public static final String ATTR_ENCODING_STYLE = "encodingStyle";

    public static final String ATTR_TRANSACTION = "transaction";

    public static final String ATTR_MAX_RETRIES = "maxRetries";

    // environment variable configurations
    protected Map mEnvVariableMap;

    private static final Messages mMessages = Messages.getMessages(MSMQExtSerializer.class);

	/**
     * Creates a new instance of MSMQExtSerializer
     */
    public MSMQExtSerializer() {
    }

	 public MSMQExtSerializer(Map envVariableMap) {
		this();
        mEnvVariableMap = envVariableMap;
    }
    /**
     * Contruction of Register Serializer class
     * 
     * @param ExtensionRegistry
     */
    public void registerSerializer(ExtensionRegistry registry) {

        // Register and map MSMQ Binding
        registry.registerSerializer(Binding.class, MSMQConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, MSMQConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, MSMQConstants.QNAME_BINDING, MSMQBinding.class);

        // Register and map MSMQ Operation
        registry.registerSerializer(BindingOperation.class, MSMQConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, MSMQConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, MSMQConstants.QNAME_OPERATION, MSMQOperation.class);

        // Register and map MSMQ Input
        registry.registerSerializer(BindingInput.class, MSMQConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, MSMQConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, MSMQConstants.QNAME_MESSAGE, MSMQInput.class);

        // Register and map MSMQ Output
        registry.registerSerializer(BindingOutput.class, MSMQConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, MSMQConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, MSMQConstants.QNAME_MESSAGE, MSMQOutput.class);

        // Register and map MSMQ Address
        registry.registerSerializer(Port.class, MSMQConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, MSMQConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, MSMQConstants.QNAME_ADDRESS, MSMQAddress.class);
    }

    /**
     * Marshall the wsdl extensability elements
     * 
     * @param Class
     * @param QName
     * @param ExtensibilityElement
     * @param PrintWriter
     * @param Definition
     * @param ExtensionRegistry
     * @throws WSDLException
     */
    public void marshall(Class parentType,
                         QName elementType,
                         ExtensibilityElement extension,
                         PrintWriter pw,
                         javax.wsdl.Definition def,
                         ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof MSMQBinding) {
            MSMQBinding msmqBinding = (MSMQBinding) extension;
            pw.print("      <msmq:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof MSMQOperation) {
            MSMQOperation msmqOperation = (MSMQOperation) extension;
            pw.print("      <msmq:operation");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            pw.println("/>");
        } else if (extension instanceof MSMQAddress) {
            MSMQAddress msmqAddress = (MSMQAddress) extension;
            pw.print("      <msmq:address");

            if (msmqAddress.getHostName() != null) {
                DOMUtils.printAttribute(MSMQAddress.ATTR_ADDRESS, msmqAddress.getHostName(), pw);
            }

            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof MSMQMessage) {
            MSMQMessage msmqMessage = (MSMQMessage) extension;
            pw.print("      <msmq:message");

            if (msmqMessage.getConnectionMode() != null) {
                DOMUtils.printAttribute(ATTR_CONNECTION_MODE, msmqMessage.getConnectionMode(), pw);
            }
            if (msmqMessage.getDestination() != null) {
                DOMUtils.printAttribute(ATTR_DESTINATION, msmqMessage.getDestination(), pw);
            }
            if (msmqMessage.getShareMode() != null) {
                DOMUtils.printAttribute(ATTR_SHARE_MODE, msmqMessage.getShareMode(), pw);
            }
            if (msmqMessage.getAccessMode() != null) {
                DOMUtils.printAttribute(ATTR_ACCESS_MODE, msmqMessage.getAccessMode(), pw);
            }
            if (msmqMessage.getMsgLookupID() != null) {
                Long msgLookupID = msmqMessage.getMsgLookupID();
                DOMUtils.printAttribute(ATTR_MSGLOOKUPID, msgLookupID.toString(), pw);
            }
            if (msmqMessage.getRetryInterval() != null) {
                Integer receiveInterval = msmqMessage.getRetryInterval();
                DOMUtils.printAttribute(ATTR_RETRY_INTERVAL, receiveInterval.toString(), pw);
            }
            if (msmqMessage.getMaxRetries() != null) {
                Integer maxRetries = msmqMessage.getMaxRetries();
                DOMUtils.printAttribute(ATTR_MAX_RETRIES, maxRetries.toString(), pw);
            }
            if (msmqMessage.getMessagePriority() != null) {
                Integer messagePriority = msmqMessage.getMessagePriority();
                DOMUtils.printAttribute(ATTR_MESSAGE_PRIORITY, messagePriority.toString(), pw);
            }
            if (msmqMessage.isAcknowledgement() != null) {
                DOMUtils.printAttribute(ATTR_ACKNOWLEDEMENT, msmqMessage.isAcknowledgement().toString(), pw);
            }
            if (msmqMessage.getMessageType() != null) {
                DOMUtils.printAttribute(ATTR_MESSAGE_TYPE, msmqMessage.getMessageType(), pw);
            }
            if (msmqMessage.getUseType() != null) {
                DOMUtils.printAttribute(ATTR_USE_TYPE, msmqMessage.getUseType(), pw);
            }
            if (msmqMessage.getEncodingStyle() != null) {
                DOMUtils.printAttribute(ATTR_ENCODING_STYLE, msmqMessage.getEncodingStyle(), pw);
            }
            if (msmqMessage.getMessagePart() != null) {
                DOMUtils.printAttribute(ATTR_PART, msmqMessage.getMessagePart(), pw);
            }
            if (msmqMessage.getTransaction() != null) {
                DOMUtils.printAttribute(ATTR_TRANSACTION, msmqMessage.getTransaction(), pw);
            }
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            pw.println("/>");
        }
    }

    /**
     * Unmarshall and element and return the extension type
     * 
     * @param Class
     * @param QName
     * @param Element
     * @param Definition
     * @param ExtensionRegistry
     * @return ExtensibilityElement
     * @throws WSDLException
     */

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
                                                                 QName elementType,
                                                                 Element el,
                                                                 Definition def,
                                                                 ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (MSMQConstants.QNAME_BINDING.equals(elementType)) {
            MSMQBinding msmqBinding = new MSMQBinding();
            returnValue = msmqBinding;
        } else if (MSMQConstants.QNAME_OPERATION.equals(elementType)) {
            MSMQOperation msmqOperation = new MSMQOperation();

            returnValue = msmqOperation;
        } else if (MSMQConstants.QNAME_ADDRESS.equals(elementType)) {
            MSMQAddress msmqAddress = new MSMQAddress();

            String address = DOMUtils.getAttribute(el, MSMQAddress.ATTR_ADDRESS);
            if (nonEmptyString(address)) {
                msmqAddress.setHostName(address);
            }
            returnValue = msmqAddress;
        } else if (MSMQConstants.QNAME_MESSAGE.equals(elementType)) {
            MSMQMessage msmqMessage = new MSMQMessage();

            String connectionmode = DOMUtils.getAttribute(el, ATTR_CONNECTION_MODE);
            if (nonEmptyString(connectionmode)) {
                msmqMessage.setConnectionMode(connectionmode);
            }

            String destination = DOMUtils.getAttribute(el, ATTR_DESTINATION);
            if (nonEmptyString(destination)) {
                msmqMessage.setDestination(destination);
            }

            String shareMode = DOMUtils.getAttribute(el, ATTR_SHARE_MODE);
            if (nonEmptyString(shareMode)) {
                msmqMessage.setShareMode(shareMode);
            }

            // Access Mode should be populated after MsgLookupID
            // bcz Access Mode logic depends upon MsgLookupID
            String msgLookupID = DOMUtils.getAttribute(el, ATTR_MSGLOOKUPID);
            if (nonEmptyString(msgLookupID)) {
                Long rMsgLookupID = new Long(msgLookupID);
                msmqMessage.setMsgLookupID(rMsgLookupID);
            }

            String accessMode = DOMUtils.getAttribute(el, ATTR_ACCESS_MODE);
            if (nonEmptyString(accessMode)) {
                msmqMessage.setAccessMode(accessMode);
            }

            String retryInterval = DOMUtils.getAttribute(el, ATTR_RETRY_INTERVAL);
            if (nonEmptyString(retryInterval)) {
                Integer rInterval = new Integer(retryInterval);
                msmqMessage.setRetryInterval(rInterval);
            }

            String maxRetries = DOMUtils.getAttribute(el, ATTR_MAX_RETRIES);
            if (nonEmptyString(maxRetries)) {
                Integer mRetries = new Integer(maxRetries);
                msmqMessage.setMaxRetries(mRetries);
            }

            String messagePriority = DOMUtils.getAttribute(el, ATTR_MESSAGE_PRIORITY);
            if (nonEmptyString(messagePriority)) {
                Integer mPriority = new Integer(messagePriority);
                msmqMessage.setMessagePriority(mPriority);
            }

            String acknowledgement = DOMUtils.getAttribute(el, ATTR_ACKNOWLEDEMENT);
            if (nonEmptyString(acknowledgement)) {
                msmqMessage.setAcknowledgement(new Boolean(acknowledgement));
            }

            String messageType = DOMUtils.getAttribute(el, ATTR_MESSAGE_TYPE);
            if (nonEmptyString(messageType)) {
                msmqMessage.setMessageType(messageType);
            }

            String useType = DOMUtils.getAttribute(el, ATTR_USE_TYPE);
            if (nonEmptyString(useType)) {
                msmqMessage.setUseType(useType);
            }

            String encodingStyle = DOMUtils.getAttribute(el, ATTR_ENCODING_STYLE);
            if (nonEmptyString(encodingStyle)) {
                msmqMessage.setEncodingStyle(encodingStyle);
            }

            String messagePart = DOMUtils.getAttribute(el, ATTR_PART);
            if (nonEmptyString(messagePart)) {
                msmqMessage.setMessagePart(messagePart);
            }

            String transaction = DOMUtils.getAttribute(el, ATTR_TRANSACTION);
            if (nonEmptyString(transaction)) {
                msmqMessage.setTransaction(transaction);
            }

            returnValue = msmqMessage;
        }
        return returnValue;
    }

    private boolean nonEmptyString(String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }
	public Map getEnvVariableMap() {
        return mEnvVariableMap;
    }

	protected boolean isAToken(String name) throws Exception {
    	boolean isToken = false;

        if (name.startsWith("${")) {
            if (name.endsWith("}")) {
                isToken = true;
            } else {
                throw new Exception(mMessages.getString("MSMQES_Invalid_token_name", name));
            }
        }

        return isToken;
    }

	 protected String getEnvVariableName(String aToken) throws Exception {
        String tokenName = null;

        if (aToken == null || "".equals(aToken)) {
            throw new Exception(mMessages.getString("MSMQES_Invalid_token_name", aToken));
        }

        tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("MSMQES_Invalid_empty_token_name", aToken));
        }

        return tokenName;

    }
}
