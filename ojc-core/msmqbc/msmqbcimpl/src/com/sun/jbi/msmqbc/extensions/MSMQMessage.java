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
 * @(#)MSMQMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.extensions;

import java.io.Serializable;
import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * Represents msmq:message extensibility element
 * 
 * @author Sun Microsystems
 */
public class MSMQMessage implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    public static final String USE_TYPE_LITERAL = "literal";

    public static final String USE_TYPE_ENCODED = "encoded";

    private QName fieldElementType = MSMQConstants.QNAME_OPERATION;

    private BindingOperation bindingOp;

    private Boolean fieldRequired = null;

    private String mep;

    private String shareMode = "DENY_NONE"; // Default value is DENY_NONE

    private String accessMode = "SEND_ACCESS"; // Default value is SEND_ACCESS

    private String actionCode = "ACTION_RECEIVE"; // Default value is ACTION_RECEIVE

    private String messageType = "array of bytes"; // Default value is array of bytes

    private String connectionMode;

    private String destination;

    private String useType = USE_TYPE_LITERAL; // default

    private String encodingStyle;

    private String messagePart;

    private String transaction = "NoTransaction"; // Default value is NoTransaction

    private Integer maxRetries = new Integer(2);// Default value is 2

    private Integer retryInterval = new Integer(5000); // Default value is 5 sec

    private Integer messagePriority = new Integer(3); // Default is 3

    private Long msgLookupID = null;

    private Boolean acknowledgement = Boolean.FALSE;

    /**
     * Get the elementType attribute value
     * 
     * @return ElementType of msmq:message wsdl extension element
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the elementType attribute
     * 
     * @param ElementType of msmq:message wsdl extension element
     */
    public void setElementType(QName elementType) {
        this.fieldElementType = elementType;
    }

    /**
     * Get the Access Mode attribute value
     * 
     * @return Access Mode of msmq:message wsdl extension element
     */
    public String getAccessMode() {
        return accessMode;
    }

    /**
     * Set the Access Mode attribute value
     * 
     * @param Action Mode to retrieve the value from queue
     */
    public void setAccessMode(String accessMode) {
        this.accessMode = accessMode;
        // if lookupID is not null then we should lookup message using message identifier
        if (accessMode.equals("RECEIVE_ACCESS") && (msgLookupID != null)) {
            this.actionCode = "LOOKUP_RECEIVE_CURRENT";
        } else if (accessMode.equals("PEEK_ACCESS") && (msgLookupID != null)) {
            this.actionCode = "LOOKUP_PEEK_CURRENT";
        } else if (accessMode.equals("SEND_ACCESS")) {
            this.actionCode = "ACTION_SEND";
        } else if (accessMode.equals("RECEIVE_ACCESS")) {
            this.actionCode = "ACTION_RECEIVE";
        } else if (accessMode.equals("PEEK_ACCESS")) {
            this.actionCode = "ACTION_PEEK_CURRENT";
        }
    }

    /**
     * Get the ActionCode attribute value
     * 
     * @return Action Code of msmq:message wsdl extension element
     */
    public String getActionCode() {
        return actionCode;
    }

    /**
     * Get the BindingOperation attribute value
     * 
     * @return BindingOperation of the msmq:extension wsdl element
     */
    public BindingOperation getBindingOp() {
        return bindingOp;
    }

    /**
     * Set the BindingOperation attribute value
     * 
     * @param BindingOperation for the msmq:message wsdl extension element
     */
    public void setBindingOp(BindingOperation bindingOp) {
        this.bindingOp = bindingOp;
    }

    /**
     * Get the Connection Mode attribute value
     * 
     * @return connectionmode from the msmq:message wsdl extension element
     */
    public String getConnectionMode() {
        return connectionMode;
    }

    /**
     * Set the Connection Mode attribute value
     * 
     * @param connection mode for the msmq:message wsdl extension element
     */
    public void setConnectionMode(String connectionMode) {
        this.connectionMode = connectionMode;
    }

    /**
     * Get the FileElementType property
     * 
     * @return QName
     */
    public QName getFieldElementType() {
        return fieldElementType;
    }

    /**
     * Set the FileElementType property
     * 
     * @param QName
     */
    public void setFieldElementType(QName fieldElementType) {
        this.fieldElementType = fieldElementType;
    }

    /**
     * Get the required property
     * 
     * @return Boolean
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set the required property
     * 
     * @param Boolean
     */
    public void setRequired(Boolean fieldRequired) {
        this.fieldRequired = fieldRequired;
    }

    /**
     * Get the MessageExchangePattern value
     * 
     * @return message exchange pattern
     */
    public String getMep() {
        return mep;
    }

    /**
     * Set the MessageExchangePattern value
     * 
     * @param message extension pattern
     */
    public void setMep(String mep) {
        this.mep = mep;
    }

    /**
     * Get the ReceiveInterval property value
     * 
     * @return receive interval from msmq:message wsdl extension element
     */
    public Integer getRetryInterval() {
        return retryInterval;
    }

    /**
     * Set the ReceiveInterval property value
     * 
     * @param receive interval to msmq:message wsdl extension element
     */
    public void setRetryInterval(Integer retryInterval) {
        this.retryInterval = retryInterval;
    }

    /**
     * Get the MaxRetries property value
     * 
     * @return maxRetries value from msmq:message wsdl extension element
     */
    public Integer getMaxRetries() {
        return maxRetries;
    }

    /**
     * Set the MaxRetries property value
     * 
     * @param maxRetries to msmq:message wsdl extension element
     */
    public void setMaxRetries(Integer maxRetries) {
        this.maxRetries = maxRetries;
    }

    /**
     * Get the share mode property value
     * 
     * @return share mode of msmq:message wsdl extension element
     */
    public String getShareMode() {
        return shareMode;
    }

    /**
     * Set the share mode property value
     * 
     * @param share mode from msmq:message wsdl extension element
     */
    public void setShareMode(String shareMode) {
        this.shareMode = shareMode;
    }

    /**
     * Get the destination property value
     * 
     * @return destination value from msmq:message wsdl extension element
     */
    public String getDestination() {
        return destination;
    }

    /**
     * Set the destination property value
     * 
     * @param destination value for msmq:message wsdl extension element
     */
    public void setDestination(String destination) {
        this.destination = destination;
    }

    /**
     * Get the message parts of the msmq:message extensiblity element
     * 
     * @return messagePart representing the part to be considered in the message
     */
    public String getMessagePart() {
        return messagePart;
    }

    /**
     * Set the message parts of the msmq:message extensibility element
     * 
     * @param messagePart represents the part of message
     */
    public void setMessagePart(String messagePart) {
        this.messagePart = messagePart;
    }

    /**
     * Get the MsgLookupID of the msmq:message extensibility element
     * 
     * @return Return the value if LookupID
     */
    public Long getMsgLookupID() {
        return msgLookupID;
    }

    /**
     * Set the msglookupID of the msmq:message extensibility element
     * 
     * @param msglookupID represents the message identifier value used to lookup in queue
     */
    public void setMsgLookupID(Long msgLookupID) {
        this.msgLookupID = msgLookupID;
    }

    /**
     * Get the MessagePriority property
     * 
     * @return Integer
     */
    public Integer getMessagePriority() {
        return messagePriority;
    }

    /**
     * Set the MessagePriority property
     * 
     * @param Integer
     */
    public void setMessagePriority(Integer messagePriority) {
        this.messagePriority = messagePriority;
    }

    /**
     * Get the Acknowledgement property
     * 
     * @return Boolean
     */
    public Boolean isAcknowledgement() {
        return this.acknowledgement;
    }

    /**
     * Set the Acknowledgement property
     * 
     * @param Boolean
     */
    public void setAcknowledgement(Boolean acknowledgement) {
        this.acknowledgement = acknowledgement;
    }

    /**
     * Get the Message Type property
     * 
     * @return message type of MSMQ Body
     */
    public String getMessageType() {
        return this.messageType;
    }

    /**
     * Put the Message Type property
     * 
     * @param message type of MSMQ Body
     */
    public void setMessageType(String messageType) {
        this.messageType = messageType;
    }

    /**
     * set the use type for the msmq:message
     * 
     * @String useType value either literal or encoded
     */
    public void setUseType(String useType) {
        this.useType = useType;
    }

    /**
     * get the use type of the msmq:message
     * 
     * @return use type of the message either literal or encoded
     */
    public String getUseType() {
        return this.useType;
    }

    /**
     * set the encoding style of msmq:message
     * 
     * @param any URI of the encoding style
     */
    public void setEncodingStyle(String encodingStyle) {
        this.encodingStyle = encodingStyle;
    }

    /**
     * get the encoding style of msmq:message
     * 
     * @return any uri representing encoding style or null
     */
    public String getEncodingStyle() {
        return this.encodingStyle;
    }

    /**
     * set the transaction type for <msmq:message>
     * 
     * @param one of the transaction support types
     */
    public void setTransaction(String transaction) {
        this.transaction = transaction;
    }

    /**
     * get the transaction type of <msmq:message>
     * @return the transaction type of <msmq:message>
     */

    public String getTransaction() {
        return this.transaction;
    }

    /**
     * get the complete destination name
     * @return destination of msmq message
     */
    // main purpose of this method to hold complete Queue Name
    // so that this can be used while logging.
    public String getQueueName() {
        if (connectionMode.equals("QueueName")) {
            return connectionMode + "\\" + destination;
        } else {
            return destination;
        }
    }
}
