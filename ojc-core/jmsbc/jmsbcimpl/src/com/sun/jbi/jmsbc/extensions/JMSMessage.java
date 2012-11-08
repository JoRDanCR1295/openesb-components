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
 * @(#)JMSMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 */
public class JMSMessage implements ExtensibilityElement, Serializable, Cloneable{
    
    private static final long serialVersionUID = 1L;

    private QName fieldElementType = JMSConstants.QNAME_MESSAGE;
    
    private Boolean fieldRequired = new Boolean(false);
        
    public static final String ATTR_MESSAGE_TYPE = "messageType";
    public static final String ATTR_TEXTPART = "textPart";
    public static final String ATTR_CORRELATION_ID_PART = "correlationIdPart";
    public static final String ATTR_DELIVERY_MODE_PART = "deliveryModePart";    
    public static final String ATTR_PRIORITY_PART = "priorityPart";
    public static final String ATTR_TYPE_PART = "typePart";
    public static final String ATTR_MESSAGE_ID_PART = "messageIDPart";
    public static final String ATTR_REDELIVERED_PART = "redeliveredPart";
    public static final String ATTR_TIMESTAMP_PART = "timestampPart";
    public static final String ATTR_USE = "use";
    public static final String ATTR_ENCODING_STYLE = "encodingStyle";
    public static final String ATTR_FORWARD_AS_ATTACHMENT = "forwardAsAttachment";
    public static final String ATTR_BYTES_PART = "bytesPart";

    public static final String ATTR_USE_TYPE_LITERAL = "literal";
    public static final String ATTR_USE_TYPE_ENCODED = "encoded";
    
    private JMSProperties jmsProperties = null;
    private JMSMapMessage jmsMapMessage = null;
    private String messageType = null;
    private String use = ATTR_USE_TYPE_LITERAL; // default 
    private String textPart = null;
    private String correlationIdPart = null;
    private String deliveryModePart = null;
    private String priorityPart = null;
    private String typePart = null;    
    private String messageIDPart = null;
    private String redeliveredPart = null;
    private String timestampPart = null;
    private String encodingStyle = null;
    private boolean forwardAsAttachment = false;
    private String bytesPart = null;
    
    /** Creates a new instance of JMSMessage */
    public JMSMessage() {
    }
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Gets the extensibility properties element
     * @return The extensibility properties element
     */
    public JMSProperties getProperties() {
        return jmsProperties;
    }

    /**
     * Sets the extensibility properties element
     * @param val An instance of JMSProperties
     */
    public void setProperties(JMSProperties val) {
        jmsProperties = val;
    }

    /**
     * Gets the extensibility mapmessage element
     * @return The extensibility mapmessage element
     */
    public JMSMapMessage getMapMessage() {
        return jmsMapMessage;
    }

    /**
     * Sets the extensibility mapmessage element
     * @param val An instance of JMSMapMessage
     */
    public void setMapMessage(JMSMapMessage val) {
        jmsMapMessage = val;
    }

    /**
     * Gets the messageType attribute
     * @return The messsage part for messageType
     */
    public String getMessageType() {
        return messageType;
    }

    /**
     * Sets the messageType attribute
     * @param val The messsage part for messageType
     */
    public void setMessageType(String val) {
        messageType = val;
    }    

    /**
     * Sets the use attribute
     * @param val The messsage part for use
     */
    public String getUse() {
        return use;
    }    
    
    /**
     * Sets the use attribute
     * @param val The message part for use
     */
    public void setUse(String val) {
        use = val;
    }    
    
    /**
     * Gets the textPart attribute
     * @return The messsage part for TextMessage text
     */
    public String getTextPart() {
        return textPart;
    }

    /**
     * Sets the textPart attribute
     * @param The messsage part for TextMessage text
     */
    public void setTextPart(String val) {
        textPart = val;
    }    
    
    /**
     * Gets the correlationIdPart attribute
     * @return The messsage part for correlationId
     */    
    public String getCorrelationIdPart() {
        return correlationIdPart;
    }

    /**
     * Sets the correlationIdPart attribute
     * @param val The messsage part for correlationId
     */
    public void setCorrelationIdPart(String val) {
        correlationIdPart = val;
    }

    /**
     * Gets the deliveryModePart attribute
     * @return The messsage part for deliveryMode
     */
    public String getDeliveryModePart() {
        return deliveryModePart;
    }

    /**
     * Sets the deliveryModePart attribute
     * @param val The messsage part for deliveryMode
     */
    public void setDeliveryModePart(String val) {
        deliveryModePart = val;
    }

    /**
     * Gets the priorityPart attribute
     * @return The messsage part for priority
     */
    public String getPriorityPart() {
        return priorityPart;
    }

    /**
     * Sets the messageIDPart attribute
     * @param val The messageID part for deliveryMode
     */
    public void setPriorityPart(String val) {
        priorityPart = val;
    }

    /**
     * Gets the type attribute
     * @return The messsage part for type
     */
    public String getTypePart() {
        return typePart;
    }

    /**
     * Sets the type attribute
     * @param val The message part for type
     */
    public void setTypePart(String val) {
        typePart = val;
    }    

    /**
     * Gets the messageID attribute
     * @return The messsage part for messageID
     */
    public String getMessageIDPart() {
        return messageIDPart;
    }

    /**
     * Sets the messageIDPart attribute
     * @param val The messageID part for deliveryMode
     */
    public void setMessageIDPart(String val) {
        messageIDPart = val;
    }

    /**
     * Gets the redeliveredPart attribute
     * @return The messsage part for redelivered flag
     */
    public String getRedeliveredPart() {
        return redeliveredPart;
    }

    /**
     * Sets the redeliveredPart attribute
     * @param val The redelivered part for redelivered flag
     */
    public void setRedeliveredPart(String val) {
        redeliveredPart = val;
    }
    
    /**
     * Gets the timestampPart attribute
     * @return The messsage part for timestamp
     */
    public String getTimestampPart() {
        return timestampPart;
    }

    /**
     * Sets the timestampPart attribute
     * @param val The messageID part for timestamp
     */
    public void setTimestampPart(String val) {
        timestampPart = val;
    }
    
    /**
     * Sets encoding style attribute
     * @param val The encodingStyle part for 
     */
    public void setJMSEncodingStyle(String val) {
        encodingStyle = val;
    }    

    /**
     * Gets the encoding style attribute
     * @return The encoding style part for 
     */
    public String getJMSEncodingStyle() {
        return encodingStyle;
    }
        
    /**
     * String format of the object
     * @return The String format of the object
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMSMessage (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nProperties=" + 
                (jmsProperties!=null?jmsProperties.toString():"null"));
        strBuf.append("\nMapMessageParts=" +                
                (jmsMapMessage!=null?jmsMapMessage.toString():"null"));
        strBuf.append("\nMessageType=" + messageType);
        strBuf.append("\nTextPart=" + textPart);
        strBuf.append("\nCorrelationIDPart=" + correlationIdPart);
        strBuf.append("\nDeliveryModePart=" + deliveryModePart);
        strBuf.append("\nMessageIDPart=" + messageIDPart);
        strBuf.append("\nPriorityPart=" + priorityPart);
        strBuf.append("\nTypePart=" + typePart);
        strBuf.append("\nRedeliveredPart=" + redeliveredPart);
        strBuf.append("\nTimestampPart=" + timestampPart);
        strBuf.append("\nUse=" + use);        
        strBuf.append("\nEncoding Style=" + encodingStyle);
        strBuf.append("\nForward as Attachment=" + forwardAsAttachment);
        strBuf.append("\n");
        return strBuf.toString();
    }

	public boolean forwardAsAttachment() {
		return forwardAsAttachment;
	}

	public void setForwardAsAttachment(boolean forwardAsAttachment) {
		this.forwardAsAttachment = forwardAsAttachment;
	}

	public String getBytesPart() {
		return bytesPart;
	}

	public void setBytesPart(String bytesPart) {
		this.bytesPart = bytesPart;
	}

	public JMSMessage getCopy(){
		JMSMessage copy = null;
		try {
			copy = (JMSMessage)super.clone();
		} catch (CloneNotSupportedException e) {
		}
		return copy;
	}

}
