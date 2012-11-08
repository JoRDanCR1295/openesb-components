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
 * @(#)JMSOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 */
public class JMSOperation implements ExtensibilityElement, Serializable, Cloneable{

    private static final long serialVersionUID = 1L;
    
    public static final String VERB_READ = "read";

    // common
    public static final String ATTR_DESTINATION = "destination";
    public static final String ATTR_DESTINATION_TYPE = "destinationType";
    public static final String ATTR_TRANSACTION = "transaction";
    
    // provider (outbound)
    public static final String ATTR_TIME_TO_LIVE = "timeToLive";
    public static final String ATTR_DELIVERY_MODE = "deliveryMode";
    public static final String ATTR_PRIORITY = "priority";
    public static final String ATTR_DISABLE_MESSAGE_ID = "disableMessageID";
    public static final String ATTR_DISABLE_MESSAGE_TIMESTAMP = "disableMessageTimeStamp";
    public static final String ATTR_TIMEOUT = "timeout";
    
    // consumer (inbound)
    public static final String ATTR_CLIENT_ID = "clientID";
    public static final String ATTR_MESSAGE_SELECTOR = "messageSelector";
    public static final String ATTR_VALIDATE_MESSAGE_SELECTOR = "validateMessageSelector";
    public static final String ATTR_SUBSCRIPTION_DURABILITY = "subscriptionDurability";
    public static final String ATTR_SUBSCRIPTION_NAME = "subscriptionName";    
    public static final String ATTR_BATCH_SZIE = "batchSize";
    public static final String ATTR_MAX_CONCURRENT_CONSUMERS = "maxConcurrentConsumers";
    public static final String ATTR_CONCURRENCY_MODE = "concurrencyMode";
    public static final String ATTR_REDELIVERY_HANDLING = "redeliveryHandling";
    
    //For sync receive
    public static final String ATTR_VERB = "verb";
    
    // nested elements
    private JMSOptions options;
    
    private String destination;
    private String destinationType = JMSConstants.TOPIC;
    private String transaction = JMSConstants.TRANSACTION_NONE;
    
    private String deliveryMode;
    private Long timeToLive;
    private Integer priority;
    private Boolean disableMessageID;
    private Boolean disableMessageTimeStamp;
    private Long timeout = new Long(5 * 60000); // 5 minutes
    
    private String clientID;
    private String messageSelector;
    private Boolean validateMessageSelector = Boolean.valueOf(true);
    private String subscriptionDurability;
    private String subscriptionName;
    private Integer batchSize;
    private Integer maxConcurrentConsumers;
    private String redeliveryHandling;
    private String concurrencyMode = "sync";
    
    private QName fieldElementType = JMSConstants.QNAME_OPERATION;

    private Boolean fieldRequired = null;
    
    private BindingOperation mBindingOp;
    private String mep;        
    private String verb = "";
    
    public JMSOperation() {
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
    
    public void setBindingOperation (BindingOperation bindingOp) {
        this.mBindingOp = bindingOp;
    }
    
    public BindingOperation getBindingOperation() {
        return mBindingOp;
    }
    
    public void setMEP (String mep) {
        this.mep = mep;
    }
    
    public String getMEP () {
        return mep;
    }

    /**
     * Get options extensiblity element
     * @return JMSOptions
     */
    public JMSOptions getOptions() {
        return options;
    }
    
    /**
     * Set options extensiblity element
     * @param val JMSOptions
     */
    public void setOptions(JMSOptions val) {
        options = val;
    }
    
    /**
     * Get destination attribute
     * @return String
     */
    public String getDestination() {
        return destination;
    }

    /**
     * Set destination attribute
     * @param val String
     */
    public void setDestination(String val) {
        destination = val;
    }

    /**
     * Get destinationType attribute
     * @return String
     */
    public String getDestinationType() {
        return destinationType;
    }

    /**
     * Set destinationType attribute
     * @param val String
     */
    public void setDestinationType(String val) {
        destinationType = val;
    }
    
    /**
     * Gets the transaction attribute
     * @return The value of the transaction attribute
     */
    public String getTransaction() {
        return transaction;
    }

    /**
     * Sets the value for the transaction attribute
     * @param val The value for the transaction attribute
     */
    public void setTransaction(String val) {
        transaction = val;
    }    
    
    /**
     * Get deliveryMode attribute
     * @return String
     */
    public String getDeliveryMode() {
        return this.deliveryMode;
    }

    /**
     * Set deliveryMode attribute
     * @param val String
     */
    public void setDeliveryMode(String val) {
        deliveryMode = val;
    }

    /**
     * Get timeToLive attribute
     * @return Long
     */
    public Long getTimeToLive() {
        return timeToLive;
    }

    /**
     * Set timeToLive attribute
     * @param val Long
     */
    public void setTimeToLive(Long val) {
        timeToLive = val;
    }

    /**
     * Get priority attribute
     * @return Integer
     */
    public Integer getPriority() {
        return priority;
    }

     /**
     * Set priority attribute
     * @param val Integer
     */
   public void setPriority(Integer val) {
        priority = val;
    }

     /**
     * Get disableMessageID attribute
     * @return Boolean
     */
   public Boolean getDisableMessageID() {
        return disableMessageID;
    }

     /**
     * Set disableMessageID attribute
     * @param val Boolean
     */
    public void setDisableMessageID(Boolean val) {
        disableMessageID = val;
    }

      /**
     * Get disableMessageTimeStamp attribute
     * @return Boolean
     */
   public Boolean getDisableMessageTimeStamp() {
        return this.disableMessageTimeStamp;
    }

     /**
     * Set disableMessageTimeStamp attribute
     * @param val Boolean
     */
    public void setDisableMessageTimeStamp(Boolean val) {
        disableMessageTimeStamp = val;
    }

    /**
     * Get timeout attribute
     * @return Long
     */
    public Long getTimeout() {
        return timeout;
    }

    /**
     * Set timeout attribute
     * @param val Long
     */
    public void setTimeout(Long val) {
        timeout = val;
    }
    
    /**
     * Get clientID attribute
     * @return String
     */
    public String getClientID() {
        return clientID;
    }

    /**
     * Set clientID attribute
     * @param val String
     */
    public void setClientID(String val) {
        clientID = val;
    }
    
    /**
     * Get messageSelector attribute
     * @return String
     */
    public String getMessageSelector() {
        return messageSelector;
    }

    /**
     * Set messageSelector attribute
     * @param val String
     */
    public void setMessageSelector(String val) {
    	if(val == null)
    		return;
        messageSelector = val.trim();
    }

     /**
     * Get validateMessageSelector attribute
     * @return Boolean
     */
   public Boolean getValidateMessageSelector() {
        return validateMessageSelector;
    }

     /**
     * Set validateMessageSelector attribute
     * @param val Boolean
     */
    public void setValidateMessageSelector(Boolean val) {
        validateMessageSelector = val;
    }
    
    /**
     * Get subscriptionDurability attribute
     * @return String
     */
    public String getSubscriptionDurability() {
        return subscriptionDurability;
    }

    /**
     * Set subscriptionDurability attribute
     * @param val String
     */
    public void setSubscriptionDurability(String val) {
        subscriptionDurability = val;
    }

    /**
     * Get subscriptionName attribute
     * @return String
     */
    public String getSubscriptionName() {
        return subscriptionName;
    }

    /**
     * Set subscriptionName attribute
     * @param val String
     */
    public void setSubscriptionName(String val) {
        subscriptionName = val;
    }
        
    /**
     * Get batchSize attribute
     * @return Integer
     */
    public Integer getBatchSize() {
        return batchSize;
    }

    /**
     * Set batchSize attribute
     * @param val Integer
     */
    public void setBatchSize(Integer val) {
        batchSize = val;
    }    

    /**
     * Get maxConcurrentConsumers attribute
     * @return Integer
     */
    public Integer getMaxConcurrentConsumers() {
        return maxConcurrentConsumers;
    }

    /**
     * Set maxConcurrentConsumers attribute
     * @param val Integer
     */
    public void setMaxConcurrentConsumers(Integer val) {
        maxConcurrentConsumers = val;
    }    

    /**
     * Get redeliveryHandling attribute
     * @return String
     */
    public String getRedeliveryHandling() {
        return redeliveryHandling;
    }

    /**
     * Set subscriptionName attribute
     * @param val String
     */
    public void setRedeliveryHandling(String val) {
        redeliveryHandling = val;
    }

    /**
     * Get concurrencyMode attribute
     * @return String
     */
    public String getConcurrencyMode() {
        return concurrencyMode;
    }

    /**
     * Set subscriptionName attribute
     * @param val String
     */
    public void setConcurrencyMode(String val) {
        concurrencyMode = val;
    }
    
    public String getVerb() {
		return verb;
	}

	public void setVerb(String verb) {
		if(verb!=null){
			this.verb = verb.trim();
		}
	}

	public JMSOperation getCopy(){
		JMSOperation copy = null;
		try {
			copy = (JMSOperation)super.clone();
		} catch (CloneNotSupportedException e) {
		}
		return copy;
	}

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMS Operation (" + fieldElementType + "):");
        strBuf.append("\nBindingOperation=" + (mBindingOp==null?"not known yet" : mBindingOp.toString()));
        strBuf.append("\nMEP="+mep);
        strBuf.append("\nOptions=" + 
                (options!=null?options.toString():"null"));
        strBuf.append("\nDestination=" + destination);
        strBuf.append("\nDestinationType=" + destinationType);
        strBuf.append("\nTransaction=" + transaction);
        strBuf.append("\nTimeToLive=" + timeToLive);
        strBuf.append("\nDeliveryMode=" + deliveryMode);
        strBuf.append("\nPriority=" + priority);
        strBuf.append("\nDisableMessageID=" + disableMessageID);
        strBuf.append("\nDisableMessageTimeStamp=" + disableMessageTimeStamp);
        strBuf.append("\nTimeout=" + timeout);
        strBuf.append("\nClientID=" + clientID);
        strBuf.append("\nMessageSelector=" + messageSelector);
        strBuf.append("\nValidateMessageSelector=" + validateMessageSelector);
        strBuf.append("\nSubscriptionDurability=" + subscriptionDurability);
        strBuf.append("\nSubscriptionName=" + subscriptionName);
        strBuf.append("\nBatchSize=" + batchSize);        
        strBuf.append("\nMaxConcurrentConsumers=" + maxConcurrentConsumers);        
        strBuf.append("\nConcurrencyMode=" + concurrencyMode);        
        strBuf.append("\nRedeliveryHandling=" + redeliveryHandling);        
        strBuf.append("\nVerb=" + verb);        
        return strBuf.toString();
    }
}
