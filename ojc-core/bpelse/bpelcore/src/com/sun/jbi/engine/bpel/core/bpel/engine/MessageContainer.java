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
 * @(#)MessageContainer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.Map;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;


/**
 * message container
 *
 * @author Sun Microsystems
 */
public interface MessageContainer {
    /** Data type message */
    public static final int MESSAGE = 2;

    /** Fault type message */
    public static final int FAULT = 4;

    /** DONE status message */
    public static final int STATUS_DONE = 11; 

    /** ERROR status message */
    public static final int STATUS_ERROR = 12; 

    /**
     * gets ID
     *
     * @return String message container ID
     */
    public String getId();

    /**
     * gets content
     *
     * @return Object content
     */
    public Object getContent();

    /**
     * check if it's status
     *
     * @return boolean: if it's status, returns true; otherwise, returns false
     */
    public boolean isStatus();

    /**
     * check if it's status
     *
     * @return boolean: if it's status and it is error, returns true; otherwise, returns false
     */
    public boolean isStatusError();

    /**
     * check if it's data
     *
     * @return boolean: if it's data, returns true; otherwise, returns false
     */
    public boolean isMessage();

    /**
     * check if it's fault
     *
     * @return boolean: if it's fault, returns true; otherwise, returns false
     */
    public boolean isFault();

    

    /**
     * return the value of the CRMPInvokeId that is sent on this Message
     * on the message exchange object query for the property 
     * "com.sun.jbi.messaging.messageid" the property that is returned 
     * must be of type String. In the case of inbound messages ie from 
     * Http-bc that are part of WSReliability, this string would be a 
     * combination of the properties "com.sun.jbi.messaging.messageid" and 
     * "com.sun.jbi.messaging.groupid".
     *
     * @return String: String value that represents the unique CRMPInvokeId sent on the message exchange object.
     */
    public String getCRMPInvokeId();
    
    /**
     * Gets the Transaction associated with the Message Exchange  
     * @return the Transaction
     */
    public Transaction getTransaction();
    
    /**
     * Sets the Transaction associated with the Message Exchange  
     * param Transaction 
     */
    public void setTransaction(Transaction transaction);
    
    /**
     * Getter that indicates that bpel-se has started a Tx
     * since the process tag had the attribute atomicTxType="Required"
     * this is used only in the context of bpel-se in atomic mode.
     * @return
     */
    public boolean isTxAtomicStarted();
    
    /**
     * Setter that indicates that bpel-se has started a Tx
     * since the process tag had the attribute atomicTxType="Required"
     * this is used only in the context of bpel-se in atomic mode.
     * This is set when the bpel-se starts a new Tx. 
     */
    public void setTxAtomicStarted();
    
    /**
     * Sets a <code>PropagationContext</code> on the MessageContainer.
     * @param propContext
     */
    public void setPropagationContext(PropagationContext propContext);
    
    /**
     * Returns the <code>PropagationContext</code> set on the MessageContainer. Returns null if a 
     * <code>PropagationContext</code> is not set.
     * @return
     */
    public PropagationContext getPropagationContext();
    
    public void startMeasurement(QName serviceName, String operation);
    
    public void endMeasurement();
    
    /**
     * The redelivery status object for the MessageExchange represented
     * by this container.
     * @param status instance of <com.sun.jbi.common.qos.redelivery.RedeliveryStatus>
     */
    public void setRedeliveryStatus(RedeliveryStatus status);
    
    /**
     * Returns the RedeliveryStatus for the MessageExchange represented 
     * by this container. This value can be null if redelivery is 
     * not configured for this MessageExchange(endpoints)
     * @return instance of <com.sun.jbi.common.qos.redelivery.RedeliveryStatus>
     */
    public RedeliveryStatus getRedeliveryStatus();

    /**
     * returns NM properties associated with the request when the container
     * holds the response/status otherwise null.
     * 
     * Service provider while processing requests might add/update NM properties
     * on the request, for example JMS message-id. To make these NMProperties
     * available to business process. bpel-engine reads them while processing
     * response/status and set it on the bpel input variable associated with Invoke.
     * 
     * @return NM properties associated with the request when the container
     *         holds the response/status otherwise null
     */
    public Map<String, Object> getNMProperties();

    /**
     * NM properties associated with the request read from the request and set
     * it on the container while processing response/status
     * 
     * @param properties
     *            NM properties associated with the request
     */
    public void setNMProperties(Map<String, Object> properties);
    
}
