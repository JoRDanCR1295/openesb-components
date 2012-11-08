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
 * @(#)InboundReplyContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import javax.jms.Message;

import com.sun.jbi.jmsbc.extensions.JMSOperation;

/**
 *
 * Holds objects used by the asynchronous outbound message processor to
 * process the NMR reply it receives to complete an inbound
 * inout JBI message exchange (jms->nmr, nmr->jms).
 */
public class InboundReplyContext {
    private long requestInvocationTime;
    private Message jmsMessage;
    private InboundReplyListener listener;
    private boolean messageAckPending = false;
    private JMSOperation jmsOperation;
    private Endpoint endpoint;
    private String exchangeId;

    /** 
     * Creates a new instance of InboundReplyContext 
     * @param requestInvocationTime The time in milliseconds when the MessageExchange was received
     * @param jmsMessage The received JMS message 
     * @param listener The call back which will be processing the reply
     * 
     */
    public InboundReplyContext(long requestInvocationTime,
                               Message jmsMessage,
                               InboundReplyListener listener,
                               boolean messageAckPending,
                               JMSOperation jmsOp,
                               Endpoint ep,
                               String exchangeId) {
        this.requestInvocationTime = requestInvocationTime;
        this.jmsMessage = jmsMessage;
        this.listener = listener;
        this.messageAckPending = messageAckPending;
        this.jmsOperation = jmsOp;
        this.endpoint = ep;
        this.exchangeId = exchangeId;
    }
    
    /**
     * Returns the time in milliseconds when the MessageExchange (request) was received.
     * @return The time in milliseconds when the MessageExchange (request) was received.
     */
    public long getRequestInvocationTime() {
        return this.requestInvocationTime;
    }
    
    /**
     * Returns the JMS correlation ID from the received jms message
     * @return The JMS correlation ID or null if no correationID.
     */
    public String getJMSCorrelationID() {
        try {
            return this.jmsMessage.getJMSCorrelationID();
        } catch (Exception ex) {
            return null;
        }
    }
    
    /**
     * Returns the received JMS message.
     * @return The JMS message.
     */
    public Message getJMSMessage() {
        return this.jmsMessage;
    }
    
    /**
     * Returns the reply listener which will process the NMR reponse.
     * @return The inbound reply listener.
     */
    public InboundReplyListener getReplyListener() {
        return this.listener;
    }
    
    /**
     * Determines if the received JMS message needs to be ack'ed
     *
     * @return Returns true if JMS message delivery completion is pending
     *         on message ack; false otherwise.
     */
    public boolean messageAckPending() {
        return messageAckPending;
    }
    
    public JMSOperation getJMSOperation(){
    	return jmsOperation;
    }
    
    public Endpoint getEndpoint(){
    	return endpoint;
    }
    
    public String getExchangeId(){
    	return exchangeId;
    }
}
