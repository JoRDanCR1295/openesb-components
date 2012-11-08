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
 * @(#)MessageExchangeSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.ws.handler.MessageContext;

public class MessageExchangeSupport {

    private static final Messages mMessages =
        Messages.getMessages(MessageExchangeSupport.class);
    private final static Logger mLogger =
        Messages.getLogger(MessageExchangeSupport.class);
    
    static Map<String, ReplyListener> replyListeners =
            Collections.synchronizedMap(new HashMap<String, ReplyListener>());
    
    static Map<String, RedeliveryListener> redeliveryListeners =
            Collections.synchronizedMap(new HashMap<String, RedeliveryListener>());
    
    /** Creates a new instance of MessageExchangeSupport */
    public MessageExchangeSupport() {}
 
    /**
     * Add a reply listener for a message exchange
     * @param messageExchangeId the ID of the exchange to register a reply listener for
     * @param listener The reply listener to notify when a reply is available
     * @param clientContext A client supplied object to return to the reply listener
     */
    public static void addReplyListener(String messageExchangeId, ReplyListener listener, Object clientContext) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Adding reply listener for messsage exchange:" + messageExchangeId);
        }
        listener.setMessageExchangeId(messageExchangeId, clientContext);
        replyListeners.put(messageExchangeId, listener);
    }
    
    /**
     * Add a reply listener for a message exchange with message context
     * @param messageExchangeId the ID of the exchange to register a reply listener for
     * @param listener The reply listener to notify when a reply is available
     * @param clientContext A client supplied object to return to the reply listener
     * @param messageContext The web service message context associated with the invoke
     */
    public static void addReplyListener(String messageExchangeId, ReplyListener listener, Object clientContext, MessageContext messageContext) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Adding reply listener for messsage exchange:" + messageExchangeId);
        }
        if (clientContext != null) {
            listener.setMessageExchangeId(messageExchangeId, clientContext);
        }
        if (messageContext != null) {
            listener.setMessageContextForCallback(clientContext, messageContext);
        }
        replyListeners.put(messageExchangeId, listener);
    }
    
    /**
     * Remove the reply listener 
     * @param messageExchangeId the exchange ID for the reply listener to remove
     */
    public static void removeReplyListener(String messageExchangeId) {
        ReplyListener listener = replyListeners.remove(messageExchangeId);
    }
    
    /**
     * Remove the reply listener for redelivered exchanges
     * @param messageExchangeId the exchange ID for the reply listener to remove
     */
    public static void removeReplyListener(String messageExchangeId, boolean isRedelivered) {
        ReplyListener listener = replyListeners.remove(messageExchangeId);
        if (isRedelivered) {
            listener.removeMessageExchangeId(messageExchangeId);
        }
    }
    
    /**
     * Notify the reply listener of a reply being available
     * To support MEPs where there might be multiple supplies, this does not remove the reply listener
     * from the MessageExchangeSupport.
     * Use <code>removeReplyListener<code> to explicitly remove the listener when the MEP is complete.
     * @param mep the message exchange instance
     */
    public static void notifyOfReply(MessageExchange mep) throws MessagingException {
        String messageExchangeId = mep.getExchangeId();
        ReplyListener listener = (ReplyListener) replyListeners.get(messageExchangeId);
        
        if (listener != null) {
            listener.onReply(mep);
        } else {
            throw new MessagingException("Cannot process reply exchange "
                    + messageExchangeId + " because no reply listener has been registered");
        }
    }
    
    /**
     * Add a redelivery listener for a message exchange
     * @param messageExchangeId the ID of the exchange to register a reply listener for
     * @param listener The redelivery listener to notify when redelivery criteria is met
     * @param exchangeMetaData the metadata needed to reinitiate a message exchange
     */
    //public static void addRedeliveryListener(String messageExchangeId, RedeliveryListener listener, Object exchangeMetaData) {
    	public static void addRedeliveryListener(String messageExchangeId, RedeliveryListener listener, 
    	                                         OperationMetaData exchangeMetaData, Object clientContext) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Adding redelivery listener for messsage exchange:" + messageExchangeId);
        }
        listener.setMessageExchangeId(messageExchangeId, exchangeMetaData, clientContext);
        redeliveryListeners.put(messageExchangeId, listener);
    }
    
    /**
     * Remove the redelivery listener 
     * @param messageExchangeId the exchange ID for the redelivery listener to remove
     */
    public static void removeRedeliveryListener(String messageExchangeId) {
        RedeliveryListener listener = redeliveryListeners.remove(messageExchangeId);
    }
    
    /**
     * Notify the redelivery listener of a re-send of the message exchange if the redelivery
     * criteria is met.
     * Use <code>removeRedeliveryListener<code> to explicitly remove the listener when retry limit is exhausted.
     * @param mep the message exchange instance
     */
    public static void notifyOfRedelivery(MessageExchange mep) throws MessagingException {
        String messageExchangeId = mep.getExchangeId();
        RedeliveryListener listener = (RedeliveryListener) redeliveryListeners.get(messageExchangeId);
        
        if (listener != null) {
            listener.onRedelivery(mep);
        } else {
            throw new MessagingException("Cannot redelivery the message exchange "
                    + messageExchangeId + " because no redelivery listener has been registered");
        }
    }
}