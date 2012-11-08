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
 * @(#)SynchronousReplySupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * This class bridges the gap between clients who need a synchronous reply
 * and the asynchronous nature of the JBI NMS message exchange.
 * A client who can operate asynchronously instead should just implement
 * their own ReplyListener instead of blocking threads by using this class.
 *
 * Also the final JBI spec NMR now has methods to send synchronously.
 *
 */
public class SynchronousReplySupport implements ReplyListener {

// TODO: optimize by using monitors of listener instead of separate "request" objects
    private final static Messages mMessages = Messages.getMessages(SynchronousReplySupport.class);
    private final static Logger mLogger = Messages.getLogger(SynchronousReplySupport.class);
    private static Map pendingRequests = Collections.synchronizedMap(new HashMap());
    
    String messageExchangeId;
    
    /**
     * Inner class for handling requests, waiting on the object monitors
     */
    static class Request {
        String messageExchangeId;
	MessageExchange  response;
    }

    /**
     * Retrieves the reply supplied to this listener
     * Blocks the calling thread until a reply is available for this listener
     * - or if the reply is already available returns that immediately.
     */ 
    public MessageExchange waitForReply() throws MessagingException {
        
        if (messageExchangeId == null) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00790.No_message_exchange_set"));
        }

        Request request = (Request) pendingRequests.get(messageExchangeId);
        
        try {
            synchronized (request) {
                while (request.response == null) {
                    try {
                        request.wait();
                    } catch (InterruptedException ex) {
                        // ignore
                    }
                }
            }
        } finally {
            pendingRequests.remove(messageExchangeId);
        }
 
        return request.response;
    }
 
    /**
     * The logic receiving the asynchronous reply calls this to 
     * wake up the thread waiting for the reply and to return the response
     */
    static void returnResponse(String messageExchangeId, MessageExchange response) throws MessagingException { 
	Request request = (Request) pendingRequests.get(messageExchangeId);
	if (request == null) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, "HTTPBC-W00645.No_request_for_response", messageExchangeId);
            }
        } else {
            synchronized (request) {
                request.response = response;
                request.notify();
            }
        }
    }
    
    public void onReply(MessageExchange exchange) throws MessagingException {
        returnResponse(exchange.getExchangeId(), exchange);
    }
    
    public void setMessageExchangeId(String messageExchangeId, Object clientContext) {
        this.messageExchangeId = messageExchangeId;
        Request request = new Request();
	request.messageExchangeId = messageExchangeId;
	pendingRequests.put(messageExchangeId, request);
    }
    
    public void setMessageContextForCallback(Object obj1, Object obj2) {
        // do nothing
    }
    
    public void removeMessageExchangeId(String messageExchangeId) {
        pendingRequests.remove(messageExchangeId);
    }    
}
