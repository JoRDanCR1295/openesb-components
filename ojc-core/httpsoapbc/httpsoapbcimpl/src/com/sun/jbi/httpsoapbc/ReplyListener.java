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
 * @(#)ReplyListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 *
 */
public interface ReplyListener {
    /**
     * Method asynchronously invoked by the framework once a reply to a message exchange is available 
     */
    void onReply(MessageExchange exchange) throws MessagingException;
    
    /**
     * The framwork will set the AsyncProviderCallback instance corresponding to the message exchange id
     * @param messageExchangeId the exchange id of the message exchange initialized
     * @param clientContext returns the client provided object associated with the request
     */
    void setMessageExchangeId(String messageExchangeId, Object clientContext);
    
    /**
     * The framwork will set the message context corresponding to the AsyncProviderCallback instance
     * @param clientContext the AsyncProviderCallback instance
     * @param messageContext the web service message context associated with the AsyncProviderCallback instance
     */
    void setMessageContextForCallback(Object clientContext, Object messageContext);
    
    /**
     * The framwork will remove the corresponding message exchange.
     * @param messageExchangeId the exchange id of the message exchange initialized
     */
    void removeMessageExchangeId(String messageExchangeId);
}
