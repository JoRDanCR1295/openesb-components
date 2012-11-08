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
public interface RedeliveryListener {
    /**
     * Method to handle the redelivery of an inbound message if the
     * redelivery configuration is applied to the endpoint
     */
    void onRedelivery(MessageExchange exchange) throws MessagingException;
    
    /**
     * The framwork will set the exchange metadata associated with
     * an exchange
     *
     * @param messageExchangeId the exchange ID of the message exchange
     * @param exchangeMetaData the metadata needed to reinitiate a message exchange
     * @deprecated Use #linkMessageExchange(String, Object) instead
     */
    void setMessageExchangeId(String messageExchangeId, OperationMetaData exchangeMetaData, Object clienContext);
    
    /**
     * Associate a message exchange to the listener, for the purpose of having
     * access to a message exchange's information during a callback.
     * 
     * @param messageExchangeId The ID of the message exchange.
     * @param context           Opaque context to associate with the exchange
     */
    //void linkMessageExchange(String messageExchangeId, Object context);
    
    /**
     * Disassociates a message exchange from the listener, for when a listener
     * is no longer responsible for a given message exchange.
     * 
     * @param messageExchangeId The ID of the message exchange
     */
    //void unlinkMessageExchange(String messageExchangeId);
}
