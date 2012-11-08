/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * MessageExchangeHandlerFactory.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import javax.jbi.messaging.MessageExchange;

/**
 * factory interface to create the message exchange handlers.
 * @author Sun Microsystems, Inc.
 */
public interface MessageExchangeHandlerFactory  {
    
    /**
     * creates new MessageExchangeHandler
     * @param msgExchange MessageExchange Object for which the handler will be created
     * @return MessageExchangeHandler for the MessageExchange object
     */
    MessageExchangeHandler newHandler(MessageExchange msgExchange);
    
    /**
     * Default MessageExchangeHandlerFactory implementation that returns the
     * DefaultMessageExchangeHandler for MessageExchange object
     */
    public static class DefaultMessageExchangeHandlerFactory implements MessageExchangeHandlerFactory {
        
        /**
         * creates DefaultMessageExchangeHandler
         * @param msgExchange
         * @return
         */
        public MessageExchangeHandler newHandler(MessageExchange msgExchange) {
            MessageExchangeHandler handler = new  DefaultMessageExchangeHandler();
            handler.setMessageExchange(msgExchange);
            return handler;
        }
    }
}
