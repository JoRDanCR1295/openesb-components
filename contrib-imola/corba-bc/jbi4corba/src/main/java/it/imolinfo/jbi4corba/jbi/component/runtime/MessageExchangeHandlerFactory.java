 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * MessageExchangeHandlerFactory.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import javax.jbi.messaging.MessageExchange;

/**
 * Factory interface to create the message exchange handlers.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
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
