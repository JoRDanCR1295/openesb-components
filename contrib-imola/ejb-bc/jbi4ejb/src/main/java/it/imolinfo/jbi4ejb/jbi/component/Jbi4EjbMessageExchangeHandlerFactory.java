/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.component;

import it.imolinfo.jbi4ejb.jbi.component.runtime.MessageExchangeHandler;
import it.imolinfo.jbi4ejb.jbi.component.runtime.MessageExchangeHandlerFactory;

import javax.jbi.messaging.MessageExchange;

/**
 * A factory for creating Jbi4EjbMessageExchangeHandler objects.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbMessageExchangeHandlerFactory implements MessageExchangeHandlerFactory {

    private Jbi4EjbSUManager suManager = null;
    
    /**
     * Instantiates a new jbi4 ejb message exchange handler factory.
     * 
     * @param sum the service unit manager
     */
    public Jbi4EjbMessageExchangeHandlerFactory (Jbi4EjbSUManager sum) {
        this.suManager = sum;
    }
    
    /**
     * creates DefaultMessageExchangeHandler.
     * 
     * @param msgExchange the message exchange
     * 
     * @return the <code>Jbi4EjbMessageExchangeHandler</code> MessageExchangeHandler 
     */
    public MessageExchangeHandler newHandler(MessageExchange msgExchange) {
        MessageExchangeHandler handler = new  Jbi4EjbMessageExchangeHandler(suManager);
        handler.setMessageExchange(msgExchange);        
        return handler;
    }
}
