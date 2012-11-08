 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * MessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import javax.jbi.messaging.MessageExchange;

/**
 * This interface is a Handler to perform message exchanges when the component
 * receives the MessageExchange object from the delivery channel.
 * Implemenation of this interface should implement the processing of
 * the active, error, done status of the MessageExchange object according to
 * the MEP for which the MessageExchange object is created.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public interface MessageExchangeHandler extends Runnable {
    /**
     * sets the MessageExchange object to be processed
     * @param msgExchange MessageExchange object.
     */
    void setMessageExchange(MessageExchange msgExchange);
    /**
     * command interface method which will be invoked to process the MessageExchange
     * object set using setMessageExchange.
     */
    void processMessageExchange() throws Exception ;
    
}
