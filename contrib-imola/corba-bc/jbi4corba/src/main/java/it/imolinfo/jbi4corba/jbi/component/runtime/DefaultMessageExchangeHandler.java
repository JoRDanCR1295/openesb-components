 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * DefaultMessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class DefaultMessageExchangeHandler extends AbstractMessageExchangeHandler {
	
	/** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(DefaultMessageExchangeHandler.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(DefaultMessageExchangeHandler.class);

    
    /** Creates a new instance of AbstractMessageExchangeHandler */
    public DefaultMessageExchangeHandler() {
        super();
    }
    
    protected void processError(Exception ex) {
        
        LOG.debug("Default MessageExchange Handler processing Error");
        ex.printStackTrace();
        LOG.debug("Default MessageExchange Handler processed Error");
    }
    
    protected void processDone() {
        
        LOG.debug("Default MessageExchange Handler processed DONE");
    }
    
    protected  void processFault(Fault fault) {
        
        LOG.debug("Default MessageExchange Handler processing FAULT");
        LOG.debug(fault.toString());
        LOG.debug("Default MessageExchange Handler processed FAULT");
    }
    protected void processMessage() {
        
        LOG.debug("Default MessageExchange Handler processing Message");
        NormalizedMessage inMsg = this.getMessageExchange().getMessage(IN_MESSAGE);
        NormalizedMessage outMsg = this.getMessageExchange().getMessage(IN_MESSAGE);
        LOG.debug("IN MESSAGE ::::::: \n" + inMsg );
        LOG.debug("OUT MESSAGE ::::::: \n" + outMsg );
        LOG.debug("Default MessageExchange Handler processed Message");
    }
    
    protected void validateMessageExchange() throws MessagingException {
        if ( this.getMessageExchange() == null ) {
        	String msg=MESSAGES.getString("CRB000437_MessageExchange_Object_is_null_in_DefaultMessageExchageHandler");
            LOG.error(msg);
            throw new MessagingException(msg);

        }
    }
    
}
