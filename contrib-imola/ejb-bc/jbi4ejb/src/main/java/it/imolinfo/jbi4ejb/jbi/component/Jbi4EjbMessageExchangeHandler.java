/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.component;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractMessageExchangeHandler;
import it.imolinfo.jbi4ejb.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint;
import it.imolinfo.jbi4ejb.processor.ExchangeProcessor;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * Message Exchange handler. Some of this code is taken from the <code>InOutProviderMessageExchangeHandler</code>
 * class.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
public class Jbi4EjbMessageExchangeHandler extends AbstractMessageExchangeHandler {       
    
    /** The Constant SEND_SYNC_TIMEOUT. */
    public static final long SEND_SYNC_TIMEOUT = 60000;

    /** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(Jbi4EjbMessageExchangeHandler.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(Jbi4EjbMessageExchangeHandler.class);
    
    /** The service unit manager. */
    private Jbi4EjbSUManager suManager = null;
    
    /**
     * Instantiates a new jbi4 ejb message exchange handler.
     * 
     * @param suManager
     *          The service unit manager
     */
    public Jbi4EjbMessageExchangeHandler(Jbi4EjbSUManager suManager) {
        super();
        this.suManager = suManager;
    }
        
    /**
     * Process the message.
     */
    protected final void processMessage() {
        
        InOut inOutMX = (InOut) this.getMessageExchange();        
                             
        Jbi4EjbEndpoint ejbEndpoint = 
        	suManager.getStartedEndpoint(inOutMX.getEndpoint());
                
        LOG.info("EJB000102_Received_message_invocation_for_endpoint", 
        		new Object[]{ejbEndpoint.getEndpointName()});
        ExchangeProcessor processor = ejbEndpoint.getExchangeProcessor();
        
        // process the message
        processor.process(inOutMX);                
    }
          
    /**
     * Validates the <code>MessageExchange</code>.
     * 
     * @throws MessagingException
     *          If something go wrong.
     */
    protected void validateMessageExchange() throws MessagingException {
        
        MessageExchange msgExchange = this.getMessageExchange();
        
        if ( this.getMessageExchange() == null ) {
        	String msg=
        		MESSAGES.getString("EJB000103_MessageExchange_Object_null_in_MessageExchageHandler");
            LOG.error(msg);
            throw new MessagingException(msg);   
        }
        
        if ( MessageExchange.Role.CONSUMER.equals(msgExchange.getRole()) ) {
        	String msg=
        		MESSAGES.getString("EJB000104_Provider_Message_Exchange_Handler_can_not_have_MessageExchange_with_ONSUMER_Role");
            LOG.error(msg);
            throw new MessagingException(msg); 
        }        
        if (!(msgExchange instanceof InOut) ) {
        	String msg=
        		MESSAGES.getString("EJB000105_InOut_Message_Exchange_Handler_MessageExchange_object_should_be_instance_of_javax.jbi.messaging.InOut");
            LOG.error(msg);
            throw new MessagingException(msg);
        }
    }
    
    /**
     * Process a <code>Fault</code>.
     * 
     * @param fault
     *          The fault
     */
    protected void processFault(Fault fault) {
    	LOG.error("EJB000106_InOut_Message_Exchange_Provider_received_FAULT");
    }
    
    /**
     * Process Done.
     */
    protected void processDone() {
        RuntimeHelper.logVerbose("InOut Message Exchange Provider handler received DONE : END of service invocation");
    }
            
    /**
     * Process an Error.
     * 
     * @param ex
     *          The eception to process
     */
    protected void processError(Exception ex) {
    	LOG.error("EJB000107_InOut_Message_Exchange_Provider_received_error");
    	LOG.error("EJB000108_Error_during_Process", ex);
    }
    
}
