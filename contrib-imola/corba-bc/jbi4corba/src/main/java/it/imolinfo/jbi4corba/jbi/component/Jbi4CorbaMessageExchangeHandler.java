 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.AbstractMessageExchangeHandler;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;
import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaSFServiceEndpoint;
import it.imolinfo.jbi4corba.jbi.processor.ExchangeProcessor;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;


/**
 * Message Exchange handler. Some of this code is taken from the <code>InOutProviderMessageExchangeHandler</code>
 * class.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
public class Jbi4CorbaMessageExchangeHandler extends AbstractMessageExchangeHandler {       
    
    /** The Constant SEND_SYNC_TIMEOUT. */
    public static final long SEND_SYNC_TIMEOUT = 60000;

    /** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger(Jbi4CorbaMessageExchangeHandler.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(Jbi4CorbaMessageExchangeHandler.class);
    
    /** The service unit manager. */
    private Jbi4CorbaSUManager suManager = null;
    
    /**
     * Instantiates a new jbi4 ejb message exchange handler.
     * 
     * @param suManager
     *          The service unit manager
     */
    public Jbi4CorbaMessageExchangeHandler(Jbi4CorbaSUManager suManager) {
        super();
        this.suManager = suManager;
    }
        
    /**
     * Process the message.
     */
    protected final void processMessage() {
        
        MessageExchange me = this.getMessageExchange();
                             
        //me.setProperty("EPR_IOR", "");
        ServiceEndpoint sep =  me.getEndpoint();
        
        Jbi4CorbaSFServiceEndpoint ep;
        
        if(sep instanceof Jbi4CorbaSFServiceEndpoint){
            ep= (Jbi4CorbaSFServiceEndpoint) sep;
            me.setProperty("EPR_IOR", ep.getIor());
            String msg=MESSAGES.getString("CRB000230_Received_message_invocation_for_endpoint");
            LOG.debug(msg);
        }
           
        Jbi4CorbaEndpoint corbaEndpoint = suManager.getStartedEndpoint(me.getEndpoint());
        
       
        
        java.util.logging.Logger.getLogger("com.sun.EnterContext").fine("JBI4Corba-" + corbaEndpoint.getSuName());
        
        LOG.debug("CRB000208_Received_message_invocation_for_endpoint", 
        		new Object[]{corbaEndpoint.getEndpointName()});
        
        ExchangeProcessor processor = corbaEndpoint.getExchangeProcessor();
        
        // process the message        
        processor.process(me);                
        java.util.logging.Logger.getLogger("com.sun.ExitContext").fine("JBI4Corba-"+corbaEndpoint.getSuName());
    }
          
    /**
     * Validates the <code>MessageExchange</code>.
     * 
     * @throws MessagingException
     *          If something go wrong.
     */
    protected void validateMessageExchange() throws MessagingException {
        
        MessageExchange msgExchange = this.getMessageExchange();
        LOG.debug("begin validation of exchange: "+msgExchange);


        if ( this.getMessageExchange() == null ) {
        	String msg=MESSAGES.getString("CRB000209_MessageExchange_Object_is_null_in_MessageExchageHandler");
            LOG.error(msg);
            throw new MessagingException(msg);

        }
        
        if ( MessageExchange.Role.CONSUMER.equals(msgExchange.getRole()) && !(msgExchange.getStatus()==ExchangeStatus.DONE) ) {
        	String msg=MESSAGES.getString("CRB000210_Message_Exchange_Handler_can_not_have_MessageExchange_with_CONSUMER_Role");
            LOG.error(msg);
            throw new MessagingException(msg);
        }        
        
        if ((!(msgExchange instanceof InOut)) && (!(msgExchange instanceof InOnly) )) {
        	String msg=MESSAGES.getString("CRB000211_MessageExchange_shoul_be_instance_of_javax.jbi.messaging.InOut_or_of_javax.jbi.messaging.InOnly");
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
	Jbi4CorbaEndpoint corbaEndpoint = suManager.getStartedEndpoint(this.getMessageExchange().getEndpoint());
        corbaEndpoint.getEndpointStatus().incrementReceivedErrors();
        String msg=MESSAGES.getString("CRB000212_InOut_Message_exchange_provider_received_FAULT");
        LOG.error(msg);
    }
    
    /**
     * Process Done.
     */
    protected void processDone() {
        Jbi4CorbaEndpoint corbaEndpoint = suManager.getStartedEndpoint(this.getMessageExchange().getEndpoint());
        corbaEndpoint.getEndpointStatus().incrementReceivedDones();    
        LOG.debug("InOut Message Exchange Provider handler received DONE : END of service invocation");        
    }
            
    /**
     * Process an Error.
     * 
     * @param ex
     *          The eception to process
     */
    protected void processError(Exception ex) {
    	String msg=MESSAGES.getString("CRB000213_InOut_Message_Exchange_Provider_handler_received_Error");
        LOG.error(msg,ex);
		RuntimeHelper.logError(msg + ex);

    }
    
}
