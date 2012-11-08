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
 * InOutProviderMessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;import it.imolinfo.jbi4ejb.LoggerFactory;import it.imolinfo.jbi4ejb.jbi.Messages;import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

/**
 * This class implements the InOut Message exchange handler implemenation for the
 * Service Provider.
 *
 * It does a synchronous message exchange processing where it invokes the service
 * and wait until the service processes the message and returns the response message.
 * It then return out or fault message to consumer and also waits until the consumer
 * returns the DONE status.
 *
 * Extended class should implement the actual invocation of the service operation by
 * implementing the abstract method invokeInOutOperation of this class.
 *
 * @author Sun Microsystems, Inc.
 */
public abstract class InOutProviderMessageExchangeHandler extends AbstractMessageExchangeHandler {
    
    public static final long SEND_SYNC_TIMEOUT = 60000;       /** The logger. */    private static final Logger LOG = LoggerFactory.getLogger(InOutProviderMessageExchangeHandler.class);        private static final Messages MESSAGES = Messages.getMessages(InOutProviderMessageExchangeHandler.class);   
    
    /** Creates a new instance of InOutProviderMessageExchangeHandler */
    public InOutProviderMessageExchangeHandler() {
        super();
    }
    
    protected void validateMessageExchange() throws MessagingException {
        
        MessageExchange msgExchange = this.getMessageExchange();
        
        if ( this.getMessageExchange() == null ) {
        	String msg=MESSAGES.getString("EJB000215_MessageExchange_Object_null_in_DefaultMessageExchageHandler");            LOG.error(msg);            throw new MessagingException(msg);  
        }
        
        if ( MessageExchange.Role.CONSUMER.equals(msgExchange.getRole()) ) {        	String msg=MESSAGES.getString("EJB000217_MessageExchange_Handler_can_not_have_MessageExchange_with_CONSUMER_Role");            LOG.error(msg);            throw new MessagingException(msg);  
        }
        
        if (!(msgExchange instanceof InOut) ) {        	String msg=MESSAGES.getString("EJB000218_InOut_Message_Exchange_Handler_MessageExchange_object_should_be_instance");            LOG.error(msg);            throw new MessagingException(msg);
        }
    }
    
    protected void processFault(Fault fault) {    	LOG.error("EJB000219_processFault");
    }
    
    protected void processDone() {
        RuntimeHelper.logVerbose("InOut Message Exchange Provider handler received DONE : END of service invocation");
    }
    
    protected void processError(Exception ex) {    	LOG.error("EJB000220_InOut_Message_Exchange_Provider_handler_received_Error");    	LOG.error("EJB000221_processError", new Object[] {ex});
    }
    
    protected final void processMessage() {
        
        InOut inOutMX = (InOut) this.getMessageExchange();
        
        NormalizedMessage inMsg = inOutMX.getInMessage();
        if ( inMsg == null ) {
            this.sendError(new MessagingException("InOut Provider MessageExchange received null In Message"));
            return;
        }
        
        handleInMessage(inMsg);
    }
    
    private void handleInMessage(NormalizedMessage inMsg) {
        
        Fault fault = null;
        Exception processingEx = null;
        NormalizedMessage outMsg = null;
        boolean result = false;
        
        InOut inOutExchange = (InOut) this.getMessageExchange();
        try {
            // create out and fault messages
            outMsg = inOutExchange.createMessage();
            fault = inOutExchange.createFault();
            // invoke operation
            result = invokeOperation(inMsg, outMsg, fault);
        } catch (Exception ex) {
            processingEx = ex;
            ex.printStackTrace();
        } finally {
            try {
                if ( result == true ) {
                    // operation success
                    this.sendOutMessage(outMsg);
                } else {
                    // operation failed or some exception happened.
                    if ( processingEx != null ) {
                        // set fault content with exception msg
                        this.sendFault(processingEx);
                    } else {
                        // fault content has been set by the operation.
                        this.sendFault(fault);
                    }
                }
            } catch (MessagingException ex) {
                ex.printStackTrace();
                this.sendError(ex);
                return;
            }
        }
        // process the message exchange again after sending the fault or out message to
        // process the done or error received.
        this.processMessageExchange();
    }
    
    public void sendFault(Fault fault) throws MessagingException {
        InOut inOutMX = (InOut) this.getMessageExchange();
        inOutMX.setFault(fault);
        boolean sent = false;
        try {
            sent = this.getDeliveryChannel().sendSync(inOutMX, SEND_SYNC_TIMEOUT);
            if (!sent) {
                inOutMX.setError(new MessagingException("InOutProvider ME Handler unable to send out message"));
            }
        } catch (Exception ex) {        	        	LOG.error("EJB000222_sendFault");
            inOutMX.setError(ex);
        }
    }
    
    private void sendOutMessage(NormalizedMessage outMsg) throws MessagingException {
        
        InOut inOutMX = (InOut) this.getMessageExchange();
        inOutMX.setOutMessage(outMsg);
        boolean sent = false;
        try {
            sent = this.getDeliveryChannel().sendSync(inOutMX, SEND_SYNC_TIMEOUT);
            if (!sent) {
                inOutMX.setError(new MessagingException("InOutProvider ME Handler unable to send out message"));
            }
        } catch (Exception ex) {
        	LOG.error("EJB000223_sendOutMessage");        	inOutMX.setError(ex);
        }
    }
    
    protected abstract boolean invokeOperation(NormalizedMessage inMsg, NormalizedMessage outMsg, Fault fault) throws MessagingException;
    
}
