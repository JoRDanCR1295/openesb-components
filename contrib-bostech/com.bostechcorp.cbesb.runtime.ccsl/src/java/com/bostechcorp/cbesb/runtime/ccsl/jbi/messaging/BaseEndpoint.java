/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * $Id: BaseEndpoint.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging;

import java.net.URI;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.RobustInOnly;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.servicemix.common.Endpoint;
import org.apache.servicemix.common.ExchangeProcessor;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.SourceHelper;


/**
 * The abtract Endpoint class for ChainBuilder ESB components. It provides the base functions for
 * dealing with both consumer and provider endpoints in JBI component.  The component developer 
 * should extend this class and overide the methods only if it is necessary to change basic behaviors.
 * 
 * It hides the great complexity of JBI Spec and API from component developers.
 * 
 * 
 * @author elu
 *
 */
public abstract class BaseEndpoint extends Endpoint implements ExchangeProcessor {
	protected final transient Log logger = LogFactory.getLog(getClass());
	
    protected ServiceEndpoint activated;
    protected DeliveryChannel channel;
    protected MessageExchangeFactory exchangeFactory;
    protected Role role;
    protected URI defaultMep;
    protected QName defaultOperation;
  

    protected IComponentProcessor providerProc;
    protected IComponentProcessor consumerProc;

    
    /* (non-Javadoc)
     * @see org.apache.servicemix.common.Endpoint#getRole()
     */
    public Role getRole() {
        return role;
    }
    public void setRole(Role role) {
    	this.role = role;
    }
    
    public URI getDefaultMep() {
    	return defaultMep;
    }
    public void setDefaultMep(URI defaultMep) {
    	this.defaultMep = defaultMep;
    }
    
    public QName getDefaultOperation() {
    	return defaultOperation;
    }
    public void setDefaultOperation(QName defaultOperation) {
    	this.defaultOperation = defaultOperation;
    }
    

    
    /* (non-Javadoc)
     * @see org.servicemix.common.Endpoint#activate()
     */
    public void activate() throws Exception {
    	start();
        ComponentContext ctx = this.serviceUnit.getComponent().getComponentContext();
        channel = ctx.getDeliveryChannel();
        exchangeFactory = channel.createExchangeFactory();

        if (getRole() == Role.PROVIDER) {
            activated = ctx.activateEndpoint(service, endpoint);
            providerProc = createProviderProcessor();

            
            providerProc.start();
        } else {

        	activated = ctx.activateEndpoint(service, endpoint);
            consumerProc = createConsumerProcessor();         
            consumerProc.start();
            
            // The consumer endpoint also needs an provider processor for accept the MessageExchange in DONE or ERROR status
            // It is not meaned to handle the ACTIVE MessageExchange
            providerProc = createProviderProcessor();
            providerProc.start();
            
        }

    }

    /* (non-Javadoc)
     * @see org.servicemix.common.Endpoint#deactivate()
     */
    public void deactivate() throws Exception {
        ComponentContext ctx = this.serviceUnit.getComponent().getComponentContext();
        if (getRole() == Role.PROVIDER) {
            ServiceEndpoint ep = activated;
            activated = null;
            ctx.deactivateEndpoint(ep);
            providerProc.stop();
        } else {
            ServiceEndpoint ep = activated;
            activated = null;
            ctx.deactivateEndpoint(ep);
//          ctx.deregisterExternalEndpoint(ep);
            consumerProc.stop();
        }
//        processor.stop();
    	stop();
    }

    
    protected abstract IComponentProcessor createProviderProcessor();
    
    protected abstract IComponentProcessor createConsumerProcessor();
    
    protected  ServiceEndpoint createExternalEndpoint() {
    	return null;
    }
    
    public ExchangeProcessor getProcessor() {
        return this;
    }
    
    public void validate() throws DeploymentException {
    	// TODO: validation to be done
    }
         
    abstract public void start() throws Exception ;
    
    public void stop() {
    }

    


    /**
     * The process() method is called when an MessageExchange is accepted from NMR
     */
    public void process(MessageExchange exchange) throws Exception {
    	
        if (ExchangeStatus.ACTIVE.equals(exchange.getStatus())) {
        	
        	if (exchange.getRole() == MessageExchange.Role.PROVIDER) {
        		processAsProvider(exchange);
            }
        	else if (exchange.getRole() == MessageExchange.Role.CONSUMER) {
        		throw  new JBIException("Receive an unexpected MessageExchange in Consumer endpoint:" + exchange.toString());
        		
//        		processAsConsumer(exchange);
        	}
        }
        else {
        	// not an ACTIVE status
        	logger.debug("Ignore an MessageExchange in non-Active Status: " + exchange.getStatus());
        }

    }

//    public void processAsConsumer(MessageExchange exchange) throws Exception {
//    	
//    	if (exchange instanceof InOnly  ) {
//            consumerProc.processInOnly(exchange);
//    	}  else if (exchange instanceof RobustInOnly  ) {
//    		consumerProc.processRobustInOnly(exchange);
//    	}  else if (exchange instanceof InOut  ) {      
//    		consumerProc.processInOut(exchange, false);
//        } else if (exchange instanceof InOptionalOut  ) {      
//        	consumerProc.processInOut(exchange, true);
//    	}else {
//            Exception e = new Exception(
//                "MessageExchangePattern not recognized :"
//                    + exchange.getPattern());
//            exchange.setError(e);
//            channel.send(exchange);
//            throw e;
//        }
//        channel.send(exchange);
//    }
//    
    
    
    protected void processAsProvider(MessageExchange exchange) throws Exception {
    	
    	if (exchange instanceof InOnly  ) {
            providerProc.processInOnly(exchange);
    	}  else if (exchange instanceof RobustInOnly  ) {
    		providerProc.processRobustInOnly(exchange);
    	}  else if (exchange instanceof InOut  ) {      
    		providerProc.processInOut(exchange, false);
        } else if (exchange instanceof InOptionalOut  ) {      
        	providerProc.processInOut(exchange, true);
    	}else {
            Exception e = new Exception(
                "MessageExchangePattern not recognized :"
                    + exchange.getPattern());
            exchange.setError(e);
            channel.send(exchange);
            throw e;
        }
    	
        channel.send(exchange);
        
    }
    
    /**
     * If the received exchange is a fault response from the consumer,
     * acknowledge it : status DONE
     * 
     * @param ex
     * @return true if the ack. of the Fault is done; false if no fault is
     *         present in the exchange
     * @throws Exception
     */
    protected boolean ackFaultReception(MessageExchange ex) throws Exception {
        boolean result = false;

        if (ex.getFault() != null) {
            ex.setStatus(ExchangeStatus.DONE);

            result = true;
        }
        return result;
    }

    /**
     * Creates a Fault from an exception
     * 
     * @param e
     * @return
     */
    protected Fault createFault(Exception e, MessageExchange exchange)
        throws MessagingException {
        Fault f = exchange.createFault();
        String faultString = SourceHelper.createSoapFault(e, null);
        Source content = SourceHelper.createSource(faultString);
        f.setContent(content);
        return f;
    }

    /**
     * Creates a NormalizedMessage for Out
     * 
     * @return
     */
    protected NormalizedMessage createNormalizedMessage(MessageExchange exchange)
        throws MessagingException {
        return exchange.createMessage();
    }

    protected void logError(Throwable e) {
        if (logger != null)
            logger.error( e.getMessage(), e);
        else
            e.printStackTrace();
    }


}
