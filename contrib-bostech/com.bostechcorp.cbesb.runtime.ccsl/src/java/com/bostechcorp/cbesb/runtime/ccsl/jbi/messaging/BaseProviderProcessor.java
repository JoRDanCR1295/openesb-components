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
 * $Id: BaseProviderProcessor.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * The abtract provider processor class for ChainBuilder ESB components. It provides the base functions for
 * provider processor for provider endpoint in JBI component. It use JBI API to implement the JBI Message Exchange 
 * Pattern; The component developer should extend this class and provides implementation for 
 * abstacrt methods processInMessage() and processInOutMessage()
 * 
 * It hides the great complexity of JBI Spec and API from component developers.
 * 
 * 
 * @author elu
 *
 */

abstract public class BaseProviderProcessor implements IComponentProcessor {
	protected final transient Log logger = LogFactory.getLog(getClass());
	
	private BaseEndpoint endpoint;
	
	
	
	/**
	 * @return the endpoint
	 */
	public BaseEndpoint getEndpoint() {
		return endpoint;
	}

	/**
	 * @param endpoint the endpoint to set
	 */
	public void setEndpoint(BaseEndpoint endpoint) {
		this.endpoint = endpoint;
	}

	/**
	 * Nothing to be done for Provider Endpoint.
	 */
	public void start() throws Exception {
		doStart();
    }
	
	/**
	 * Nothing to be done for Provider Endpoint.
	 */
	
    protected void doStart() throws Exception {
    }

    /**
	 * Nothing to be done for Provider Endpoint.
	 */
    
    public void stop() throws Exception {
    	doStop();
        
    }

    /**
	 * Nothing to be done for Provider Endpoint.
	 */
    protected void doStop() throws Exception {
    }
	

    /**
	 * Nothing to be done for Provider Endpoint.
	 */
    public void process(Object message) throws Exception {
    	// nothing to imeplemnt for Provider side processor
    }
    
    
    /**
     * Constructor.
     * @param endpoint
     */
	public BaseProviderProcessor(BaseEndpoint endpoint) {
		super();
		this.endpoint = endpoint;
	}

	/**
	 * Process the RobustInOnly MessageExchange. Refer to Page 32 in the JBI spec.
	 */
	public void processRobustInOnly(MessageExchange exchange)
     throws Exception {
		 
	     try {
	         processInMessage(exchange.getEndpoint().getServiceName(), exchange
	             .getOperation(), exchange.getMessage("in"), exchange);
	
	         exchange.setStatus(ExchangeStatus.DONE);
	     } catch (Exception e) {
	         try {
	             exchange.setFault(endpoint.createFault(e, exchange));
	         } catch (Exception e1) {
	             endpoint.logError(new Exception(
	                 "Error occured while processing, send a Fault.", e1));
	         }
	     }
	}

	/**
	 * Process the InOnly MessageExchange. Refer to Page 31 in the JBI spec.
	 */
     public void processInOnly(MessageExchange exchange) throws Exception {
         try {
             processInMessage(exchange.getEndpoint().getServiceName(), exchange
                 .getOperation(), exchange.getMessage("in"), exchange);
         } catch (Exception e) {
             // nothing, InOnly can not indicate a fault
             endpoint.logError(new Exception(
                 "Error occured while processing, but InOnly message can not contains Fault.",
                 e));
         }
         exchange.setStatus(ExchangeStatus.DONE);
     }

     /**
      * Process the InOut and In OptionalOut MessageExchange Pattern. Refer to Page 32/33 in the JBI spec.
      */
     public void processInOut(MessageExchange exchange, boolean optional)
         throws Exception {
         try {
             if (!endpoint.ackFaultReception(exchange)) {
                 NormalizedMessage out = endpoint.createNormalizedMessage(exchange);

                 boolean outNeeded = processInOutMessage(exchange.getEndpoint()
                     .getServiceName(), exchange.getOperation(), exchange
                     .getMessage("in"), out, optional, exchange);

                 if (outNeeded) {
                	 if (exchange.getMessage("out") == null)
                	 {
                		 exchange.setMessage(out, "out");
                	 }
                 } else {
                     exchange.setStatus(ExchangeStatus.DONE);
                 }
             }
         } catch (Exception e) {
             try {
            	 e.printStackTrace();
                 exchange.setFault(endpoint.createFault(e, exchange));
             } catch (Exception e1) {
                 endpoint.logError(new Exception(
                     "Error occured while processing, send a Fault.", e1));
             }
         }
     }

   

     /**
      * Process the In message (InOnly or RobustInOnly).The child class needs to provide implementation.
      * 
      * 
      * @param service
      * @param operation
      * @param in
      * @throws Exception
      *             if RobustInOnly, a Fault will be generated with the exception
      */
     protected abstract void processInMessage(QName service, QName operation,
         NormalizedMessage in, MessageExchange exchange) throws Exception;

     /**
      * Process the InOut message (InOut or InOptionalOut).The child class needs to provide
      * implementation.
      * 
      * @param service
      * @param operation
      * @param in
      * @param out
      *            the out message on which the response has to be set
      * @param optionalOut
      *            the response is optional or not
      * @return true if the out response has been set (correspond to the
      *         'optionalOut' notion)
      * @throws Exception
      *             a Fault will be generated with the exception, the "out" will
      *             not be treated
      */
     protected abstract boolean processInOutMessage(QName service, QName operation,
         NormalizedMessage in, NormalizedMessage out, boolean optionalOut, MessageExchange exchange)
         throws Exception;

	 
}
