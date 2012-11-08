 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.processor;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import it.imolinfo.jbi4corba.jbi.endpoint.ConsumerEndpoint;

import javax.jbi.messaging.MessageExchange;



/**
 * ConsumerExchangeProcessor.
 * 
 * @author raffaele 
 */
public class ConsumerExchangeProcessor implements ExchangeProcessor {

  /**
   * Logger.
   */
  private static final transient Logger LOG
    = LoggerFactory.getLogger(ConsumerExchangeProcessor.class);
  private static final Messages MESSAGES = 
  	Messages.getMessages(ConsumerExchangeProcessor.class);


/** 
 * The consumer endpoint.
 */
ConsumerEndpoint consumerEndpoint;
  
 /**
  * 
  * @param consumerEndpoint  The consumer endpoint
  */
  public ConsumerExchangeProcessor(ConsumerEndpoint consumerEndpoint) {
    this.consumerEndpoint = consumerEndpoint;
  }

  // private Map locksMap=Collections.synchronizedMap(new HashMap<K, V>)
 /**
  * @param messageexchange  The message exchange
  * @throws Exception       The exception
  */
  public void process(MessageExchange messageExchange) {
      
    LOG.debug("process - MessageExchange=" + messageExchange);
    
    try {
        if (messageExchange != null) {
            consumerEndpoint.getConsumerServiceDescriptor()
                .getConsumerInvocationHandler().process(messageExchange);
        } else {
            LOG.debug("process - No Operations to do.");
        }
    } catch (Exception ex)  {
    	String msg=MESSAGES.getString("CRB000800_Error_in_message_exchange", 
    			new Object[] {ex.getMessage()});
        LOG.error(msg);
        // No exception is thrown...       
    }
  }

}
