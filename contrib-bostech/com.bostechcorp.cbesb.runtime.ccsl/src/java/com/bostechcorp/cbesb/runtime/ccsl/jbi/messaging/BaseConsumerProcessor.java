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
 * $Id: BaseConsumerProcessor.java,v 1.3 2007/04/25 20:46:00 afung Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging;

import javax.xml.namespace.QName;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.RobustInOnly;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.lib.ExternalInput;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.SourceHelper;
import com.bostechcorp.cbesb.runtime.component.util.wsdl.WsdlMepConstants;


/**
 * The abtract consumer processor class for ChainBuilder ESB components. It provides the base functions for
 * consumer processor for consumer endpoint in JBI component. It use JBI API to implement the JBI Message Exchange 
 * Pattern; The component developer should extend this class and make call to the process() method 
 * and provides implementation for abstacrt methods.
 * 
 * It hides the great complexity of JBI Spec and API from component developers.
 * 
 * 
 * @author elu
 *
 */
abstract public class BaseConsumerProcessor implements IComponentProcessor {
	protected final transient Log logger = LogFactory.getLog(getClass());
	private static final String SENDER_ENDPOINT_PROPERTY = "org.apache.servicemix.senderEndpoint";

	private BaseEndpoint endpoint;
	private DeliveryChannel channel;
	
	/**
	 * Default method is to start a Work. it is needed for BindComponent required multi-threading
	 * processing (e.g, File, FTP, TCP)
	 */
	public void start() throws Exception {
	
		WorkManager wm = endpoint.getServiceUnit().getComponent().getWorkManager();
			wm.startWork( new Work() {
				public void release() {}
				public void run() {
					try {
						doStart();
					} catch (Exception e) {
					logger.error("Exception caught in start :" + e.getLocalizedMessage());
					e.printStackTrace();
				}
			}
		});
	}
	
	
	protected void doStart() throws Exception {
	}
	
	public void stop() throws Exception {
		doStop();
	    
	}
	
	protected void doStop() throws Exception {
	}
	
	public BaseConsumerProcessor(BaseEndpoint endpoint) {
		super();
		this.endpoint = endpoint;
		this.channel = endpoint.channel;
	}

	public void processRobustInOnly(MessageExchange exchange)
     throws Exception {
		throw  new JBIException("Receive an unexpected MessageExchange in Consumer processor :" + exchange.toString());

	}

     public void processInOnly(MessageExchange exchange) throws Exception {
 		throw  new JBIException("Receive an unexpected MessageExchange in Consumer processor :" + exchange.toString());
     }

     public void processInOut(MessageExchange exchange, boolean optional)
         throws Exception {
 		throw  new JBIException("Receive an unexpected MessageExchange in Consumer processor :" + exchange.toString());
     }

   

     
     /**
      * The process() method is called from the child consumer processor when it read data from external connection and 
      * turn it into an MessageExchange and route to NMR. 
      *  
      */
     public void process (Object data) throws Exception{
    	 
    	 logger.debug("Endpoint's DefaultMEP :" + endpoint.defaultMep);
    	 if (endpoint.defaultMep.equals(WsdlMepConstants.IN_ONLY)) {
    		 handleInOnly(data);
    	 } else if (endpoint.defaultMep.equals(WsdlMepConstants.IN_OUT)) {
    		 handleInOut(data);
    	 } else if (endpoint.defaultMep.equals(WsdlMepConstants.ROBUST_IN_ONLY)) {
    		 handleRobustInOnly(data);
    	 } else 
    		 throw new Exception("trying to process unknown MEP \""+endpoint.defaultMep+"\"");
     }
     
     /**
      * 
      * The transforme() method is to transform the data read from connection into NormalizedMessage in the MessageExchange.
      * This implementation expects "data" to be an instance of ExternalInput. Subclasses can override this if they have
      * other requirements. Subclasses can also override this if they need to copy metadata values into the exchange. Most
      * classes should still be able to use super.transform() to do the actual data copy. 
      * 
      * @param data
      * @param me
      * @throws Exception
      */
      protected void transform (Object data, MessageExchange me) throws Exception {
         ExternalInput ext = (ExternalInput) data;
         NormalizedMessage msg = me.createMessage();
         ext.populateMessage(msg, null);
         me.setMessage(msg, "in");
      }

     
     
     /**
      * Get fault message and do something with it;
      * The child class need to overwrite this one to provide more action such as write
      * into a File, put into databasee and etc al.
      * 
      * @param me
      * @param data
      */
     protected void handleFault(MessageExchange me, Object data) throws Exception{

         NormalizedMessage nm = me.getFault();

         if (nm != null) {
        	 doProcessFault(nm);
             me.setStatus(ExchangeStatus.DONE);
             channel.send(me);
         }
         
     }
     
     
     protected void doProcessFault(NormalizedMessage nm) throws Exception {
    	 String fault = null;
    	 if (nm != null) {
             fault = SourceHelper.createString(nm.getContent());
    	 }
    	 doProcessFault(nm, fault);
     }
     
     abstract protected void doProcessFault(NormalizedMessage nm, String fault) throws Exception;
     
     abstract protected void doProcessOut(NormalizedMessage nm, String s, MessageExchange me) throws Exception;
     
     protected void doProcessOut(NormalizedMessage nm, MessageExchange me) throws Exception {
    	 String response = null;
         if (nm != null) {
             response = SourceHelper.createString(nm.getContent());
         }
         doProcessOut(nm, response, me);
     }
     
     protected boolean handleInOnly(Object data) throws Exception{
    	 boolean sended = false;
         try {
             InOnly me = null;
             ComponentContext context = endpoint.getServiceUnit().getComponent().getComponentContext();
             me = channel.createExchangeFactory()
                     .createInOnlyExchange();
             String endpointKey = "{"+endpoint.getService().getNamespaceURI()+"}"+endpoint.getService().getLocalPart()+":"+endpoint.getEndpoint();
             me.setProperty(SENDER_ENDPOINT_PROPERTY, endpointKey); 
             transform (data, me);
             
             logger.debug("Consumer endpoint service="+endpoint.getService() + "  endpoint="+endpoint.getEndpoint());
             ServiceEndpoint linkedEndpoint = context.getEndpoint(endpoint.getService(), endpoint.getEndpoint());
             logger.debug("Got target endpoint "+linkedEndpoint+"   service="+linkedEndpoint.getServiceName()+"   endpoint="+linkedEndpoint.getEndpointName());


		     me.setEndpoint(linkedEndpoint);
		     me.setService(endpoint.getService());
		     
             channel.sendSync(me);
             if (ExchangeStatus.DONE.equals(me.getStatus()))
             {
                 sended = true;
             }
             return sended;
         } catch (Exception ex) {
        	 ex.printStackTrace();
             throw new JBIException(ex.getMessage());
         }
     }
     
	 protected void handleInOut(Object data) throws Exception{
		  try {

	            InOut me = null;
//	            me = channel.createExchangeFactory().createInOutExchange();

// This was missing from in-out causing a null destination	    	    
	             ComponentContext context = endpoint.getServiceUnit().getComponent().getComponentContext();
	             me = channel.createExchangeFactory()
	                     .createInOutExchange();
	             String endpointKey = "{"+endpoint.getService().getNamespaceURI()+"}"+endpoint.getService().getLocalPart()+":"+endpoint.getEndpoint();
	             me.setProperty(SENDER_ENDPOINT_PROPERTY, endpointKey);
		     
		     //me.setOperation(endpoint.getDefaultOperation());
		     // HACK to test stuff.
		     me.setOperation(new QName("http://j2ee.netbeans.org/wsdl/server", "serverOperation"));
	             logger.debug("Consumer endpoint service="+endpoint.getService() + "  endpoint="+endpoint.getEndpoint());
	             ServiceEndpoint linkedEndpoint = context.getEndpoint(endpoint.getService(), endpoint.getEndpoint());
	             logger.debug("Got target endpoint "+linkedEndpoint+"   service="+linkedEndpoint.getServiceName()+"   endpoint="+linkedEndpoint.getEndpointName());


			     me.setEndpoint(linkedEndpoint);
			     me.setService(endpoint.getService());
			     


	            transform (data, me);
	            channel.sendSync(me);

	            // Maybe it had an error in called service
	            if ((ExchangeStatus.ERROR).equals(me.getStatus())) {
	                handleFault(me, data);
	            } else {
	                NormalizedMessage nm = me.getMessage("out");
	                if (nm != null) doProcessOut(nm, me);
	                else throw new MessagingException("No out message returned for in-out exchange");
	            }
	            
	            me.setStatus(ExchangeStatus.DONE);
	            channel.send(me);
	        } catch (Exception ex) {
	        	ex.printStackTrace();
	            throw new JBIException(ex.getMessage());
	        }
     }
 
	 
	 protected boolean handleRobustInOnly(Object objMsg) throws Exception{
		boolean sended = false;
        try {
            RobustInOnly msg = null;
            msg = channel.createExchangeFactory()
                    .createRobustInOnlyExchange();

            transform(objMsg, msg);
            channel.sendSync(msg);

            ExchangeStatus status = msg.getStatus();

            if (status.equals(ExchangeStatus.ERROR)) {
                // Notify error to external service
                handleFault(msg, objMsg);
                sended = true;
            } else if (status.equals(ExchangeStatus.DONE)) {
                sended = true;
            }
            return sended;
        } catch (Exception ex) {
        	ex.printStackTrace();
            throw new JBIException(ex.getMessage());
        }
		
     }
}
