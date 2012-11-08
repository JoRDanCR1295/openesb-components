/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jms.BytesMessage;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.TextMessage;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

import net.java.hulp.measure.Probe;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.NMPropertiesUtil.NMProperties;
import com.sun.jbi.jmsbc.NMPropertiesUtil.NMPropertiesParsingException;
import com.sun.jbi.jmsbc.NMPropertiesUtil.OutBoundInOnlyNMProperties;
import com.sun.jbi.jmsbc.NMPropertiesUtil.OutBoundInOutNMProperties;
import com.sun.jbi.jmsbc.NMPropertiesUtil.SynchronousReadNMProperties;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.jms.ChannelException;
import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.jms.SendChannel;
import com.sun.jbi.jmsbc.jms.SendChannelJCAImpl;
import com.sun.jbi.jmsbc.jms.Util;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.util.JMSBCContext;
import com.sun.jbi.jmsbc.util.NamesUtil;
import com.sun.jbi.jmsbc.util.ShutdownSynchronization;
import com.sun.jbi.jmsbc.util.SynchronizedStateChange;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

/**
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor implements Runnable {
    
    public static final String MESSAGEID = "com.sun.jbi.messaging.messageid";
    public static final String GROUPID = "com.sun.jbi.messaging.groupid";
    public static final String MEXID = "com.sun.jbi.messaging.mexid";
    
   
	private static final String CLIENT = "Client";
	private static final String SERVER = "Server";
    private static final Messages mMessages =
        Messages.getMessages(OutboundMessageProcessor.class);
    private static final Logger mLog =
        Messages.getLogger(OutboundMessageProcessor.class);

    private Collection mServiceUnits;
    private ChannelManager mJMSChannelMgr;
    private Map mInboundExchanges;
    private JMSDenormalizer mJMSDenormalizer;
    private JMSNormalizer mJMSNormalizer;
    private ComponentContext mComponentContext;
    private MessageExchange mMessageExchange;
    
    public OutboundMessageProcessor(ComponentContext componentContext,
                                    Collection serviceUnits,
                                    ChannelManager jmsChannelMgr,
                                    Map inboundMessageExchanges) throws Exception {
        mComponentContext = componentContext;
        mServiceUnits = serviceUnits;
        mJMSChannelMgr = jmsChannelMgr;
        mInboundExchanges = inboundMessageExchanges;
        mJMSDenormalizer = new JMSDenormalizer();
        mJMSNormalizer = new JMSNormalizer();
    }

//    protected OutboundMessageProcessor(ComponentContext componentContext,
//                                       Collection serviceUnits,
//                                       ChannelManager jmsChannelMgr,
//                                       Map outboundMessageExchanges,
//                                       Map inboundMessageExchanges,
//                                       JMSDenormalizer jmsDenormalizer,
//                                       JMSNormalizer jmsNormalizer) {
//        mComponentContext = componentContext;
//        mServiceUnits = serviceUnits;
//        mJMSChannelMgr = jmsChannelMgr;
//        mInboundExchanges = inboundMessageExchanges;
//        mMonitor = new Object();
//        mJMSDenormalizer = jmsDenormalizer;
//        mJMSNormalizer = jmsNormalizer;
//    }
    
    public void setMessageExchange(MessageExchange messageExchange){
    	this.mMessageExchange = messageExchange;
    }
    /**
     * Main entry point to execute this in a thread.
     * Calls accept on the JBI NMR and process the MessageExchange it receives.
     * Delegates the real work of processing the MessageExchange to <code>execute</code>
     */
    public void run() {
    	if(mMessageExchange == null)
    		return;
    	
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG, "OutboundMessageProcessor_SERVICE_LOOP_ENTER");
        }

        try {                
            if (mMessageExchange != null) {
                
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG, 
                            "OutboundMessageProcessor_NMR_ACCEPT_MXCH",
                            new Object[]{mMessageExchange.getExchangeId(), mMessageExchange.getPattern().toString()});
                }
                                    
                // Determine if the message is a request or a reply
                boolean isInboundReply = false;
                String exchangeId = getExchangeId(mMessageExchange);
                if (mInboundExchanges.containsKey(exchangeId) &&
                    !isRequest(mMessageExchange)) {
                    // processing inbound reply
                    isInboundReply = true;
                } 
                
                execute(mMessageExchange, isInboundReply);
            }
        } catch (Throwable ex) {
            mLog.log(Level.SEVERE, 
                     mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
                         new Object[]{ex.getLocalizedMessage()}),
                     ex);
             AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
                         new Object[]{ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0743");
            
        }finally{
        	mMessageExchange = null; //mark done
        }
        
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG, "OutboundMessageProcessor_SERVICE_LOOP_EXIT");
        }
    }
    
    /**
     * Process the message exchange
     */
    public void execute(MessageExchange msgExchange,
                        boolean isInboundReply) {
        
        String exchangeId = getExchangeId(msgExchange);
        InboundReplyContext inbReplyContext = null;
        if (isInboundReply) {
            inbReplyContext = (InboundReplyContext) mInboundExchanges.get(exchangeId);
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                long difference = System.currentTimeMillis() - inbReplyContext.getRequestInvocationTime();
                mLog.log(LogSupport.LEVEL_DEBUG, 
                        "OutboundMessageProcessor_MXCH_RESPONSE_RECVD",
                        new Object[]{exchangeId, msgExchange.getPattern(), new Long(difference)});
            }
        }

        // Determine which endpoint type; if the reply is for a pending
        // inbound exchange, then get the consuming (inbound) endpoint.
        // Otherwise it a request to an endpoint the jms bc had provisioned so
        // get the provider (outbound) endpoint.
        // Note: directionality is needed to support bidrectional symmetry of endpoint
        int endpointType = (isInboundReply ? 
                            Endpoint.EndpointType.INBOUND :
                            Endpoint.EndpointType.OUTBOUND);
        
        // Locate the Endpoint of interest from the masses of SUs
        Endpoint endpoint = null;
        QName serviceName = msgExchange.getEndpoint().getServiceName();
        String endpointName = msgExchange.getEndpoint().getEndpointName();
        QName qualOperation = msgExchange.getOperation();
        endpoint = getEndpoint(endpointType, serviceName, endpointName);

        if (endpoint == null ){

        	boolean error = true;
        	if(isInboundReply){
            	//Check if it was a redelivery
            	RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(msgExchange);
            	ServiceEndpoint actualEndpoint = Redelivery.getEndpoint(msgExchange);
            	    if(actualEndpoint != null){
            	    	endpointName = actualEndpoint.getEndpointName();
            	    	serviceName = actualEndpoint.getServiceName();
            	        endpoint = getEndpoint(endpointType, serviceName, endpointName);
            	        if(endpoint != null){
            	        	error = false; //Got the actual end point
            	        	//update with actual JMSOperation
            	            qualOperation = endpoint
									.createOperationAddress(inbReplyContext
											.getJMSOperation()
											.getBindingOperation().getName());
            	        }
            	    }
            	
        	}

        	if(error){
                String errMsg = mMessages.getString("JMSBC-E0744.UnableToLocateServiceEndpoint",
                        new Object[]{msgExchange.getRole(),
                                     msgExchange.getEndpoint().toString(),
                                     msgExchange.getService(),
                                     msgExchange.getExchangeId()});

                MessageExchangeProcessingException mpex = new MessageExchangeProcessingException (errMsg);
                
                handleMessagePreProcessingErrors (isInboundReply,
                                                  inbReplyContext,
                                                  msgExchange,
                                                  mpex);
                
                return;
        	}
        }
        
            // Find the JMS "channel" for the service endpoint operation in question
            Channel jmsChannel = null;      
            try {
                // Look for the JMS Channel from Channel Manager
                ServiceEndpoint serviceEndpoint = msgExchange.getEndpoint();
                // Qualify operation name with service endpoint namespace if it is not qualified
                if (qualOperation.getNamespaceURI()==null||qualOperation.getNamespaceURI().length()==0) {
                    qualOperation = endpoint.createOperationAddress(qualOperation.getLocalPart());
                }
                jmsChannel = mJMSChannelMgr.lookup(endpoint, qualOperation);
            } catch (Exception ex) {
                handleMessagePreProcessingErrors (isInboundReply,
                                                  inbReplyContext,
                                                  msgExchange,
                                                  ex);
                return;
            }
                                   
            String ndcContextName = new StringBuffer(endpoint.getServiceUnitID())
                                    .append("::")
                                    .append(endpoint.getEndpointName())
                                    .append("::")
                                    .append(qualOperation.getLocalPart()).toString();
        
        
        //before processing make sure that shutdown has not started 
        String key = endpoint.getKey();
        //Acquire the life cycle lock
        int rs = ShutdownSynchronization.acquire(key);
        try{
            if(rs == SynchronizedStateChange.CHANGE_IN_PROGRESS){
                String errMsg = mMessages.getString("JMSBC-E0758.EndpointShutdownInProcess",
                        new Object[]{endpoint.getEndpointName(),
                                     endpoint.getServiceName(),
                                     msgExchange.getExchangeId()});

                MessageExchangeProcessingException mpex = new MessageExchangeProcessingException (errMsg);    
                
                handleMessagePreProcessingErrors (isInboundReply,
                                                  inbReplyContext,
                                                  msgExchange,
                                                  mpex);
                return;
            }
            
            int state = endpoint.getState();
            if (!(state == EndpointState.RUNNING)) {
                String strState = (state==EndpointState.STOPPED?"STOPPED":"SHUTDOWN");
                String errMsg = mMessages.getString("JMSBC-E0745.EndpointNotInRunState",
                        new Object[]{endpoint.getEndpointName(),
                                     endpoint.getServiceName(),
                                     strState,
                                     msgExchange.getExchangeId()});

                MessageExchangeProcessingException mpex = new MessageExchangeProcessingException (errMsg);    
                
                handleMessagePreProcessingErrors (isInboundReply,
                                                  inbReplyContext,
                                                  msgExchange,
                                                  mpex);
                return;
            } 

            switch (ExchangePattern.valueOf(msgExchange)) {
            case IN_OUT:                  
                // NDC - enter context
                Logger.getLogger("com.sun.EnterContext").fine(ndcContextName);                    
                if (isInboundReply) {
                    processRequestReplyInbound((InOut)msgExchange,
                                               endpoint,
                                               inbReplyContext,
                                               qualOperation);
                } else {
                    processRequestReplyOutbound((InOut)msgExchange,
                                                 endpoint,
                                                 jmsChannel,
                                                 qualOperation);
                }
                // NDC - exit context
                Logger.getLogger("com.sun.ExitContext").fine(ndcContextName);                                       
                break;
            case IN_ONLY:
                // NDC - enter context
                Logger.getLogger("com.sun.EnterContext").fine(ndcContextName);                    
                if (isInboundReply) {
                    processOneWayInbound((InOnly)msgExchange, 
                                          endpoint,
                                          inbReplyContext,
                                          qualOperation);
                } else {
                    processOneWayOutbound((InOnly)msgExchange, 
                                           endpoint,
                                           jmsChannel,
                                           qualOperation);
                }
                // NDC - exit context
                Logger.getLogger("com.sun.ExitContext").fine(ndcContextName);                                       
                break;
            default:
                String errMsg = 
                        mMessages.getString("JMSBC-E0746.UnsupportedMessageExchangePatternForMessageExchange",
                            new Object[]{msgExchange.getExchangeId(),
                                         msgExchange.getPattern()});

                MessageExchangeProcessingException mpex = new MessageExchangeProcessingException (errMsg);    
                
                handleMessagePreProcessingErrors (isInboundReply,
                                                  inbReplyContext,
                                                  msgExchange,
                                                  mpex);
                break;
            }
        }finally{
            //make sure to release the lock
            if(rs == SynchronizedStateChange.PASS)
            	ShutdownSynchronization.release(key);
        	
        }
    }

	private String getExchangeId(MessageExchange msgExchange) {
		String msgId = (String) msgExchange.getProperty(MEXID);
		/*
		 * if(msgId == null){ return msgExchange.getExchangeId(); } String
		 * exchangeId = (String)mInboundExchanges.get(msgId);
		 * 
		 * if(exchangeId == null){ return msgExchange.getExchangeId(); }
		 * 
		 * return exchangeId;
		 */
		return msgId;
	}

	private Endpoint getEndpoint(int endpointType,
			QName serviceName, String endpointName) {
		Endpoint endpoint = null;
		Iterator it = mServiceUnits.iterator();
        while (it.hasNext()) {
            ServiceUnit endpointSU = (ServiceUnit)it.next();
            endpoint = endpointSU.getEndpoint(serviceName.toString(),
                                              endpointName,
                                              endpointType);
            if(endpoint != null) {
                break;
            }
        }
		return endpoint;
	}
    
    private boolean isRequest(MessageExchange msgX) {
        boolean request = false;
        if (msgX instanceof InOut) {
            InOut inout = (InOut)msgX;
            request = inout.getOutMessage()==null &&
                      inout.getFault()==null &&
                      inout.getStatus()==ExchangeStatus.ACTIVE;                   
        } else if (msgX instanceof InOnly) {
            InOnly inonly = (InOnly)msgX;
            request = inonly.getFault()==null &&
                      inonly.getStatus()==ExchangeStatus.ACTIVE;
        }
        return request;
    }
    
    private TransactionManager getTransactionManager() {
        return (TransactionManager)mComponentContext.getTransactionManager();
    }
    
    
    /**
     * Process the reply for a request/reply request originating from this BC.
     */
    private void processRequestReplyInbound(InOut inout,
                                            Endpoint endpoint,
                                            InboundReplyContext inbReplyContext,
                                            QName qualOperation) {
                
        if (inout.getOutMessage() != null) {
            endpoint.getEndpointStatus().incrementReceivedReplies();
            
            Transaction tx = null;        
            // send "response" to JMS as a result of:
            // jmsbc->nmr, nmr->jmsbc(reply)
            try {
                boolean success = false;
                try {
                    inbReplyContext.getReplyListener().onOutput(inout);
                    success = true;
                } catch (Exception ex) {
                    throw ex;
                } finally {
                    // Callback listener to complete JMS message delivery
                    if (inbReplyContext.messageAckPending()) {
                        InboundReplyListener listener = inbReplyContext.getReplyListener();
                        listener.onReply(inout, success, false);
                    }
                }
                                
                inout.setStatus(ExchangeStatus.DONE);
            } catch (Exception t) { // catch all errors                
            	String faultCode = null;
            	String faultDetail = null;
            	String faultString = null;
            	if(t instanceof ChannelException){
                	faultCode = SERVER;
                	faultDetail = "Unable to send message to the JMS provider. Check if the JMS server is up.";
                	faultString = mMessages.getString(
    						"JMSBC-W0728.UnableToSendMessageToJMSProvider", new Object[] {
    								inout.getExchangeId(), endpoint.toString(),
    								qualOperation.toString() });
            	}else if(t instanceof WrapperProcessingException){
                	faultCode = CLIENT;
                	faultDetail = "Unable to denormalize message. The normalize reply message from consumer is not correct.";
                	faultString = mMessages.getString(
        					"JMSBC-W0730.UnableToDenormalizeReply", new Object[] {
        							inout.getExchangeId(), endpoint.toString(),
        							qualOperation.toString()});
            	}else if(t instanceof NMPropertiesParsingException){
                	faultCode = CLIENT;
                	faultDetail = "Unable to send message to the JMS provider. Error parsing NMProperties.";
                	faultString = t.getMessage();
            	}else{
                	faultCode = SERVER;
                	faultDetail = "Unable to process message exchange.";
                	faultString = mMessages.getString(
    						"JMSBC-W0727.UnableToProcessMessageExchange", new Object[] {
    								inout.getExchangeId(), endpoint.toString(),
    								qualOperation.toString() });
            	}
            	t = new Exception(faultString, t);
            	
                endpoint.getEndpointStatus().incrementSentErrors();            
                
                mLog.log(Level.SEVERE, 
                         "OutboundMessageProcessor_INBOUND_REPLY_PROCESSING_FAILED",
                         new Object [] {inout.getExchangeId(), 
                                        LogSupport.getStackTraceAsString(t)});
                AlertsUtil.getAlerter().critical(mMessages.getString("OutboundMessageProcessor_INBOUND_REPLY_PROCESSING_FAILED",
                         new Object [] {inout.getExchangeId(), 
                                        LogSupport.getStackTraceAsString(t)}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0727");

                setErrorInExchange(inout, faultCode, faultDetail, t);
                
                // Failed to deliver the JMS "reply" successfully, 
                // so send error back
                try {
                    inout.setStatus(ExchangeStatus.ERROR);

                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "OutboundMessageProcessor_MXCH_SET_STATUS",
                                 new Object[]{"ERROR", inout.getExchangeId(), inout.getPattern()});
                    }
                } catch (MessagingException ex2) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inout.getExchangeId(),
                                              inout.getPattern(),
                                              ex2.getLocalizedMessage()}),
                             ex2);
                    AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inout.getExchangeId(),
                                              inout.getPattern(),
                                              ex2.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0748");
                }                    
                
            } finally {
                try {
                    // Send back status to consumer
                	JMSBCContext.getRef().getChannel().send(inout);
                } catch (Throwable mex2) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                                new Object [] {inout.getExchangeId(),
                                               inout.getPattern(),
                                               mex2.getLocalizedMessage()}),
                             mex2);
                    AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                                new Object [] {inout.getExchangeId(),
                                               inout.getPattern(),
                                               mex2.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0749");
                }                
            }
        } else if (inout.getStatus() == ExchangeStatus.ERROR ||
                   inout.getError() != null ||
                   inout.getFault() != null) {
            
            if (inout.getError() != null) {
                Exception ex = inout.getError();
                mLog.log (Level.SEVERE, 
                          mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inout.getExchangeId(), 
                                             inout.getPattern(),
                                             ex.getLocalizedMessage()}),
                          ex);
                 AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inout.getExchangeId(), 
                                             inout.getPattern(),
                                             ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0752");
                
                checkForClientError(inout, endpoint, qualOperation);
            }            
            
            endpoint.getEndpointStatus().incrementReceivedErrors();
            
            
            try {
                boolean markedForDeletion = checkForRedelivery(inout, endpoint, inbReplyContext.getJMSMessage().getJMSMessageID());
                // Callback listener to complete JMS message delivery
                if (inbReplyContext.messageAckPending()) {
                    InboundReplyListener listener = inbReplyContext.getReplyListener();
                    if (inout.isTransacted()) {
                        Transaction tx = (Transaction)inout.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                        rollbackTransaction(inout, tx);
                    }
                    listener.onReply(inout, false, markedForDeletion);
                } 
            } catch (Throwable ex) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{inout.getExchangeId(),
                                          inout.getPattern(),
                                          ex.getLocalizedMessage()}),
                         ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{inout.getExchangeId(),
                                          inout.getPattern(),
                                          ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0750");
                
            }
        } 
        
        // Remove the message exchange from the inbound message exchange map
        removeMexId(inout);        
    }

	private void checkForClientError(MessageExchange ex, Endpoint endpoint,
			QName qualOperation) {
		String faultCode = (String)ex.getProperty("com.sun.jbi.crl.faultcode");
		if(faultCode != null && faultCode.trim().equalsIgnoreCase(CLIENT)){
			//The problem is in the client message format. Log this information 
			//and send an alert so that client can take some action to either suspend this
			//EP or redeploy with dead letter queue configuration.
			String msg = mMessages.getString("JMSBC-W0731.InvalidData", new Object[] {
							ex.getExchangeId(), endpoint.toString(),
							qualOperation.toString() });
			mLog.log(Level.WARNING, msg);
		    AlertsUtil.getAlerter().warning (msg, 
		    		AlertsUtil.SUN_JMS_BINDING, 
		            null, 
		            AlertsUtil.getServerType(),
		            AlertsUtil.COMPONENT_TYPE_BINDING,
		            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		            NotificationEvent.EVENT_TYPE_ALERT,
		            "JMSBC-W0731");
			
		}
	}

	private void rollbackTransaction(MessageExchange me, Transaction tx)
			throws Exception, SystemException {
		if(tx == null)
			return;
		
		try{
			resumeThreadTx(tx, me);
			tx.setRollbackOnly();
			if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
			    mLog.log(LogSupport.LEVEL_DEBUG,
			             "OutboundMessageProcessor_TX_SET_ROLLBACK_SUCCEEDED",
			             new Object [] {tx.toString(), 
			                            me.getExchangeId(), 
			                            me.getPattern()});
			}
		}catch(Exception t){
			    mLog.log(Level.SEVERE, mMessages
					.getString("JMSBC-E0761.CouldNotRollbackTransaction",
							new Object[] { tx.toString(),
									me.getEndpoint().getServiceName(),
									me.getEndpoint().getEndpointName(),
									tx.getStatus()}), t);
					AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0761.CouldNotRollbackTransaction",
							new Object[] { tx.toString(),
									me.getEndpoint().getServiceName(),
									me.getEndpoint().getEndpointName(),
									tx.getStatus()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0761");
		}finally{
			try{
				suspendThreadTx(tx, me);
			}catch(Exception e){}
		}
	}
    
    private void processRequestReplyOutbound (InOut inout,
                                              Endpoint endpoint,
                                              Channel jmsChannel,
                                              QName qualOperation) {
        JMSOperation jmsOperation = 
            (JMSOperation)endpoint.getJMSOperations()
                                  .get(qualOperation);
        
    	String dest = jmsOperation.getDestination();
    	boolean isTopic = jmsOperation.getDestinationType().equalsIgnoreCase(JMSConstants.TOPIC);
		String transaction = jmsOperation.getTransaction();
    	
        if (inout.getStatus() == ExchangeStatus.DONE) {            
             // DONE status on reply as a result of:
             // nmr->jmsbc, jmsbc->nmr, nnmr->jmsbc(DONE)
            endpoint.getEndpointStatus().incrementReceivedDones();                                    
        } else if (inout.getStatus() == ExchangeStatus.ERROR ||
                   inout.getError() != null) {
            
            if (inout.getError() != null) {
                Exception ex = inout.getError();
                mLog.log (Level.SEVERE, 
                          mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inout.getExchangeId(), 
                                             inout.getPattern(),
                                             ex.getLocalizedMessage()}),
                          ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inout.getExchangeId(), 
                                             inout.getPattern(),
                                             ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0752");
            }
            
            // ERROR status on reply as a result of:
            // nmr->jmsbc, jmsbc->nmr, nnmr->jmsbc(ERROR)
            endpoint.getEndpointStatus().incrementReceivedErrors();
            
            
            
        } else { 
            // Process outbound request/reply as a result of:
            // nmr->jmsbc
            endpoint.getEndpointStatus().incrementReceivedRequests();
            
            //
            // InOut exchange process follows:
            // Send request to JMS, reply from JMS will be received asynchronously.
            // The MessageExchange will be stored in an exchange map for later
            // processing by the asynchronous receiving channel.
            //
            
            SendChannel sendChannel = (SendChannel)jmsChannel;
            String faultCode = null;
            String faultDetail = null;
            try {
                
                JMSMessage jmsOutput = endpoint.getJMSOperationOutput(jmsOperation).getJMSMessage();

                String msgXchangeID = inout.getExchangeId();
                
                NormalizedMessage normalizedMsg = inout.getInMessage();
                long timeout = jmsOperation.getTimeout().longValue();
                // Change retries?
                Message jmsReplyMsg = null;

                if(jmsOperation.getVerb().equals(JMSOperation.VERB_READ)){
                    //Get the NMProperties and get new send channel
                	SynchronousReadNMProperties nmProperties = NMPropertiesUtil.getSynchronousReadNMProperties(normalizedMsg);
        			sendChannel = NMPropertiesUtil.getNewSendChannelIfRequired(
        					sendChannel, nmProperties, endpoint);
                    //--------------------------------
                	//Override properties from NMProperties
                	if(nmProperties.getDestination() != null){
                		dest = nmProperties.getDestination();
                	}
            		if(nmProperties.getDestinationtype() != null){
            			isTopic = nmProperties.getDestinationtype().equalsIgnoreCase(JMSConstants.TOPIC);
            		}
                	
                	if(nmProperties.getTimeOut() != -1){
                		timeout = nmProperties.getTimeOut();
                	}
                	if(nmProperties.getXatransaction() != null){
                		transaction = nmProperties.getXatransaction();
                	}
                	//----End overriding properties--------------
                	
                	//For synchronous read we should try to enable the transaction.
                	Transaction tx = getTransactionFromMessageProperties(inout, qualOperation, transaction, dest);
                    try{
                        resumeThreadTx(tx, inout);
                    	try{
                            jmsReplyMsg = sendChannel.synchronousReceive(dest, isTopic, timeout);                
                    	}finally{
                            suspendThreadTx(tx, inout);
                    	}
                    }catch(ChannelException ex){
    	            	faultCode = SERVER;
    	            	faultDetail = "Unable to read message from the JMS provider. Check if the JMS server is up and try again.";
    	            	String faultString = mMessages.getString(
    							"JMSBC-W0734.UnableToReadMessageFromJMSProvider", new Object[] {
    									inout.getExchangeId(), endpoint.toString(),
    									qualOperation.toString() });
    	            	throw new Exception(faultString, ex);
                    	
                    }catch(Throwable t){
    	            	faultCode = CLIENT;
    	            	faultDetail = "Unable to read message from the JMS provider. Provide all the required parameters";
    	            	throw new Exception(t.getMessage(), t);
                    }
                    //Override forwarded as attachment property
                    if(nmProperties.getForwardAsAttachment() != null){
                    	jmsOutput = jmsOutput.getCopy();
                    	jmsOutput.setForwardAsAttachment(Boolean.parseBoolean(nmProperties.getForwardAsAttachment()));
                    }
                    //---------------------------
                    
                }else{
                	
                    //Get the NMProperties and get new send channel
                	OutBoundInOutNMProperties nmProperties = NMPropertiesUtil.getOutBoundInOutNMProperties(normalizedMsg);
        			sendChannel = NMPropertiesUtil.getNewSendChannelIfRequired(
        					sendChannel, nmProperties, endpoint);
                    //--------------------------------
                	
                	JMSMessage jmsInput = endpoint.getJMSOperationInput(jmsOperation).getJMSMessage();
                    Message jmsMsg;
                    try{
                    	jmsMsg = denormalizeMessage(msgXchangeID,
                                                        normalizedMsg,
                                                        endpoint,
                                                        jmsOperation,
                                                        jmsInput,
                                                        sendChannel);
    	            }catch(Exception e){
    	            	faultCode = CLIENT;
    	            	faultDetail = "Unable to denormalize message. The normalize message from consumer is not correct.";
    	            	String faultString = mMessages.getString(
    							"JMSBC-W0725.UnableToDenormalize", new Object[] {
    									inout.getExchangeId(), endpoint.toString(),
    									qualOperation.toString() });
    	            	throw new Exception(faultString, e);
    	            }
                    
    	            //Set NM properties into message
    	            NMPropertiesUtil.fillJMSMessage(jmsMsg, nmProperties);
                    
                	//Override properties from NMProperties
                	if(nmProperties.getDestination() != null){
                		dest = nmProperties.getDestination();
                	}
            		if(nmProperties.getDestinationtype() != null){
            			isTopic = nmProperties.getDestinationtype().equalsIgnoreCase(JMSConstants.TOPIC);
            		}
                	if(nmProperties.getTimeOut() != -1){
                		timeout = nmProperties.getTimeOut();
                	}
                    String replyToDest = nmProperties.getReplytodestination();
                    boolean isTopicReplyToDest = (replyToDest != null)
							&& nmProperties.getReplytodestinationtype()
									.equalsIgnoreCase(JMSConstants.TOPIC);
                	//----End overriding properties--------------
                    
                    try{
                        jmsReplyMsg = sendChannel.send(jmsMsg, dest, isTopic,
								replyToDest, isTopicReplyToDest, timeout);                
                        //jmsReplyMsg = sendChannel.send(jmsMsg,timeout);                
                    	
                    }catch(ChannelException ex){
    	            	faultCode = SERVER;
    	            	faultDetail = "Unable to send message to the JMS provider. Check if the JMS server is up and try again.";
    	            	String faultString = mMessages.getString(
    							"JMSBC-W0728.UnableToSendMessageToJMSProvider", new Object[] {
    									inout.getExchangeId(), endpoint.toString(),
    									qualOperation.toString() });
    	            	throw new Exception(faultString, ex);
                    	
                    }
                    
                    //Override forwarded as attachment property
                    if(nmProperties.getForwardAsAttachment() != null){
                    	jmsOutput = jmsOutput.getCopy();
                    	jmsOutput.setForwardAsAttachment(Boolean.parseBoolean(nmProperties.getForwardAsAttachment()));
                    }
                    //---------------------------
                    
                }
                // Got a reply jms message, send back to consumer
                NormalizedMessage outNormalizedMsg = inout.createMessage();
                if (jmsReplyMsg == null) {
                	
                	/*
					 * Do not throw exception send empty message faultCode =
					 * SERVER; faultDetail = "No reply from the external
					 * service. Check if the external service is up and try
					 * again."; String errMsg =
					 * mMessages.getString("JMSBC-E0751.NoReplyFromExternalReceived",
					 * new Object[]{timeout, inout.getExchangeId(),
					 * inout.getPattern()}); throw new
					 * MessageExchangeProcessingException (errMsg); // throw and
					 * catch
					 */

                	this.mJMSNormalizer.generateEmptyMessage(outNormalizedMsg,
							endpoint, jmsOperation, jmsOutput, false);
                } else {

                    try{
                    normalizeMessage(jmsReplyMsg,
                                     outNormalizedMsg,
                                     endpoint,
                                     jmsOperation,
                                     msgXchangeID,
                                     jmsOutput);
                    
                    }catch(Exception e){
    	            	faultCode = SERVER;
    	            	faultDetail = "Unable to normalize response from the external service.";
    	            	String faultString = mMessages.getString(
    							"JMSBC-W0729.UnableToNormalizeResponseFromExternalService", new Object[] {
    									inout.getExchangeId(), endpoint.toString(),
    									qualOperation.toString() });
    	            	throw new Exception(faultString, e);
                    }
                }
                //Set all NM properties in the message exchange
                NMPropertiesUtil
						.fillNMProperties(jmsReplyMsg, outNormalizedMsg,
								((SendChannelJCAImpl) sendChannel)
										.getJMSOperation(),
								((SendChannelJCAImpl) sendChannel)
										.getJMSAddress(), dest, isTopic, jmsOutput.forwardAsAttachment());
                
                inout.setOutMessage(outNormalizedMsg);
                
                if (this.mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG,
                             "OutboundMessageProcessor_SENDING_BACK_EXCHANGE_WITH_OUTPUT",
                             new Object[]{inout.getExchangeId(),
                                          inout.getPattern()});
                }
                
            } catch (Throwable ex)  {
            	if(ex instanceof NMPropertiesParsingException){
                	faultCode = CLIENT;
                	faultDetail = "Error parsing normalized message properties. Set correct message properties.";
                	String faultString = ex.getMessage();
                	ex = new Exception(faultString, ex);
            	}
            	
            	if(faultCode == null){
                	faultCode = SERVER;
                	faultDetail = "Unable to process message exchange.";
                	String faultString = mMessages.getString(
    						"JMSBC-W0727.UnableToProcessMessageExchange", new Object[] {
    								inout.getExchangeId(), endpoint.toString(),
    								qualOperation.toString() });
                	ex = new Exception(faultString, ex);
            	}
                                
                endpoint.getEndpointStatus().incrementSentErrors();            
                
                mLog.log(Level.SEVERE, 
                         mMessages.getString("JMSBC-E0747.MessageExchangeProcessError",
                             new Object [] {inout.getExchangeId(),
                                            inout.getPattern(),
                                            ex.getLocalizedMessage()}),
                         ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0747.MessageExchangeProcessError",
                             new Object [] {inout.getExchangeId(),
                                            inout.getPattern(),
                                            ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0747");
                                    
                setErrorInExchange(inout, faultCode, faultDetail, (Exception)ex);
                
                // setError should set status to error, but just in case...
                try {
                    inout.setStatus(ExchangeStatus.ERROR);

                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "OutboundMessageProcessor_MXCH_SET_STATUS",
                                 new Object[]{"ERROR", inout.getExchangeId(), inout.getPattern()});
                    }

                } catch (MessagingException ex2) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inout.getExchangeId(),
                                              inout.getPattern(),
                                              ex.getLocalizedMessage()}),
                             ex2);
                    AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inout.getExchangeId(),
                                              inout.getPattern(),
                                              ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0748");
                }                    
            } finally {
                // JMS "send" failed so send back fault or error to NMR
                try {
                    // Send back exchange fault or error
                	JMSBCContext.getRef().getChannel().send(inout);
                } catch (Exception mex2) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                                new Object [] {inout.getExchangeId(),
                                               inout.getPattern(),
                                               mex2.getLocalizedMessage()}),
                             mex2);
                    AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                                new Object [] {inout.getExchangeId(),
                                               inout.getPattern(),
                                               mex2.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0749");
                }
                
                compareAndClose(endpoint, jmsChannel, sendChannel);
            }
        }
    }
    
    /**
     * Process the reply for the inbound inonly exchange
     */
    private void processOneWayInbound(InOnly inonly, 
                                      Endpoint endpoint,
                                      InboundReplyContext inbReplyContext,
                                      QName qualOperation) {
        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();

            try {
            	//We need to check for redelivery failure. In case of delete the 
            	//MessageExchange wrapper simply reply with done. This method call would simply
            	//log the fact that message has been deleted
                //checkForRedelivery(inonly, endpoint, inbReplyContext.getJMSMessage().getJMSMessageID());
                if (inbReplyContext.messageAckPending()) {
                    InboundReplyListener listener = inbReplyContext.getReplyListener();
                    listener.onReply(inonly, true, false);
                }
            } catch (Exception ex) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{inonly.getExchangeId(), 
                                          inonly.getPattern(),
                                          ex.getLocalizedMessage()}),
                         ex);
               AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{inonly.getExchangeId(), 
                                          inonly.getPattern(),
                                          ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0750");
            }            
        } else if (inonly.getStatus() == ExchangeStatus.ERROR ||
                   inonly.getError() != null) {
            
            if (inonly.getError() != null) {
                Exception ex = inonly.getError();
                mLog.log (Level.SEVERE, 
                          mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inonly.getExchangeId(), 
                                             inonly.getPattern(),
                                             ex.getLocalizedMessage()}),
                          ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0752.ReceivedErrorOnMessageExchangeWithException",
                              new Object [] {inonly.getExchangeId(), 
                                             inonly.getPattern(),
                                             ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0752");
                checkForClientError(inonly, endpoint, qualOperation);
            }
            
            
            
            endpoint.getEndpointStatus().incrementReceivedErrors();
            
            try {                
                boolean markedForDeletion = checkForRedelivery(inonly, endpoint, inbReplyContext.getJMSMessage().getJMSMessageID());
                if (inbReplyContext.messageAckPending()) {
                    InboundReplyListener listener = inbReplyContext.getReplyListener();
                    if (inonly.isTransacted()) {
                        Transaction tx = (Transaction)inonly.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                        rollbackTransaction(inonly, tx);
                    }
                    listener.onReply(inonly, false, markedForDeletion);
                }
            } catch (Exception ex) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                              new Object[]{inonly.getExchangeId(),
                                           inonly.getPattern(),
                                           ex.getLocalizedMessage()}),
                         ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                              new Object[]{inonly.getExchangeId(),
                                           inonly.getPattern(),
                                           ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0750");
            }            
        }
        
        // Remove the message exchange from the inbound message exchange map
        removeMexId(inonly);
    }

	private boolean checkForRedelivery(MessageExchange ex, Endpoint endpoint, String jmsMessageId) {
		boolean deleteMessage = false;
		//Here try to redeliver the code if redelivery is enabled
		RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(ex);
		//if(retryStatus == null)
		{
		    EndpointInfo info = new EndpointInfo(false, endpoint
					.getEndpointName(), null, endpoint.getServiceName(), null);
			RedeliveryConfig retryConfig = ((MessagingChannel) JMSBCContext
					.getRef().getChannel()).getServiceQuality(info,
					RedeliveryConfig.class);
			if (retryConfig != null) {
				Failure onFailureOption = retryConfig.getFailure();
				if (onFailureOption == Failure.suspend) {
					try {
						endpoint.getServiceUnit().suspend(endpoint);
					} catch (Exception e) {
						mLog.log(Level.SEVERE, "JMSBC-E0764.CouldNotSuspendEndpoint", new Object[]{endpoint.getKey()});
						AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0764.CouldNotSuspendEndpoint", new Object[]{endpoint.getKey()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0764");
					}
				} else if (onFailureOption == Failure.error) {
					// Not decided what to do in error. As of now do whatever is
					// done in delete
                    mLog.log (Level.WARNING, mMessages.getString("JMSBC-W0723.RedeliveryFailed",
                            new Object[] {retryConfig.getMaxRetries() + "",
                    					info.getEndpointName(),
                                        info.getServiceName(),
                                        ex.getExchangeId(),
                                        jmsMessageId}));
					deleteMessage = false;
				} else if (onFailureOption == Failure.delete) {
					//Log this fact
                    mLog.log (Level.WARNING, mMessages.getString("JMSBC-W0739.RedeliveryFailed",
                            new Object[] {retryConfig.getMaxRetries() + "",
                    					info.getEndpointName(),
                                        info.getServiceName(),
                                        ex.getExchangeId(),
                                        jmsMessageId}));
					deleteMessage = false;
				} else if (onFailureOption == Failure.redirect) {
					//Log this fact
                    mLog.log (Level.WARNING, mMessages.getString("JMSBC-W0740.RedeliveryFailed",
                            new Object[] {retryConfig.getMaxRetries() + "",
                    					info.getEndpointName(),
                                        info.getServiceName(),
                                        ex.getExchangeId(),
                                        jmsMessageId,
                                        ex.getEndpoint().getEndpointName(),
                                        ex.getEndpoint().getServiceName()}));
					deleteMessage = false;
				}
			}
		}
		return deleteMessage;
	}
    
    
    private void processOneWayOutbound(InOnly inonly, 
                                       Endpoint endpoint,
                                       Channel jmsChannel,
                                       QName qualOperation) {
                
        Transaction tx = null;        
        String faultCode = null;
        String faultDetail = null;
        SendChannel sendChannel = (SendChannel)jmsChannel;
        try {
            // Increment received requests on endpoint (bc is provider)
            endpoint.getEndpointStatus().incrementReceivedRequests();
                        
            JMSOperation jmsOperation = 
                    (JMSOperation)endpoint.getJMSOperations()
                                          .get(qualOperation);
            JMSInput jmsInput = endpoint.getJMSOperationInput(jmsOperation);
            
            String msgXchangeID = inonly.getExchangeId();
            
            NormalizedMessage normalizedMsg = inonly.getInMessage();            
            Message jmsMsg;
            
            //Get the NMProperties and get new send channel
            OutBoundInOnlyNMProperties nmProperties = NMPropertiesUtil.getOutBoundInOnlyNMProperties(normalizedMsg);
			sendChannel = NMPropertiesUtil.getNewSendChannelIfRequired(
					sendChannel, nmProperties, endpoint);
            //--------------------------------
            
            //Override transaction attribute from NMProperties
            String transaction = jmsOperation.getTransaction();
            if(nmProperties.getXatransaction() != null){
            	transaction = nmProperties.getXatransaction();
            }
            //----------------------------------
            
            try{
            	
	            tx = getTransactionFromMessageProperties(inonly, qualOperation, transaction, jmsOperation.getDestination());
	            resumeThreadTx(tx, inonly);
	            
	            try{
	                jmsMsg = denormalizeMessage(msgXchangeID,
	                                                normalizedMsg,
	                                                endpoint,
	                                                jmsOperation,
	                                                jmsInput.getJMSMessage(),
	                                                sendChannel);
	            }catch(ChannelException e){
	            	faultCode = SERVER;
	            	faultDetail = "Unable to send message to the JMS provider. Check if the JMS server is up and try again.";
	            	String faultString = mMessages.getString(
							"JMSBC-W0728.UnableToSendMessageToJMSProvider", new Object[] {
									inonly.getExchangeId(), endpoint.toString(),
									qualOperation.toString() });
	            	throw new Exception(faultString, e);
	            }catch(Exception e){
	            	faultCode = CLIENT;
	            	faultDetail = "Unable to denormalize message. The normalize message from consumer is not correct.";
	            	String faultString = mMessages.getString(
							"JMSBC-W0725.UnableToDenormalize", new Object[] {
									inonly.getExchangeId(), endpoint.toString(),
									qualOperation.toString() });
	            	throw new Exception(faultString, e);
	            }
            
                 sendMessageToJMS(jmsMsg, jmsOperation, sendChannel, tx, inonly, endpoint, nmProperties);
                 inonly.getInMessage().setProperty(NMPropertiesUtil.INOUT_MESSAGEID, jmsMsg.getJMSMessageID());
            }catch(SystemException e){
            	faultCode = SERVER;
            	faultDetail = "Unable to resume/suspend transaction. Rollback the transaction and try again.";
            	String faultString = mMessages.getString(
						"JMSBC-W0726.UnableToSuspendResumeTransaction", new Object[] {
								inonly.getExchangeId(), endpoint.toString(),
								qualOperation.toString() });
            	throw new Exception(faultString, e);
            }catch(ChannelException ex){
            	faultCode = SERVER;
            	faultDetail = "Unable to send message to the JMS provider. Check if the JMS server is up and try again.";
            	String faultString = mMessages.getString(
						"JMSBC-W0728.UnableToSendMessageToJMSProvider", new Object[] {
								inonly.getExchangeId(), endpoint.toString(),
								qualOperation.toString() });
            	throw new Exception(faultString, ex);
            }finally{
                suspendThreadTx(tx, inonly);
            }
            
            inonly.setStatus(ExchangeStatus.DONE);

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "OutboundMessageProcessor_MXCH_SET_STATUS",
                         new Object[]{"DONE", inonly.getExchangeId(), inonly.getPattern()});
            }
            
            endpoint.getEndpointStatus().incrementSentDones();
        } catch (Exception e) {
        	if(e instanceof NMPropertiesParsingException){
            	faultCode = CLIENT;
            	faultDetail = "Error parsing normalized message properties. Set correct message properties.";
            	String faultString = e.getMessage();
            	e = new Exception(faultString, e);
        	}
        	if(faultCode == null){
            	faultCode = SERVER;
            	faultDetail = "Unable to process message exchange.";
            	String faultString = mMessages.getString(
						"JMSBC-W0727.UnableToProcessMessageExchange", new Object[] {
								inonly.getExchangeId(), endpoint.toString(),
								qualOperation.toString() });
            	e = new Exception(faultString, e);
        	}
        	
            endpoint.getEndpointStatus().incrementSentErrors();
            mLog.log(Level.SEVERE, 
                     mMessages.getString("JMSBC-E0747.MessageExchangeProcessError",
                         new Object [] {inonly.getExchangeId(),
                                        inonly.getPattern(),
                                        e.getLocalizedMessage()}),
                     e);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0747.MessageExchangeProcessError",
                         new Object [] {inonly.getExchangeId(),
                                        inonly.getPattern(),
                                        e.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0747");

            setErrorInExchange(inonly, faultCode, faultDetail, e);
        	
            try {
                inonly.setStatus(ExchangeStatus.ERROR);
                
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "OutboundMessageProcessor_MXCH_SET_STATUS",
                             new Object[]{"ERROR", inonly.getExchangeId(), inonly.getPattern()});
                }
                
            } catch (MessagingException ex) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inonly.getExchangeId(),
                                              inonly.getPattern(),
                                              ex.getLocalizedMessage()}),
                             ex);
                    AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                                 new Object[]{inonly.getExchangeId(),
                                              inonly.getPattern(),
                                              ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0748");
            }            
        } finally {                
            try {
                // Send back exchange status
            	JMSBCContext.getRef().getChannel().send(inonly);
            } catch (Throwable mex) {
                final String msg = mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                                new Object [] {inonly.getExchangeId(),
                                               inonly.getPattern(),
    				                   mex.getLocalizedMessage()});
				mLog.log(Level.SEVERE,msg, mex);
                try {
                	faultCode = SERVER;
                	faultDetail = "Unable to process message exchange.";
                	String faultString = msg;
                	Exception e = new Exception(faultString, mex);
                	setErrorInExchange(inonly, faultCode, faultDetail, e);
                	//Try to send ERROR status
					inonly.setStatus(ExchangeStatus.ERROR);
	            	JMSBCContext.getRef().getChannel().send(inonly);
				} catch (MessagingException e) {
					mLog.log(Level.SEVERE,msg, e);
				}
                
              AlertsUtil.getAlerter().critical(msg, 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0749");
            }
            
        	compareAndClose(endpoint, jmsChannel, sendChannel);
        }
    }
    
	private Transaction getTransactionFromMessageProperties(MessageExchange exchange,
			QName qualOperation, String transaction, String dest) {
		Transaction tx = null;
		// Participate in XA transaction?
		boolean xa = transaction.equals(JMSConstants.TRANSACTION_XA);
		
		if (xa && exchange.isTransacted()) {
		    // If XA send, get the Transaction from the MessageExchange
		    // if there is one and call the send within context of that
		    // transaction
		    tx = (Transaction)exchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
		    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
		        mLog.log (LogSupport.LEVEL_DEBUG, "OutboundMessageProcessor_OPER_MARKED_XA_FOUND_TX_IN_MXCH",
		                                            new Object[] {qualOperation.toString(),
		                                                          exchange.getExchangeId(),
		                                                          dest,
		                                                          tx});
		    }
		} else if (xa && !exchange.isTransacted()) {
		    // Allow send but report warning
		    mLog.log(Level.INFO, "JMSBC-I0715.TransactionContextNotFound",
		                            new Object[] {qualOperation.toString(),
		                                          exchange.getExchangeId(),
		                                          dest});
		} else if (!xa && exchange.isTransacted()) {
		    Transaction temp = (Transaction)exchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
		    // Allow send but report warning
		    mLog.log(Level.INFO, "JMSBC-I0716.TransactionContextFound",
		                            new Object[] {qualOperation.toString(),
		                                          exchange.getExchangeId(),
		                                          dest,
		                                          temp});
		}
		return tx;
	}

	private void setErrorInExchange(MessageExchange ex, String faultCode,
			String faultDetail, Exception e) {
		ex.setError(e);
		ex.setProperty("com.sun.jbi.crl.faultcode", faultCode);
		ex.setProperty("com.sun.jbi.crl.faultstring", e.getMessage());
		ex.setProperty("com.sun.jbi.crl.faultactor", AlertsUtil.SUN_JMS_BINDING);
		ex.setProperty("com.sun.jbi.crl.faultdetail", faultDetail);
	}
    
    private Message denormalizeMessage (String msgXchangeID,
                                        NormalizedMessage normalizedMsg,                                                  
                                        Endpoint endpoint,
                                        JMSOperation jmsOp,
                                        JMSMessage mappingInfo,
                                        SendChannel sendChannel) 
        throws Exception {
        
        logNormalizedMessage(msgXchangeID, normalizedMsg.getContent());
        
        String strJMSMsgType = mappingInfo.getMessageType();

        // Create the JMS Message to send
        Message jmsMsg = null;            
        int iJMSMsgType = Util.toIntMessageType(strJMSMsgType);
        switch (iJMSMsgType) {
            case Channel.MESSAGE_TYPE_TEXT:
            case Channel.MESSAGE_TYPE_MAP:                
            case Channel.MESSAGE_TYPE_BYTES:                
                jmsMsg = sendChannel.createMessage(strJMSMsgType);                
                break;
            default:
                throw new MessageExchangeProcessingException (
                         mMessages.getString("JMSBC-E0702.UnsupportedJMSMessageType",
                         new Object[]{strJMSMsgType}));                
        } 

        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                     "OutboundMessageProcessor_BEGIN_DENORMALIZE_MESSAGE",
                     new Object [] {msgXchangeID, strJMSMsgType});
        }

        boolean isOperationInput = true;
        // Denormalize from NMS message to JMS message
        NamesUtil helper = new NamesUtil();
        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)?
        		(helper.createConsumingEndpointIdentifier(
        				endpoint.getServiceName(), endpoint.getEndpointName()))
        				: (helper.createProvisioningEndpointIdentifier(
        						endpoint.getServiceName(), endpoint.getEndpointName()));
        Probe denormalizationMeasurement = Probe.info(getClass(),
        		endPointID,
                JMSBindingComponent.PERF_CAT_DENORMALIZATION);
        this.mJMSDenormalizer.denormalize(normalizedMsg,
                                          jmsMsg,
                                          endpoint,
                                          jmsOp,
                                          mappingInfo,
                                          isOperationInput);
        if (denormalizationMeasurement != null) {
        	denormalizationMeasurement.end();
        }
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                     "OutboundMessageProcessor_END_DENORMALIZE_MESSAGE",
                     new Object [] {msgXchangeID, strJMSMsgType});
        }       
        
        return jmsMsg;
    }
    
    private void normalizeMessage (javax.jms.Message jmsMsg,
                                   NormalizedMessage normalizedMsg,
                                   Endpoint endpoint,
                                   JMSOperation jmsOp,
                                   String msgXchangeID,
                                   JMSMessage mappingInfo) throws Exception {
        if (jmsMsg instanceof TextMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                String jmsMessageID = jmsMsg.getJMSMessageID();                
                TextMessage jmsTextMsg = (TextMessage)jmsMsg;
                String textMsg = jmsTextMsg.getText();
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "OutboundMessageProcessor_JMSMSG_TEXTMSG", 
                        new Object[]{jmsMsg.getClass().getName(), 
                                     jmsMessageID,
                                     msgXchangeID, 
                                     textMsg});
            }
        } else if (jmsMsg instanceof MapMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                String jmsMessageID = jmsMsg.getJMSMessageID();                
                mLog.log(LogSupport.LEVEL_DEBUG,
                         "OutboundMessageProcessor_JMSMSG_NON_TEXTMSG", 
                         new Object[]{jmsMsg.getClass().getName(), 
                                      jmsMessageID,
                                      msgXchangeID});
            }
        } else if (jmsMsg instanceof BytesMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                String jmsMessageID = jmsMsg.getJMSMessageID();                
                mLog.log(LogSupport.LEVEL_DEBUG,
                         "OutboundMessageProcessor_JMSMSG_NON_TEXTMSG", 
                         new Object[]{jmsMsg.getClass().getName(), 
                                      jmsMessageID,
                                      msgXchangeID});
            }
        } else {
            throw new MessageExchangeProcessingException (mMessages.getString(
                                 "JMSBC-E0702.UnsupportedJMSMessageType",
                                 new Object[]{jmsMsg.getClass().getName()}));            
        }

        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                     "OutboundMessageProcessor_BEGIN_NORMALIZE_MESSAGE",
                     new Object [] {jmsMsg.getClass().getName(),
                                    msgXchangeID});
        }

        boolean isOperationInput = false;
        NamesUtil helper = new NamesUtil();
        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)?
        		(helper.createConsumingEndpointIdentifier(
        				endpoint.getServiceName(), endpoint.getEndpointName()))
        				: (helper.createProvisioningEndpointIdentifier(
        						endpoint.getServiceName(), endpoint.getEndpointName()));
        Probe normalizationMeasurement = Probe.info(getClass(),
        		endPointID,
        		JMSBindingComponent.PERF_CAT_NORMALIZATION);
        this.mJMSNormalizer.normalize(jmsMsg,
                                      normalizedMsg,
                                      endpoint,
                                      jmsOp,
                                      mappingInfo,
                                      isOperationInput);
        if (normalizationMeasurement != null) {
            normalizationMeasurement.end();
        }
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "OutboundMessageProcessor_END_NORMALIZE_MESSAGE",
                     new Object [] {jmsMsg.getClass().getName(),
                                    msgXchangeID});               
        }
    }
    
    
    /**
     * Sends the JMS Message
     */
    private void sendMessageToJMS (Message jmsMsg,
                                   JMSOperation jmsOperation,
                                   SendChannel sendChannel,
                                   Transaction tx,
                                   MessageExchange msgEx,
                                   Endpoint endpoint,
                                   OutBoundInOnlyNMProperties nmProperties) throws Exception {
    	
    	
        //Set NM properties into message
        NMPropertiesUtil.fillJMSMessage(jmsMsg, nmProperties);

        //Override the destination if available in the nmProperties
        String dest = jmsOperation.getDestination();
        boolean isTopic = jmsOperation.getDestinationType().equalsIgnoreCase(JMSConstants.TOPIC);
        
        if(nmProperties.getDestination() != null){
        	dest = nmProperties.getDestination();
        }
		if(nmProperties.getDestinationtype() != null){
        	isTopic = nmProperties.getDestinationtype().equalsIgnoreCase(JMSConstants.TOPIC);
		}
        String replyToDest = nmProperties.getReplytodestination();
        boolean isTopicReplyToDest = (replyToDest != null)
				&& nmProperties.getReplytodestinationtype()
						.equalsIgnoreCase(JMSConstants.TOPIC);
        //----------------------------------------------------
        
        try {
            //sendChannel.send(jmsMsg);           
            sendChannel.send(jmsMsg, dest, isTopic, replyToDest, isTopicReplyToDest);           
        } catch (Exception ex) {
            if (tx != null) {
                try {
                    tx.setRollbackOnly();
                } catch (Exception ex1) {
                    mLog.log(Level.WARNING,
                            "JMSBC-W0712.TransactionSetRollbackOnlyFailed",
                            new Object [] {tx, 
                                           msgEx.getExchangeId(), 
                                           msgEx.getPattern(), 
                                           ex.getLocalizedMessage()});
                     AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-W0712.TransactionSetRollbackOnlyFailed",
                            new Object [] {tx, 
                                           msgEx.getExchangeId(), 
                                           msgEx.getPattern(), 
                                           ex.getLocalizedMessage()}), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-W0712");
                             }
            }
            throw ex;
        }
    }
    
    private static String getRequestReplyMessageID (Message jmsMessage,
                                                    String msgExchangeID) 
    throws Exception {
        String key = null;
        // If application supplied JMSCorrelationID is available, use it as the key
        // Otherwise, use the MessageExchange ID as the key
        if (jmsMessage.getJMSCorrelationID() != null) {
            key = jmsMessage.getJMSCorrelationID();
        } else {
            key = msgExchangeID;
        }
        
        return key;
    }    
        
    // suspend thread transactional context
    private void suspendThreadTx (Transaction tx, MessageExchange msgEx) throws Exception {
        // suspend only if current running transaction exists
        if (tx != null) {
            ((TransactionManager)mComponentContext.getTransactionManager())
                                                  .suspend();
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log (LogSupport.LEVEL_DEBUG, 
                          mMessages.getString("OutboundMessageProcessor_TX_SUSPEND_SUCCEEDED",
                              new Object[] {tx.toString(),
                                            msgEx.getExchangeId(),
                                            msgEx.getPattern()}));
            }
        }
    }

    // suspend thread transactional context
    private void resumeThreadTx (Transaction tx, MessageExchange msgEx) throws Exception {
        if (tx != null) {
            ((TransactionManager)mComponentContext.getTransactionManager())
                                                  .resume(tx);
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log (LogSupport.LEVEL_DEBUG, 
                          mMessages.getString("OutboundMessageProcessor_TX_RESUME_SUCCEEDED",
                               new Object[] {tx.toString(),
                                             msgEx.getExchangeId(),
                                             msgEx.getPattern()}));
            }
        }
    }
    

    private void handleMessagePreProcessingErrors (boolean isInboundReply,
                                                   InboundReplyContext inbReplyContext,
                                                   MessageExchange msg,
                                                   Exception cause) {
        if (isInboundReply) {
            try {                
                if (inbReplyContext.messageAckPending()) {
                    InboundReplyListener listener = 
                            inbReplyContext.getReplyListener();
                    if (msg.isTransacted()) {
                        Transaction tx = (Transaction)msg.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                        rollbackTransaction(msg, tx);
                    }
                    listener.onReply(msg, false, false);
                }
            } catch (Throwable ex) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{msg.getExchangeId(), 
                                          msg.getPattern(), 
                                          ex.getLocalizedMessage()}),
                         ex);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0750.ProcessMessageExchangeReplyFailed",
                             new Object[]{msg.getExchangeId(), 
                                          msg.getPattern(), 
                                          ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0750");
            }finally{
                removeMexId(msg);        
            }
        } 
        
        if (msg.getStatus() == ExchangeStatus.ACTIVE) {
            setErrorStatusAndSend (msg, cause);            
        }
    }

	private void removeMexId(MessageExchange msg) {
		Object msgId = msg.getProperty(MEXID);
		//Object mexId = mInboundExchanges.remove(msgId);
		//mInboundExchanges.remove(mexId);
		mInboundExchanges.remove(msgId);
	}
    
    private void setErrorStatusAndSend (MessageExchange msg, Exception cause) {
        if (cause != null) {
            msg.setError(cause);
        }
        
        try {
            msg.setStatus(ExchangeStatus.ERROR);

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "OutboundMessageProcessor_MXCH_SET_STATUS",
                         new Object[]{"ERROR", msg.getExchangeId(), msg.getPattern()});
            }

        } catch (MessagingException ex) {
            mLog.log(Level.SEVERE, 
                     mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                         new Object[]{msg.getExchangeId(),
                                      msg.getPattern(),
                                      ex.getLocalizedMessage()}),
                     ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0748.MessageExchangeSetStatusError",
                         new Object[]{msg.getExchangeId(),
                                      msg.getPattern(),
                                      ex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0748");
        }            

        try {
            // Send back exchange status
        	JMSBCContext.getRef().getChannel().send(msg);
        } catch (MessagingException mex) {
            mLog.log(Level.SEVERE, 
                     mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                        new Object [] {msg.getExchangeId(),
                                       msg.getPattern(),
                                       mex.getLocalizedMessage()}),
                     mex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0749.DeliveryChannelSendFailed",
                        new Object [] {msg.getExchangeId(),
                                       msg.getPattern(),
                                       mex.getLocalizedMessage()}), 
                                   JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                   null, 
                                   AlertsUtil.getServerType(),
                                   AlertsUtil.COMPONENT_TYPE_BINDING,
                                   NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                   NotificationEvent.EVENT_TYPE_ALERT,
                                   "JMSBC-E0749");
        }                
    }
    
    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            StringWriter out = null;
            if (msgSrc != null) {
                try {
                    TransformerFactory tFactory = TransformerFactory.newInstance();
                    Transformer trans = tFactory.newTransformer();
                    trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
                    trans.setOutputProperty(OutputKeys.INDENT, "yes");
                    trans.setOutputProperty(OutputKeys.METHOD, "xml");
                    trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                    out = new StringWriter();
                    StreamResult result = new StreamResult(out);
                    trans.transform(msgSrc, result);
                    out.flush();
                    out.close();
                } catch (Throwable t) {
                    ;;
                }
            }
            mLog.log (LogSupport.LEVEL_DEBUG, 
                      "OutboundMessageProcessor_NORMALIZED_MESSAGE_CONTENT_DUMP",
                      new Object[] {exchangeId, (out==null?"null":out.toString())});    
        }
    }        

	private void compareAndClose(Endpoint endpoint, Channel jmsChannel,
			SendChannel sendChannel) {
		if(sendChannel != jmsChannel){
			try {
				sendChannel.stop();
				sendChannel.close();
			} catch (Exception ex) {
			    mLog.log(Level.WARNING, 
			             mMessages.getString("JMSBC-W0701.ReplySenderCloseFailed",
			                 new Object [] {ex.getLocalizedMessage()}),
			             ex);
			    AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0701.ReplySenderCloseFailed",
			                 new Object [] {ex.getLocalizedMessage()}), 
			        JMSBindingComponent.SHORT_DISPLAY_NAME, 
			        endpoint.getServiceUnitID(), 
			        AlertsUtil.getServerType(),
			        AlertsUtil.COMPONENT_TYPE_BINDING,
			        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
			        NotificationEvent.EVENT_TYPE_ALERT,
			        "JMSBC-W0701");  
			}
			
		}
	}
	
	
}
