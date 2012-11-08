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
 * @(#)AsyncJBIProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.HttpSoapComponentContext;
import com.sun.jbi.httpsoapbc.InboundMessageProcessor;
import com.sun.jbi.httpsoapbc.MessageExchangeSupport;
import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.NormalizedMessageProperties;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.ReplyListener;
import com.sun.jbi.httpsoapbc.SoapNormalizer;
import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.LoggingMonitoringUtil;
import com.sun.jbi.httpsoapbc.util.StringUtil;
import com.sun.jbi.httpsoapbc.util.TransactionsUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.xml.ws.api.server.AsyncProvider;
import com.sun.xml.ws.api.server.AsyncProviderCallback;
import com.sun.xml.wss.SubjectAccessor;
import com.sun.xml.wss.XWSSecurityException;

import java.security.Principal;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.handler.MessageContext;
import javax.security.auth.Subject;

import net.java.hulp.measure.Probe;

/**
 * JAX-WS Provider implementation for use in Java SE. 
 *
 * Ties together JBI and JAX-WS: JAX-WS invokes this with the SOAPMessage, 
 * this then normalize the message and send it to the NMR. 
 * When the response comes back from the NMR the reply handling is triggered,
 * this denormalizes the message and calls back JAX-WS to proceed with the response.
 */
@javax.xml.ws.RespectBinding
@WebServiceProvider
@ServiceMode(Service.Mode.MESSAGE)
public class AsyncJBIProvider implements AsyncProvider<SOAPMessage>, ReplyListener {

    private static final String BASIC_AUTH_SUBJECT = "basicAuthSubject";
    private static final Messages mMessages =
        Messages.getMessages(AsyncJBIProvider.class);
    private static final Logger mLogger =
        Messages.getLogger(AsyncJBIProvider.class);    
    
    private Endpoint targetEndpoint;
    private HttpSoapBindingLifeCycle lifeCycle = null;
    private ReplyListener replyListener; 

    // measurement
    private Probe mFromInvokeToSendResponse= null;
    
    // message tracking and NDC logging
    private boolean ndcEnabled = false;
    private boolean mMonitorEnabled = false;
    private String mMode = LoggingMonitoringUtil.getMessageTrackingIDModeInbound();  
    
    /**
     * A mapping from the JBI message exchange ID to the request context
     */
    //TODO: find non-synchronized way of doing this
    Map exchangeIDToContext = Collections.synchronizedMap(new HashMap());    
    Map cbToMessageContext = Collections.synchronizedMap(new HashMap());
    
    public AsyncJBIProvider(Endpoint endpoint) {
        targetEndpoint = endpoint;
        ndcEnabled = LoggingMonitoringUtil.isNdcEnabled(endpoint);
        lifeCycle = (HttpSoapBindingLifeCycle) HttpSoapComponentContext.getInstance().getAssociatedLifeCycle();
        replyListener = this; 
        mMonitorEnabled = LoggingMonitoringUtil.isMonitorEnabled(targetEndpoint);
    }    

    /**
     * Process the Provider invoke
     * @param request the Provider reqeust
     * @return the provider response
     */
    public void invoke(SOAPMessage request, AsyncProviderCallback asyncProviderCallback, WebServiceContext webserviceContext) {
    	mFromInvokeToSendResponse = Probe.fine(getClass(),
                                               targetEndpoint.getUniqueName(),
                                               "processSOAPRequest");
        
        MessageContext context = webserviceContext.getMessageContext();
        if (mLogger.isLoggable(Level.FINE)) {
            String service = String.valueOf(context.get(MessageContext.WSDL_SERVICE));
            String port = String.valueOf(context.get(MessageContext.WSDL_PORT));
            String op = String.valueOf(context.get(MessageContext.WSDL_OPERATION));
            mLogger.log(Level.FINE, "Processing SOAPMessage received in AsyncProvider. "
                    + "[Service: " + service + "] [Port: " + port + "] [Operation: " + op + "]");
        }
                
        Transaction suspendedTransaction = null;        
        try {
            if (ndcEnabled ) {
                // "Push" the NDC context based on the setting in the NDC properties file
                Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[] {LoggingMonitoringUtil.SOLUTION_GROUP,
                                                                                                 LoggingMonitoringUtil.getSolutionGroup(targetEndpoint)});
            }
            suspendedTransaction = TransactionsUtil.suspendTransaction();
            InboundMessageProcessor anInboundProcessor = new InboundMessageProcessor(getProcessorSupport().normalizer, this);
            anInboundProcessor.setInput(request);
            if (suspendedTransaction != null) {
                anInboundProcessor.setTransaction(suspendedTransaction);
            }
            anInboundProcessor.setTargetEndpoint(targetEndpoint);
            anInboundProcessor.setMessageContext(context);
            anInboundProcessor.setSubject(readSubject(webserviceContext));
            Probe soapToNMRMeasurement = Probe.fine(getClass(),
                                                    targetEndpoint.getUniqueName(),
                                                    "soapToNMRSend");
            anInboundProcessor.setMeasurement(soapToNMRMeasurement);
            String exchangeID = anInboundProcessor.execute(asyncProviderCallback);
            if (suspendedTransaction != null) {
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Transaction suspended.. wait for reply notification.");
                }
                synchronized (asyncProviderCallback) {
                    asyncProviderCallback.wait();
                }
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Received reply notification.. resuming transaction");
                }
                TransactionsUtil.resumeTransaction(suspendedTransaction);
            }
        } catch (XWSSecurityException e) {
            String errmsg = mMessages.getString(
                    "HTTPBC-E00682.Failed_Acquire_Subject",
                    new Object[]{webserviceContext});
            mLogger.log(Level.SEVERE, errmsg, e);
            if (suspendedTransaction != null) {
                setTransactionRollbackOnly(suspendedTransaction, false);
            }
            asyncProviderCallback.sendError(e);
        } catch (SystemException e) {
            asyncProviderCallback.sendError(e);
        } catch (Exception e) {
            if (suspendedTransaction != null) {
                setTransactionRollbackOnly(suspendedTransaction, false);
            }
            asyncProviderCallback.sendError(e);
        } finally {
            if (ndcEnabled ) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[] {LoggingMonitoringUtil.SOLUTION_GROUP,
                                                                                                 LoggingMonitoringUtil.getSolutionGroup(targetEndpoint)});
            }
        }
       
    }

    private Subject readSubject(WebServiceContext webserviceContext)
            throws XWSSecurityException {
        Subject sub = SubjectAccessor.getRequesterSubject(webserviceContext);
        if(sub == null){
            sub =  (Subject)webserviceContext.getMessageContext().get(BASIC_AUTH_SUBJECT);
        }
        return sub;
    }

    /**
     * The inbound message processor will call us back in execute() once it knows the message exchange for the request.
     * @see ReplyListener
     */
    public void setMessageExchangeId(String messageExchangeId, Object clientContext) {
        exchangeIDToContext.put(messageExchangeId, clientContext);
    } 
    
    
    public void setMessageContextForCallback(Object clientContext, Object messageContext) {
        cbToMessageContext.put(clientContext, messageContext);   
    }
    
    /**
     * Removes a message exchange ID and its associated call back context
     * @see ReplyListener
     */
    public void removeMessageExchangeId(String messageExchangeId) {
        Object clientContext = exchangeIDToContext.remove(messageExchangeId);
        if (clientContext != null) {
            cbToMessageContext.remove(clientContext);
        }
    }    
    
    /**
     * Handle the reply available from JBI.
     */
    public void onReply(MessageExchange exchange) throws MessagingException {    
        SOAPMessage response = null;
        NormalizedMessage outMsg = null;
        NormalizedMessage inMsg = null;
        
        // message tracking
        String trackingId = null;
        
        // MEP is complete, we do not expect any further replies. Remove from MessageExchangeSupport.
        // This also clears the exchange ID --> ReplayListener entry in the static map.
        MessageExchangeSupport.removeReplyListener(exchange.getExchangeId());
        AsyncProviderCallback asyncProviderCallback = (AsyncProviderCallback) exchangeIDToContext.remove(exchange.getExchangeId());
        MessageContext messageContext = (MessageContext) cbToMessageContext.remove(asyncProviderCallback);
                
        Probe denormalizationMeasurement = null;
        
        Transaction transaction = (Transaction) exchange.getProperty(
             MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);

        try {
            if (ndcEnabled ) {
                // "Push" the NDC context based on the setting in the NDC properties file
                Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[] {LoggingMonitoringUtil.SOLUTION_GROUP,
                                                                                                  LoggingMonitoringUtil.getSolutionGroup(targetEndpoint)});
            }
            
            if (exchange.getStatus() == ExchangeStatus.ACTIVE) {
                // Next step in the message exchange ping-pong game.
                // Prcoess the reponse - whether out message or fualt
                if (ExchangePattern.isInOut(exchange)) {
                    InOut inout = (InOut) exchange;
                    inMsg = inout.getInMessage();
                    NormalizedMessage outMsgOrFault = inout.getOutMessage();
            
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Processing-inbound-response", targetEndpoint);
                    }

                    if (outMsgOrFault == null) {
                        Fault aFault = exchange.getFault(); 
                        outMsgOrFault = aFault;
                    }

                    // TODO: it should be possible to disallow/disable this completely for compliance reasons
                    if (mLogger.isLoggable(Level.FINE)) {
                        if (outMsgOrFault != null) {
                            DebugLog.debugLog(mLogger, Level.FINE, "Denormalizing received msg", outMsgOrFault.getContent());
                        } else {
                            mLogger.log(Level.FINE, "Message received is empty.");
                        }
                    }

                    Map nameToMeta = targetEndpoint.getOperationNameToMetaData();
                    String operation = exchange.getOperation().getLocalPart();
                    OperationMetaData operationMetaData = (OperationMetaData) nameToMeta.get(operation);
                    if (operationMetaData == null) {
                        throw new MessagingException(mMessages.getString("HTTPBC-E00667.No_opmeta_for_operation", exchange.getOperation()));
                    }

                    Denormalizer denormalizer = getProcessorSupport().denormalizer;
                    denormalizationMeasurement = Probe.info(getClass(),
                                                            targetEndpoint.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_DENORMALIZATION);
                    response = (SOAPMessage) denormalizer.denormalize(outMsgOrFault, exchange, response, operationMetaData);
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        try {
                            LoggingMonitoringUtil.setCheckpoint(targetEndpoint, trackingId, "Denormalized-SOAP-response", response.getSOAPPart().getEnvelope());
                        } catch (Exception e) {
                            if (mLogger.isLoggable(Level.WARNING)) {
                                mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                            }
                        }
                    }
                } else if (ExchangePattern.isInOnly(exchange)) {
                    // For one-way exchanges we do not have to provide a reponse, just call JAX-WS back with null
                    InOnly inonly = (InOnly)exchange;
                    inMsg= inonly.getInMessage();
                    response = null;
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Processing-inbound-response", targetEndpoint);
                    }
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Calling back JAX-WS asynchronously to proceed with response");
                }
                
                // measuring total time taken from receving the SOAP request to send back the response to client
                if (mFromInvokeToSendResponse != null) {
                    mFromInvokeToSendResponse.end();
                }
                
                if (transaction != null) {
                    synchronized (asyncProviderCallback) {
                        asyncProviderCallback.notify();
                    }
                }
                if (mMonitorEnabled) {
                    trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                    LoggingMonitoringUtil.setCheckpoint(trackingId, "Sending-SOAP-response", targetEndpoint);
                }
                asyncProviderCallback.send(response);
                
            } else if (exchange.getStatus() == ExchangeStatus.DONE) {
                // The game is over; the partner component
                // is closing the MEP, and so should I.
                // For the following MEPs the following are true:
                //
                // Pattern  me.getRole() me Invariant
                // -------  ------       ------------
                // In       CONSUMER     getInMessage() != null && getError() == null
                // In-out   PROVIDER     getInMessage() != null && 
                //                       (getOutMessage() != null xor getFaultMessage() != null) &&
                //                       getError() == null
                //
                // (the invariant conditions with optional responses are
                // more complex.)
                if (ExchangePattern.isInOnly(exchange)) {
                    if (transaction != null) {
                        synchronized (asyncProviderCallback) {
                            asyncProviderCallback.notify();
                        }
                    }
                    // One-way invoke successful, report 202 accepted
                    asyncProviderCallback.send(null);
                }
            } else if (exchange.getStatus() == ExchangeStatus.ERROR) {
                // The game is over; the partner component 
                // is abruptly closing the MEP, and so should I.
                // ME properties will reflect
                // the on-going state of the exchange before it
                // was ending by the partner abruptly.
                //
                // Pattern  me.getRole() me Invariant
                // -------  ------------ ------------
                // In       CONSUMER     getInMessage() != null
                // In-out   CONSUMER     getInMessage() != null
                // In-out   PROVIDER     getInMessage() != null &&
                //                       (getOutMessage() != null xor getFaultMessage() != null )
                
                RedeliveryConfig redeliveryConfig = targetEndpoint.getRedeliveryConfiguration();
                Exception errorDetail = null;
                if (exchange.getError() != null) {
                    errorDetail = exchange.getError();
                } else {
                    // Provider did not give details about the error, can only generate a very generic failure
                    // TODO: i18n
                    errorDetail = new Exception("Provider for " + targetEndpoint.getServiceName() + " " + targetEndpoint.getEndpointName() + " responded with an error status.");
                }
                
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Sending error back to the client via AsyncProviderCallback. Error detail is: " + errorDetail);            
                }
                // For inout, convert to our fault format. We may want to consider if we leave this to JAX-WS.
                if (ExchangePattern.isInOut(exchange)) {
                    InOut inout = (InOut) exchange;
                    inMsg = inout.getInMessage();
                    Denormalizer denormalizer = getProcessorSupport().denormalizer;
                    Failure onFailureOption = (redeliveryConfig != null)? redeliveryConfig.getFailure() : null;
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Preparing-SOAP-fault-after-exhausting-max-retries", targetEndpoint);
                    }
                    if (redeliveryConfig != null && (onFailureOption == Failure.suspend || onFailureOption == Failure.error)) {
                        // if we get here when redelivery is configured, that means retry attempts have been
                        // exhausted and the error still cannot be resolved.
                        // we will add appropriate details in the SOAP fault.
                        String faultDetail = getRedeliveryFaultDetail(exchange, redeliveryConfig);
                        response = (SOAPMessage) denormalizer.denormalizeError(exchange, faultDetail,targetEndpoint);
                    } else {
                        response = (SOAPMessage) denormalizer.denormalizeError(exchange, response,targetEndpoint);
                    }
                    
                    if (transaction != null) {
                    	if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "Transaction is not null, about to rollback the transaction...");            
                        }
                        setTransactionRollbackOnly(transaction, true);
                        synchronized (asyncProviderCallback) {
                            asyncProviderCallback.notify();
                        }
                    }
                    
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "About to send a fault back to the client via AsyncProviderCallback");            
                    }
                    
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Sending-SOAP-fault", targetEndpoint);
                    }
                    asyncProviderCallback.send(response);
                } else if (ExchangePattern.isInOnly(exchange)) {
                    InOnly inonly = (InOnly) exchange;
                    inMsg = inonly.getInMessage();
                    if (mMonitorEnabled) {
                        trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null)? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") + 
                                     ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null)? (String)inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Sending-HTTP-error-status", targetEndpoint);
                    }
                    // set RM_ACK property accordingly, such that no RM acknowledgment will be sent back here
                    // also set the HTTP status code to 500
                    messageContext.put("RM_ACK", "false");
                    messageContext.put(MessageContext.HTTP_RESPONSE_CODE, 500);
                    asyncProviderCallback.send(null);
                    
                    /** Following is not relevant: per BP 1.1 spec, no SOAP envelope should go back to the client in case of one-way operations.
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Sending error back to the client via AsyncProviderCallback. Error detail is: " + errorDetail);            
                    }
                    if (redeliveryConfig != null) {
                    	// if we get here when redelivery is configured, that means retry attempts have been
                        // exhausted and the error still cannot be resolved.
                        // we will add appropriate details in the SOAP fault.
                    	String faultDetail = getRedeliveryFaultDetail(exchange, redeliveryConfig);
                        if (transaction != null) {
                            setTransactionRollbackOnly(transaction, true);
                            synchronized (asyncProviderCallback) {
                                asyncProviderCallback.notify();
                            }
                        }
                        asyncProviderCallback.sendError(new Exception(faultDetail, errorDetail));
                    } else {
                        if (transaction != null) {
                            setTransactionRollbackOnly(transaction, true);
                            synchronized (asyncProviderCallback) {
                                asyncProviderCallback.notify();
                            }
                        }
                        asyncProviderCallback.sendError(errorDetail);
                    }
                    **/
                }
            }
        } catch (MessagingException e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-E00799.Denormalize_fail"), e);
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction, true);
                synchronized (asyncProviderCallback) {
                    asyncProviderCallback.notify();
                }
            }
            asyncProviderCallback.sendError(e);
            throw e;
        } catch (Exception e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-E00800.Failed_to_process_reply_from_jbi"), e);
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction, true);
                synchronized (asyncProviderCallback) {
                    asyncProviderCallback.notify();
                }
            }
            asyncProviderCallback.sendError(e);
            throw new MessagingException(mMessages.getString("HTTPBC-E00800.Failed_to_process_reply_from_jbi"), e);
        } finally {
            if (denormalizationMeasurement != null) {
                denormalizationMeasurement.end();
            }
            targetEndpoint.decrementPendingExchangeReplies();
            if (this.mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Decremented pending message exchange replies; endpoint '"+targetEndpoint.getUniqueName()+"'");
            }
            if (ndcEnabled ) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[] {LoggingMonitoringUtil.SOLUTION_GROUP,
                                                                                                 LoggingMonitoringUtil.getSolutionGroup(targetEndpoint)});
            }
        }
    }


    String getRedeliveryFaultDetail(MessageExchange exchange, RedeliveryConfig redeliveryConfig) {
    	StringBuffer faultDetail = new StringBuffer();
        String lastError = (String)exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING);
        if (lastError != null) {
            lastError = StringUtil.escapeXML(lastError);
        }
        
        if (redeliveryConfig != null) {
            Failure onFailureOption = redeliveryConfig.getFailure();
            faultDetail.append("Maximum redelivery attempts has been exhausted yet the exchange error persisted. ");     // no I18N
            switch (onFailureOption) {
                case suspend:
                    faultDetail.append("Endpoint with service name '" + targetEndpoint.getServiceName() + "' and endpoint name '" + 
                                       targetEndpoint.getEndpointName() + "' will be suspended per the redelivery on-failure option.");
                case redirect:
                    if (ExchangePattern.isInOnly(exchange)) {
                        faultDetail.append("The attempt to route the exchange to the error endpoint specified in redelivery configuration also failed.");
                    }
                    break;
                case delete:    // not much can be done and in theory, we should not reach here anyway.
                    if (ExchangePattern.isInOnly(exchange)) {
                        faultDetail.append("The request message will be silently deleted.");
                    }
                    break;    
                case error:   
                    if (lastError != null) { 
                        faultDetail.append("Error causing the last redelivery attempt to fail is: " + lastError);
                    }
                    break;
                default:
                    break;
            } 
        }
        
        return faultDetail.toString();
    }
    
    /**
     * Workaround for issue 180 (https://jax-ws.dev.java.net/issues/show_bug.cgi?id=180) where the 
     * SOAPMessage does not have the transport-specific headers we need.  Specifically, we need the SOAPAction
     * header.
     *
     */
    public SOAPMessage soapActionHeaderWorkaround(SOAPMessage request, WebServiceContext webserviceContext) {
        try {
            MessageContext messageContext = webserviceContext.getMessageContext();
            Map headers = (Map)messageContext.get(MessageContext.HTTP_REQUEST_HEADERS);
            
            // The headers are stored with the key all in lower case
            List soapActionHeaders = (List)headers.get("SOAPAction".toLowerCase());
            if (soapActionHeaders != null) {
                // Just grab the first item fromt he list.
                request.getMimeHeaders().setHeader("SOAPAction", (String)soapActionHeaders.get(0));
                request.saveChanges();
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Unable to migrate SOAPAction URI from MessageContext to SOAPMessage",
                        ex);
        }
        return request;
    }
        
    //TODO: BEST TO REMOVE ALL THREAD LOCAL ARTIFACTS    
    
    /**
     * Get the thread specific processor support
     * Beware: Do not use the processor support instances in a different thread than 
     * the one calling getProcessorSupport. 
     */
    ProcessorSupport getProcessorSupport() throws MessagingException {
        // Get the processor support instances associated with the thread if present, create if not.
        ProcessorSupport currentProcSupport = (ProcessorSupport) processorSupport.get();
        if (currentProcSupport == null) {
            currentProcSupport = new ProcessorSupport();
            currentProcSupport.normalizer = new JAXWSNormalizer();
            currentProcSupport.denormalizer = new JAXWSDenormalizer();
            currentProcSupport.inboundProcessor = new InboundMessageProcessor(currentProcSupport.normalizer, replyListener);
            processorSupport.set(currentProcSupport);
        }
        return currentProcSupport;
    }
    
    /**
     * Holds instances that are not thread safe
     */    
    private static ThreadLocal processorSupport = new ThreadLocal();

    /**
     * Holds instances that are not thread safe
     */
    static class ProcessorSupport {
        Normalizer normalizer;
        Denormalizer denormalizer;
        InboundMessageProcessor inboundProcessor;        
    }    
    
    private void setTransactionRollbackOnly (Transaction transaction, boolean suspend) {
        boolean txResumed = false;
        if (transaction != null) {
            txResumed = TransactionsUtil.resumeTransaction(transaction);
        }
        if (txResumed) {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Calling setRollbackOnly on transaction [" + transaction + "]");
            }
            TransactionsUtil.setRollbackOnlyOnTransaction(transaction);
            if (suspend) {
                try {
                    TransactionsUtil.suspendTransaction();
                } catch (SystemException se) {
                    // really no recourse for this as tx state is probably messed up
                    // Note - TransactionsUtil logs exception
                }
            }
        }
    }    
}
