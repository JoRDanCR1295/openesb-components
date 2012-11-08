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
 * @(#)InboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

/*
 * This class might need to be refactored if this is 
 * going to be implemented for SQLSE since SQLSE does not implement 
 * polling there would not be an Inbound thread
*/
public class ReplyListenerImpl implements ReplyListener{
	

	private EndpointBean targetEndpoint;
    private JDBCBindingLifeCycle lifeCycle = null;
    private SQLSELifeCycle seLifeCycle = null;
    private ReplyListener replyListener; 


	/**
     * A mapping from the JBI message exchange ID to the request context
     */
    //TODO: find non-synchronized way of doing this
    Map exchangeIDToContext = Collections.synchronizedMap(new HashMap());    

	public ReplyListenerImpl(EndpointBean targetEndPoint){
		this.targetEndpoint = targetEndpoint;
		lifeCycle = (JDBCBindingLifeCycle) JDBCComponentContext.getInstance().getAssociatedLifeCycle();
		replyListener = this;
	}

	/*
	*  currently this makes no sense since we do not have polling for SQLSE
	*  this constructor is useful if and only if we use polling in SQL SE
	*/
	public ReplyListenerImpl(EndpointBean targetEndPoint, boolean se){
		this.targetEndpoint = targetEndpoint;
		//seLifeCycle = (JDBCBindingLifeCycle) JDBCComponentContext.getInstance().getAssociatedLifeCycle();
		replyListener = this;
	}

	/*	
	 * Method asynchronously invoked by the framework once a reply to a message exchange is available 
     */
    public void onReply(MessageExchange exchange) throws MessagingException{    

		// MEP is complete, we do not expect any further replies. Remove from MessageExchangeSupport.
        // TODO: is this remove really needed? If so, document why            
		MessageExchangeSupport.removeReplyListener(exchange.getExchangeId());

		/*SOAPMessage response = null;
        NormalizedMessage outMsg = null;
        
        
        AsyncProviderCallback asyncProviderCallback = (AsyncProviderCallback) exchangeIDToContext.remove(exchange.getExchangeId());
                
        Probe denormalizationMeasurement = null;
        
        boolean transactionResumed = false;
        Transaction transaction = (Transaction) exchange.getProperty(
             MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);

        try {
            if (exchange.getStatus() == ExchangeStatus.ACTIVE) {
                // Next step in the message exchange ping-pong game.
                // Prcoess the reponse - whether out message or fualt
                if (ExchangePattern.isInOut(exchange)) {
                    InOut inout = (InOut) exchange;
                    NormalizedMessage outMsgOrFault = inout.getOutMessage();

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
                } else if (ExchangePattern.isInOnly(exchange)) {
                    // For one-way exchanges we do not have to provide a reponse, just call JAX-WS back with null
                    response = null;
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Calling back JAX-WS asynchronously to proceed with response");
                }
                //response = saajWorkAround(response);
                transactionResumed = TransactionsUtil.resumeTransaction(transaction);
                
                // measuring total time taken from receving the SOAP request to send back the response to client
                if (mFromInvokeToSendResponse != null) {
                    mFromInvokeToSendResponse.end();
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
                    // One-way invoke successful, report 202 accepted
                    transactionResumed = TransactionsUtil.resumeTransaction(transaction);
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
                Exception errorDetail = null;
                if (exchange.getError() != null) {
                    errorDetail = exchange.getError();
                } else {
                    // Provider did not give details about the error, can only generate a very generic failure
                    // TODO: i18n
                    errorDetail = new Exception("Provider for " + targetEndpoint.getServiceName() + " " + targetEndpoint.getEndpointName() + " responded with an error status.");
                }
                
                // For inout, convert to our fault format. We may want to consider if we leave this to JAX-WS.
                if (ExchangePattern.isInOut(exchange)) {
                    Denormalizer denormalizer = getProcessorSupport().denormalizer;
                    response = (SOAPMessage) denormalizer.denormalizeException(errorDetail, response);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Sending a fault back to the client via AsyncProviderCallback");            
                    }
                    //response = saajWorkAround(response);
                    transactionResumed = TransactionsUtil.resumeTransaction(transaction);
                    asyncProviderCallback.send(response);
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Sending error back to the client via AsyncProviderCallback. Error detail is: " + errorDetail);            
                    }
                    transactionResumed = TransactionsUtil.resumeTransaction(transaction);
                    asyncProviderCallback.sendError(errorDetail);
                }
            }
            
        } catch (Exception e) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Caught failure in response processing, sending error. ", e);
            }
            asyncProviderCallback.sendError(e);
        } finally {
            if (denormalizationMeasurement != null) {
                denormalizationMeasurement.end();
            }
            if (!transactionResumed) {
                try {
                    TransactionsUtil.resumeTransaction(transaction);
                } catch (Exception e) {
                    String errmsg = mMessages.getString(
                            "HTTPBC-E00681.Transaction_resume_failed",
                            transaction.toString());
                    mLogger.log(Level.SEVERE, errmsg, e);
                }
            }
        }*/

	}
    
	/**
     * The framwork will set the corresponding message exchange
     * once it initialized the message exchange.
     * @param messageExchangeId the exchange id of the message exchange initialized
     * @param clientContext returns the client provided object associated with the request
     */
    public void setMessageExchangeId(String messageExchangeId, Object clientContext){
		exchangeIDToContext.put(messageExchangeId, clientContext);
	}

}