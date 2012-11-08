/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

import java.net.URI;
import java.util.Map;
import java.util.Iterator;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.dcombc.DCOMException;
import com.sun.jbi.dcombc.dcom.Channel;
import com.sun.jbi.dcombc.dcom.ChannelManager;
import com.sun.jbi.dcombc.Endpoint.EndpointState;
import com.sun.jbi.dcombc.Endpoint.EndpointType;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;
import com.sun.jbi.dcombc.extensions.DCOMMessage;
import com.sun.jbi.dcombc.extensions.DCOMConstants;
import com.sun.jbi.dcombc.extensions.DCOMAddress;
import com.sun.jbi.dcombc.util.DCOMUtil;
import com.sun.jbi.nms.exchange.ExchangePattern;


/**
 * Process replies/requests received from the SE.
 *
 * @author Chandrakanth Belde
 */
public class  OutboundMessageProcessor implements Runnable {

    private static final Messages mMessages = Messages.getMessages(OutboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);

    private Collection mServiceUnits;

    private DeliveryChannel mChannel;

    private ChannelManager dcomChannelMgr;

    private Map mInboundExchanges;

    private Object mMonitor;

    private DCOMDenormalizer dcomDenormalizer;

    private DCOMNormalizer dcomNormalizer;

    private ComponentContext mComponentContext;

    private MessageExchange msgExchange;

    public OutboundMessageProcessor(ComponentContext componentContext, 
    								DeliveryChannel chnl, 
    								Collection serviceUnits,
    								ChannelManager mChannelMgr, 
    								Map inboundMessageExchanges, 
    								MessageExchange mExchange) {
        mComponentContext = componentContext;
        mChannel = chnl;
        mServiceUnits = serviceUnits;
        dcomChannelMgr = mChannelMgr;
        mInboundExchanges = inboundMessageExchanges;
        mMonitor = new Object();
        msgExchange = mExchange;
        dcomDenormalizer = DCOMDenormalizer.getInstance();
        dcomNormalizer = DCOMNormalizer.getInstance();
    }

    /**
     * Main entry point to execute this in a thread. Calls accept on the JBI NMR and process the
     * MessageExchange it receives. Delegates the real work of processing the MessageExchange to
     * <code>execute</code>
     */
    public void run() {
        mLogger.log(Level.INFO, "OutboundMessageProcessor.SERVICE_LOOP_ENTER");
        try {
            if (msgExchange != null) {
                    mLogger.log(Level.INFO, "OutboundMessageProcessor.NMR_ACCEPT_MXCH",
                                    new Object[] { msgExchange.getExchangeId() });

                    Map inboundMessageExchanges = InboundMessageProcessor.getInboundExchanges();

                    // Look for the DCOM Channel from Channel Manager
                    ServiceEndpoint serviceEndpoint = msgExchange.getEndpoint();
                    QName operation = msgExchange.getOperation();
                    int endpointType;
                    if (inboundMessageExchanges.containsKey(msgExchange.getExchangeId()) && !isRequest(msgExchange)) {
                            // processing inbound reply
                            endpointType = EndpointType.INBOUND;
                    } else {
                            // processing outbound dcom request
                            endpointType = EndpointType.OUTBOUND;
                    }

                    Channel dcomChannel = dcomChannelMgr.lookup(serviceEndpoint.getServiceName(),
                                    serviceEndpoint.getEndpointName(), endpointType, operation);

                    execute(msgExchange, dcomChannel, endpointType);

                    mLogger.log(Level.INFO, "OutboundMessageProcessor.NMR_COMPLETED_MXCH",
                                    new Object[] { msgExchange.getExchangeId() });
            }
        } catch (Throwable ex) {
            mLogger.log(Level.SEVERE, "OutboundMessageProcessor.UNEXPECTED_ERROR",
                    new Object[] { DCOMUtil.getStackTraceAsString(ex) });
        }

        mLogger.log(Level.INFO, "OutboundMessageProcessor.SERVICE_LOOP_EXIT");
    }

    /**
     * Process the message exchange
     */
    public void execute(MessageExchange msgExchange, Channel dcomChannel, int endpointType) {

        String exchangeId = msgExchange.getExchangeId();
        boolean inbound = endpointType == EndpointType.INBOUND;
        ReplyListener listener = null;
        if (inbound) {
            listener = (ReplyListener) mInboundExchanges.get(exchangeId);
            long invocationTime = listener.getRequestInvocationTime();
            if (mLogger.isLoggable(Level.INFO)) {
                long difference = System.currentTimeMillis() - invocationTime;
                mLogger.log(Level.INFO, "OutboundMessageProcessor.MXCH_RESPONSE_RECVD", new Object[] { exchangeId,
                        new Long(difference) });
            }
        }

        Endpoint destination = null;
        Iterator it = mServiceUnits.iterator();
        while (it.hasNext()) {
            Iterator it2 = ((ServiceUnit) it.next()).getEndpoints().iterator();
            while (it2.hasNext()) {
                Endpoint endpoint = (Endpoint) it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();
                if (endpoint.getServiceName().equals(serviceName) && endpoint.getEndpointName().equals(endpointName)) {
                    destination = endpoint;
					break;
                }
            }
        }

        if (destination == null) {
            mLogger.log(Level.SEVERE, "OutboundMessageProcessor.ENDPOINT_UNDEFINED", new Object[] {
                    msgExchange.getEndpoint().toString(), msgExchange.getExchangeId() });

            setErrorStatusAndSend(msgExchange);

            return;
        }

        int state = destination.getState();
        if (!(state == EndpointState.RUNNING)) {
            String strState = (state == EndpointState.STOPPED ? "STOPPED" : "SHUTDOWN");

            // If the endpoint is not in the RUNNING state
            // (i.e. is stopped or shutdown), ignore the message
            mLogger.log(Level.SEVERE, "OutboundMessageProcessor.ENDPOINT_NON_RUNNING_STATE", new Object[] {
                    destination.getEndpointName(), strState, msgExchange.getExchangeId() });

            setErrorStatusAndSend(msgExchange);
        } else {
            URI pattern = msgExchange.getPattern();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "OutboundMessageProcessor.MXCH_PATTERN", new Object[] {
                        msgExchange.getExchangeId(), msgExchange.getPattern().toString() });
            }
            switch (ExchangePattern.valueOf(msgExchange)) {
                case IN_OUT:
                    if (inbound) {
                        /*processRequestReplyInbound((InOut) msgExchange,
                                                    destination, 
                                                    listener); */
                    } else {
                        processRequestReplyOutbound((InOut) msgExchange, 
                                                    destination, 
                                                    dcomChannel);
                    }
                    break;
                case IN_ONLY:
                    if (inbound) {
                            /*processOneWayInbound((InOnly) msgExchange, 
                                                 destination, 
                                                 listener);*/
                        } else {
                            /*processOneWayOutbound((InOnly) msgExchange, 
                                                  destination, 
                                                  dcomChannel);*/
                        }
                        break;
                default:                    
                    mLogger.log(Level.SEVERE, "OutboundMessageProcessor.MXCH_PATTERN_INVALID",
                            new Object[] { msgExchange.getExchangeId() });
                    break;
            }
        }            
    }

    private boolean isRequest(MessageExchange msgX) {
        boolean request = false;
        if (msgX instanceof InOut) {
            InOut inout = (InOut) msgX;
            request = inout.getOutMessage() == null && inout.getFault() == null
                    && inout.getStatus() == ExchangeStatus.ACTIVE;
        } else if (msgX instanceof InOnly) {
            InOnly inonly = (InOnly) msgX;
            request = inonly.getFault() == null && inonly.getStatus() == ExchangeStatus.ACTIVE;
        }
        return request;
    }

    /*private TransactionManager getTransactionManager() {
        return (TransactionManager) mComponentContext.getTransactionManager();
    }*/

    /**
     * Process the reply for a request/reply request originating from this BC.
     */
    /*private void processRequestReplyInbound(InOut inout, Endpoint destination, ReplyListener listener) {

        boolean success = false;
        
        if (inout.getOutMessage() != null) {
            destination.getEndpointStatus().incrementReceivedReplies();
        } else if (inout.getStatus().equals(ExchangeStatus.ERROR)) {
            destination.getEndpointStatus().incrementReceivedErrors();
        } else if (inout.getError() != null) {
            // for now handle exception as error
            destination.getEndpointStatus().incrementReceivedErrors();
        } else if (inout.getFault() != null) {
            // for now handle fault as error
            destination.getEndpointStatus().incrementReceivedErrors();            
        }
        try {
            success = listener.onReply(inout);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, 
                     "OutboundMessageProcessor.EXCEPTION_PROCESSING_REPLY",
                     new Object [] {inout.getExchangeId(), ex});
        }
        try {
            if (success) {
                inout.setStatus(ExchangeStatus.DONE);
                mLogger.log(Level.INFO, 
                             "OutboundMessageProcessor.SET_EXCHANGE_STATUS_DONE",
                             new Object [] {inout.getExchangeId()});
            } else {
                inout.setStatus(ExchangeStatus.ERROR);
                mLogger.log(Level.SEVERE, 
                         "OutboundMessageProcessor.SET_EXCHANGE_STATUS_ERROR",
                         new Object [] {inout.getExchangeId()});
            }
            mChannel.send(inout);
            if (destination != null) {
                if (success) {
                    destination.getEndpointStatus().incrementSentDones();
                } else {
                    destination.getEndpointStatus().incrementSentErrors();
                }
            }
        } catch (MessagingException ex) {
            mLogger.log(Level.SEVERE, 
                    "OutboundMessageProcessor.MXCH_SET_STATUS_ERROR",
                     new Object[]{"in-out",
                                  inout.getExchangeId(),
                                  ex.getLocalizedMessage()});
        }         


    }*/
    
    /**
     * 
     * @param inout
     * @param destination
     * @param dcomChannel
     */
    private void processRequestReplyOutbound(InOut inout, Endpoint destination, Channel dcomChannel) {
    	
    	Object replyContextKey = null;
        if (inout.getStatus() == ExchangeStatus.DONE) {            
        	destination.getEndpointStatus().incrementReceivedDones();                                    
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
        	destination.getEndpointStatus().incrementReceivedErrors();
        } else { 
        	destination.getEndpointStatus().incrementReceivedRequests();
            
        }
        //
        // InOut exchange process follows:
        // Send request to DCOM operation, return value will be received synchronously.
        //
            
        try {
            ServiceEndpoint serviceEndpoint = inout.getEndpoint();
            QName opQname = inout.getOperation();
            DCOMOperation dcomOperation = 
                    (DCOMOperation)destination.getDCOMOperations()
                                          .get(opQname);
            
           
            DCOMAddress dcomAddress = destination.getDCOMAddress();
            String msgXchangeID = inout.getExchangeId();
            NormalizedMessage normalizedMsg = inout.getInMessage();
            String inputVal =  dcomDenormalizer.denormalize(dcomChannel, normalizedMsg, opQname);

            mLogger.log(Level.INFO, "OutboundMessageProcessor.END_DENORMALIZE_MESSAGE");
            mLogger.log(Level.INFO, "OutboundMessageProcessor.DCOM_SENDING_ARG_TO_METHOD",
                    new Object[] { dcomOperation.getMethod()});

        	String dcomRetVal = dcomChannel.invoke(inputVal);
	            
            if (dcomRetVal == null) {
                String errMsg = mMessages.getString("OutboundMessageProcessor_NO_REPLY_RECEIVED",
                            new Object[]{inout.getExchangeId()});
                throw new Exception (errMsg); // throw and catch
            } else {
                // Got a reply dcom response/acknowledgement message, send back to consumer
                NormalizedMessage outNormalizedMsg = dcomNormalizer.normalize(inout,
                                                                             opQname,
                                                                             false,
                                                                             destination,
                                                                             dcomRetVal);
                inout.setOutMessage(outNormalizedMsg);
                mLogger.log(Level.INFO,
                            "OutboundMessageProcessor_SENDING_BACK_EXCHANGE_WITH_OUTPUT",
                            new Object[]{inout.getExchangeId()});                    
            }
	     } catch (Exception ex)  {           
	            destination.getEndpointStatus().incrementSentErrors();            	            
	            mLogger.log(Level.SEVERE, 
	                     "OutboundMessageProcessor_MXCH_ERROR",
	                     new Object [] {inout.getExchangeId(), 
	                                    DCOMUtil.getStackTraceAsString(ex)});
	                                
	            // setError should set status to error, but just in case...
	            try {
	                inout.setStatus(ExchangeStatus.ERROR);
	                    mLogger.log(Level.INFO,
	                            "OutboundMessageProcessor_MXCH_SET_STATUS",
	                             new Object[]{"ERROR", inout.getExchangeId()});
	
	            } catch (MessagingException ex2) {
	                mLogger.log(Level.WARNING, 
	                        "OutboundMessageProcessor_MXCH_SET_STATUS_ERROR",
	                         new Object[]{"in-only",
	                                      inout.getExchangeId(),
	                                      DCOMUtil.getStackTraceAsString(ex2)});
	            }                    
	        } finally {
	            // DCOM "invoke" failed so send back fault or error to NMR
	            try {
	                // Send back exchange fault or error
	                mComponentContext.getDeliveryChannel().send(inout);
	            } catch (Exception mex2) {
	                mLogger.log(Level.SEVERE, 
	                        "OutboundMessageProcessor_NMR_SEND_STATUS_ERROR",
	                        new Object [] {inout.getExchangeId(), 
	                                       DCOMUtil.getStackTraceAsString(mex2)});
	            }
	        }
    }

    /**
     * 
     * @param inonly
     * @param destination
     * @param listener
     */
    /*private void processOneWayInbound(InOnly inonly, Endpoint destination, ReplyListener listener) {
        if (inonly.getStatus().equals(ExchangeStatus.DONE)) {
            destination.getEndpointStatus().incrementReceivedDones();
        } else if (inonly.getStatus().equals(ExchangeStatus.ERROR)) {
            destination.getEndpointStatus().incrementReceivedErrors();
        }

        listener.onReply(inonly);
    }*/
    
    /**
     * 
     * @param inonly
     * @param destination
     * @param dcomChannel
     */
    /*private void processOneWayOutbound(InOnly inonly, Endpoint destination, Channel dcomChannel) {
        boolean success = false;
        Transaction tx = null;
        try {
            QName opQname = inonly.getOperation();
            DCOMOperation dcomOperation = (DCOMOperation) destination.getDCOMOperations().get(opQname);

            String transaction = dcomOperation.getTransaction();
            boolean xa = transaction.equals("XATransaction") ? true : false;

            // Increment received requests on endpoint (bc is provider)
            destination.getEndpointStatus().incrementReceivedRequests();

            // Get the Normalized Message from MessageExchange
            NormalizedMessage normalizedMsg = inonly.getInMessage();

            mLogger.log(Level.INFO, "OutboundMessageProcessor.BEGIN_DENORMALIZE_MESSAGE");

            // Denormalize from NMS message to DCOM "request" message
            String payload = dcomDenormalizer.denormalize(dcomChannel, normalizedMsg, opQname);

            mLogger.log(Level.INFO, "OutboundMessageProcessor.END_DENORMALIZE_MESSAGE");
            mLogger.log(Level.INFO, "OutboundMessageProcessor.DCOM_SENDING_TO_DESTINATION",
                    new Object[] { dcomOperation.getMethod()});

            if (xa && inonly.isTransacted()) {
                // for XA, just enlist the XAResource to the transaction
                // started by the initiator of the message exchange,
                // transaction will be committed or rolled back by the initiator.
                tx = (Transaction) inonly.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);

                // we have to resume the suspended transaction
                resumeThreadTx(tx);
            }

            try {
                if (tx != null) {
                    // enlist the dcom xaresource and send message to
                    // microsoft message server
                    dcomChannel.invoke(payload, tx);
                } else {
                    dcomChannel.invoke(payload);
                }
                break;
            } catch (Exception ex) {
            }

            if (tx != null) {
                try{
                    // we have to suspend the tx before sending it across NMR.
                    suspendThreadTx(tx);
                } catch(Exception ex){
                    mLogger.log(Level.INFO,"OutboundMessageProcessor.SUSPEND_FAILED", new Object[] { "in-only",
                            inonly.getExchangeId(), DCOMUtil.getStackTraceAsString(ex) });
                    throw new DCOMException(ex);
                }
            }

            inonly.setStatus(ExchangeStatus.DONE);

            mLogger.log(Level.INFO, "OutboundMessageProcessor.MXCH_SET_STATUS", new Object[] { "DONE",
                    inonly.getExchangeId() });

            destination.getEndpointStatus().incrementSentDones();

            success = true;
        } catch (Exception e) {

            if (tx != null) {
                try{
                    // we have to suspend the tx before sending it across NMR.
                    suspendThreadTx(tx);
                } catch(Exception ex){
                    mLogger.log(Level.INFO,"OutboundMessageProcessor.SUSPEND_FAILED", new Object[] { "in-only",
                            inonly.getExchangeId(), DCOMUtil.getStackTraceAsString(ex) });
                }
            }

            destination.getEndpointStatus().incrementSentErrors();
            mLogger.log(Level.WARNING, "OutboundMessageProcessor.MXCH_ERROR", new Object[] { inonly.getExchangeId(),
                    DCOMUtil.getStackTraceAsString(e) });
            inonly.setError(e);
            // setError should set status to error, but just in case...
            try {
                inonly.setStatus(ExchangeStatus.ERROR);

                mLogger.log(Level.INFO, "OutboundMessageProcessor.MXCH_SET_STATUS", new Object[] { "ERROR",
                        inonly.getExchangeId() });

            } catch (MessagingException ex) {
                mLogger.log(Level.WARNING, "OutboundMessageProcessor.MXCH_SET_STATUS_ERROR", new Object[] { "in-only",
                        inonly.getExchangeId(), DCOMUtil.getStackTraceAsString(ex) });
            }
        }

        try {
            // Send back exchange status
            mChannel.send(inonly);
        } catch (MessagingException mex) {
            mLogger.log(Level.WARNING, "OutboundMessageProcessor.NMR_SEND_STATUS_ERROR", inonly.getExchangeId());
        }
    } */

    private void setErrorStatusAndSend(MessageExchange msg) {
        try {
            msg.setStatus(ExchangeStatus.ERROR);

            mLogger.log(Level.INFO, "OutboundMessageProcessor.MXCH_SET_STATUS", new Object[] { "ERROR",
                    msg.getExchangeId() });

        } catch (MessagingException ex) {
            mLogger.log(Level.WARNING, "OutboundMessageProcessor.MXCH_SET_STATUS_ERROR", new Object[] { "in-only",
                    msg.getExchangeId(), ex });
        }

        try {
            // Send back exchange status
            mChannel.send(msg);
        } catch (MessagingException mex) {
            mLogger.log(Level.WARNING, "OutboundMessageProcessor.NMR_SEND_STATUS_ERROR", msg.getExchangeId());
        }

    }

    // suspend thread transactional context
    private void suspendThreadTx(Transaction tx) throws Exception {
        // suspend only if current running transaction exists
        if (tx != null) {
            ((TransactionManager) mComponentContext.getTransactionManager()).suspend();
            mLogger.log(Level.INFO, "OutboundMessageProcessor.TX_SUSPEND_SUCCEEDED", new Object[] { tx.toString() });
        }
    }

    // suspend thread transactional context
    private void resumeThreadTx(Transaction tx) throws Exception {
        if (tx != null) {
            ((TransactionManager) mComponentContext.getTransactionManager()).resume(tx);
            mLogger.log(Level.INFO, "OutboundMessageProcessor.TX_RESUME_SUCCEEDED", new Object[] { tx.toString() });
        }
    }

}
