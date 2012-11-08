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

package com.sun.jbi.msmqbc;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Collections;

import javax.xml.namespace.QName;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;

import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import org.w3c.dom.Document;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.Endpoint.EndpointMessageType;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.msmq.Channel;
import com.sun.jbi.msmqbc.jni.MSMQTxMode;
import com.sun.jbi.msmqbc.util.MSMQUtil;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.msmqbc.packaging.EndpointConfigurationFactory;

/**
 * Processes inbound messages. Beware that replies come back the same route as outbound requests and
 * will be processed by the OutboundMessageProcessor
 *
 * @author Sun Microsystems
 */
public class InboundMessageProcessor implements Runnable, ReplyListener {

    private static final long serialVersionUID = 3256727264572813369L;

    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    static final String IN_MSG = "in";

    private DocumentBuilderFactory factory;

    private DocumentBuilder builder;

    private DeliveryChannel mChannel;

    private ComponentContext mContext;

    private MessageExchangeFactory mMessageExchangeFactory;

    private Channel msmqChannel;

    private Endpoint mEndpoint;

    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private Object mMonitor;

    private long mIdleTime = 0;

    private long mLastProcessedTime = System.currentTimeMillis();

    private ServiceEndpoint mServiceEndpoint = null;

    private MSMQInput msmqInput;

    private MSMQOutput msmqOutput;

    private MSMQMessage msmqMessage;

    private MSMQOperation msmqOperation;

    private MSMQNormalizer msmqNormalizer;

    private MSMQDenormalizer msmqDenormalizer;

    private boolean mTransacted;

    private boolean isInput;

    private String mTransaction;

    // For inout message exchanges, this map stores the reply destinations
    // keyed by the message exchange id
    private Map mMsgContexts = Collections.synchronizedMap(new HashMap());

    public InboundMessageProcessor(ComponentContext context, Endpoint endpoint, DeliveryChannel dChannel,
            Channel channel) throws MessagingException {

        mContext = context;
        mEndpoint = endpoint;
        mChannel = dChannel;
        msmqChannel = channel;
        msmqOperation = msmqChannel.getMSMQOperation();
        msmqInput = msmqOperation.getMSMQOperationInput();
        msmqOutput = msmqOperation.getMSMQOperationOutput();
        msmqMessage = msmqChannel.getMSMQMessage();
        initialize();
    }

    protected InboundMessageProcessor(ComponentContext context, Endpoint endpoint, DeliveryChannel dChannel,
            Channel channel, MSMQDenormalizer mDenormalizer, MSMQNormalizer mNormalizer) throws MessagingException {

        mContext = context;
        mEndpoint = endpoint;
        mChannel = dChannel;
        msmqChannel = channel;
        msmqOperation = msmqChannel.getMSMQOperation();
        msmqInput = msmqOperation.getMSMQOperationInput();
        msmqOutput = msmqOperation.getMSMQOperationOutput();
        msmqMessage = msmqChannel.getMSMQMessage();
        msmqDenormalizer = mDenormalizer;
        msmqNormalizer = mNormalizer;

        initialize();

    }

    private void initialize() throws MessagingException {

        mTransaction = msmqMessage.getTransaction();

        if (mTransaction.equals(MSMQTxMode.XA_TRANSACTION)) {
            mTransacted = true;
        }

        mMonitor = new Object();

        try {
            factory = DocumentBuilderFactory.newInstance();
            builder = factory.newDocumentBuilder();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_INIT_FAILED", new Object[] { ex.getLocalizedMessage() });

            throw new MessagingException(
                    mMessages.getString("InboundMessageProcessor_INIT_FAILED", new Object[] { ex }));
        }
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }

    public void run() {

        if (msmqInput != null) {
            mLogger.log(Level.INFO, "InboundMessageProcessor_SERVICE_LOOP_ENTER", new Object[] {
                    msmqMessage.getQueueName(), mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
                    msmqOperation.getBindingOperation().getName() });
        }

        // First, initialize normalizer and denormalizer
        if (msmqNormalizer == null) {
            msmqNormalizer = MSMQNormalizer.getInstance();
        }

        if (msmqDenormalizer == null) {
            msmqDenormalizer = MSMQDenormalizer.getInstance();
        }

        MSMQPayLoad msmqPayLoad = null;
        byte[] msmqMsg = null;

        do {

            Transaction tx = null;
            if (mTransacted && (msmqOperation.getMEP().equals("inonly"))) {
                try {
                    tx = startThreadTx();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_XA_TX_START_FAILED", new Object[] { ex });
                    break;
                }

                try {
                    if (tx != null) {
                        tx.enlistResource(msmqChannel.getXAResource());
                    }
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_FAILED_TO_ENLIST_XA_RESOURCE",
                            new Object[] { ex });
                    break;
                }
            }

            MSMQPayLoad mPayLoad = null;

            try {
                // retrieve the msmqPayload from the Channel
                mPayLoad = msmqChannel.receive();
            } catch (Exception ex) {
                try {
                    msmqChannel.close();
                } catch (MSMQException me) {
                    mLogger.log(Level.WARNING, "InboundMessageProcessor_MSMQ_ERROR", new Object[] { me });

                }
                mLogger.log(Level.WARNING, "InboundMessageProcessor_MSMQ_ERROR", new Object[] { ex });
            }

            if (mPayLoad.getPayLoad() != null) {
                onMessage(mPayLoad, tx);
            } else {
                if (tx != null){
                    //if message is not received delist resource and commit tx
                    commitThreadTx(tx);
                }
            }

            // Retry using user specified interval or default interval
            long rInterval = msmqMessage.getRetryInterval().longValue();
            try {
                Thread.sleep(rInterval);
            } catch (InterruptedException ex) {
                // do nothing
                ;
                ;
            }
        } while (mMonitor != null);

        mLogger.log(Level.INFO, "InboundMessageProcessor_SERVICE_LOOP_EXIT", new Object[] {
                mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
                msmqOperation.getBindingOperation().getName() });
    }

    private void commitThreadTx(Transaction tx) {

        // if xa, commit on timeout receive
        if (tx != null) {
            try {
                tx.delistResource(msmqChannel.getXAResource(), XAResource.TMSUCCESS);
            } catch (Exception ex) {
                mLogger.log(Level.WARNING, "InboundMessageProcessor_DELIST_XA_RESOURCE_FAILED",
                        new Object[] { ex });
            }

            try {
                tx.commit();
            } catch (Exception ex) {
                mLogger.log(Level.WARNING, "InboundMessageProcessor_XA_TX_COMMIT_FAILED", new Object[] { ex });
            }
        }

    }

    private void commitThreadTx(MessageExchange msgXChange) throws Exception {
        if (mTransacted) {
            Transaction tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tx != null) {
                try {
                    tx.delistResource(msmqChannel.getXAResource(), XAResource.TMSUCCESS);
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_DELIST_XA_RESOURCE_FAILED", new Object[] { ex });
                    throw ex;
                }

                try {
                    tx.commit();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_XA_TX_COMMIT_FAILED", new Object[] { "commit",
                            ex });
                    throw ex;
                }
            } else {
                mLogger.log(Level.WARNING, "InboundMessageProcessor_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                        new Object[] { msgXChange.getExchangeId() });
            }
        }
    }

    private void rollbackThreadTx(MessageExchange msgXChange) throws Exception {
        if (mTransacted) {
            Transaction tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tx != null) {
                try {
                    tx.delistResource(msmqChannel.getXAResource(), XAResource.TMFAIL);
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_DELIST_XA_RESOURCE_FAILED", new Object[] { ex });
                    throw ex;
                }

                try {
                    tx.rollback();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "InboundMessageProcessor_XA_TX_ROLLBACK_FAILED", new Object[] { "commit",
                            ex });
                    throw ex;
                }
            } else {
                mLogger.log(Level.WARNING, "InboundMessageProcessor_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                        new Object[] { msgXChange.getExchangeId() });
            }
        }
    }

    public long getRequestInvocationTime() {
        return mLastProcessedTime;
    }

    /**
     * Stop the InboundMessageProcessor thread.
     */
    public void stopReceiving() {
        mLogger.log(Level.INFO, "InboundMessageProcessor_STOP", new Object[] { mEndpoint.getServiceName().toString(),
                mEndpoint.getEndpointName(), msmqOperation.getBindingOperation().getName() });

        mMonitor = null;
    }

    public void onMessage(MSMQPayLoad mPayLoad, Transaction tx) {
        mLogger.log(Level.INFO, "InboundMessageProcessor_MSMQMSG_RECV");

        long currentTime = System.currentTimeMillis(); // capture current time
        mIdleTime = currentTime - mLastProcessedTime;
        mLastProcessedTime = currentTime;

        boolean success = true;
        QName bindingOpQname = new QName(msmqOperation.getBindingOperation().getName());
        MessageExchange msgEx = null;
        NormalizedMessage normalizedMsg = null;
        isInput = true; // this is one way

        try {
            // First prepare MessageExchange (i.e., convert MSMQ message to Normalized Message)
            byte[] msg = mPayLoad.getPayLoad();
            String textMsg = new String(msg);

            mLogger.log(Level.INFO, "InboundMessageProcessor_MSMQMSG_MSG_MSG", new Object[] { textMsg });

            msgEx = createMessageExchange(msmqOperation.getMEP());

            mLogger.log(Level.INFO, "InboundMessageProcessor_BEGIN_NORMALIZE_MESSAGE");

            normalizedMsg = msmqNormalizer.normalize(msgEx, bindingOpQname, msmqMessage, isInput, mEndpoint, mPayLoad);

            mLogger.log(Level.INFO, "InboundMessageProcessor_END_NORMALIZE_MESSAGE");

            synchronized (this) {
                if (msgEx instanceof InOnly) {
                    sendInOnly((InOnly) msgEx, normalizedMsg, bindingOpQname, tx);
                } else if (msgEx instanceof InOut) {
                    sendInOut((InOut) msgEx, normalizedMsg, bindingOpQname);
                }

                // save reply context for future processing
                if (msmqOperation.getMEP().equals(EndpointMessageType.IN_OUT) && msmqOutput.getMsmqMessage() != null) {
                    mMsgContexts.put(msgEx.getExchangeId(), new ReplyContext(msmqOutput.getMsmqMessage(),
                            bindingOpQname, msmqOperation, msmqInput, msmqOutput));
                    mLogger.log(Level.INFO, "InboundMessageProcessor_REPLY_CONTEXT_SAVED", new Object[] {
                            msgEx.getExchangeId(), bindingOpQname.toString(), msmqOutput.getMsmqMessage() });
                }

                if (mTransacted) { // need to block further receiving of msmq messages until
                    // message xchange response is processed.
                    mLogger.log(Level.INFO, "InboundMessageProcessor_TRANSACTION_WAIT_FOR_RESPONSE",
                            new Object[] { msgEx.getExchangeId() });
                    // need to have a timeout here?
                    wait();
                }
            }
        } catch (Exception ex) {
            if (msmqInput != null) {
                mLogger.log(Level.SEVERE, "InboundMessageProcessor_MSMQ_MSGING_ERROR", new Object[] {
                        msmqInput.getMsmqMessage().getQueueName(), ex.getLocalizedMessage() });
            }
            success = false;
        } catch (Throwable ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_NMR_SEND_ERROR", new Object[] {
                    mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
                    msmqChannel.getMSMQOperation().getBindingOperation().getName(), ex.getLocalizedMessage() });
            success = false;
        }

        // rollback on any processing errors
        if (!success) {
            try {
                if (tx != null) {
                    // possibly, didn't get to the point where the transaction
                    // is set on the msg exchange (i.e., normalization error)
                    msgEx.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
                }

                rollbackThreadTx(msgEx);
            } catch (Throwable ex) {
                // logged
                ;
                ;
            }
        } else {

        }

    }

    public boolean onReply(MessageExchange msgXChange) {
        boolean success = false;
        if (msgXChange instanceof InOnly) {
            success = processInOnlyReply((InOnly) msgXChange);
        } else if (msgXChange instanceof InOut){
        	success = processInOutReply((InOut) msgXChange);
    	} else {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_UNSUPPORTED_MEP",
                    new Object[] { msgXChange.getPattern().toString() });
    	}
        return success;
    }

    private boolean processInOnlyReply(InOnly msgXChange) {
        Transaction tx = null;
        boolean success = false;
        String messageId = msgXChange.getExchangeId();

        mLogger.log(Level.INFO, "InboundMessageProcessor_IN_ONLY_REPLY", new Object[] { messageId });

        ExchangeStatus status = msgXChange.getStatus();

        mLogger.log(Level.INFO, "InboundMessageProcessor_MXCH_PROCESS_REPONSE", new Object[] { "In-Only", messageId,
                status.toString() });

        if (status == ExchangeStatus.DONE) {
            if (mInboundExchanges.containsKey(messageId)) {
                mLogger.log(Level.INFO, "InboundMessageProcessor_MXCH_RESPONSE_ID_FOUND", new Object[] { messageId });
                success = true;
                if (msgXChange.isTransacted()) {
                    // As we are the initiator for tx we have commit it.
                    tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                    try {
                        // we have to resume the suspended transaction
                        resumeThreadTx(tx);
                    } catch (Exception ex) {
                        // for in-only there's no sending of status back to nmr
                        // failure will be logged
                        mLogger.log(Level.WARNING, "InboundMessageProcessor_RESUME_FAILED", 
							new Object[]{MSMQUtil.getStackTraceAsString(ex)});
                    }
                    try{
                        if(tx.getStatus() == Status.STATUS_MARKED_ROLLBACK){
                            try{
                                // As we are the initiator for tx we have to rollback
                                rollbackThreadTx(msgXChange);
                            } catch(Exception ex){
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.WARNING, "InboundMessageProcessor_ROLLBACK_FAILED", 
									new Object[]{MSMQUtil.getStackTraceAsString(ex)});
                            }
                        } else {
                            try {
                                // As we are the initiator for tx we have to commit
                                commitThreadTx(msgXChange);
                            } catch (Exception ex) {
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.WARNING, "InboundMessageProcessor_COMMIT_FAILED", 
									new Object[]{MSMQUtil.getStackTraceAsString(ex)});
                            }
                        }
                    } catch(Exception ex){
                        mLogger.log(Level.INFO, "InboundMessageProcessor_POST_PROCESS_FAILED", 
							new Object[]{MSMQUtil.getStackTraceAsString(ex)});
                    }
                }

            } else {
                mLogger.log(Level.INFO, "InboundMessageProcessor_MXCH_NOT_FOUND", new Object[] { messageId });
            }
        } else { // ERROR or ACTIVE (for one way we don't expect a response)
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_MXCH_BAD_STATUS", new Object[] { status.toString(),
                    messageId });
            if (mInboundExchanges.containsKey(messageId)) {
                try {
                    // As we are the initiator for tx we have to rollback
                    rollbackThreadTx(msgXChange);
                } catch (Exception ex) {
                    // for in-only there's no sending of status back to nmr
                    // failure will be logged
                    ;
                    ;
                }
            }
            success = true;
        }

        if (mInboundExchanges.containsKey(messageId)) {
            mInboundExchanges.remove(messageId);
            mLogger.log(Level.INFO, "InboundMessageProcessor_MXCH_REMOVED", new Object[] { messageId });
        }

        // Notify inbound message processor thread possibly waiting...
        synchronized (this) {
            notify();
        }

        return success;
    }
    private boolean processInOutReply(InOut msgXChange){
    	boolean success = false;
    	
        String messageId = msgXChange.getExchangeId();
        
         mLogger.log(Level.INFO, 
                     "InboundMessageProcessor_IN_OUT_REPLY",
                     new Object [] {messageId});

        ExchangeStatus status = msgXChange.getStatus();

        mLogger.log(Level.INFO, 
                "InboundMessageProcessor_MXCH_PROCESS_REPONSE",
                new Object[]{"In-Out", messageId, status.toString()});

        
        if (msgXChange.getOutMessage() != null) {
            if (mInboundExchanges.containsKey(messageId))  {      
                mLogger.log(Level.INFO,
                        "InboundMessageProcessor_MXCH_RESPONSE_ID_FOUND",
                        new Object[]{messageId});
                
                boolean replyContextFound = false;
                
                synchronized (this) {
                    replyContextFound = mMsgContexts.containsKey(messageId) &&
                                        mMsgContexts.get(messageId) != null;
                }
                
                if (replyContextFound) {       
                    mLogger.log(Level.INFO,
                            "InboundMessageProcessor_REPLY_CONTEXT_FOUND",
                            new Object[]{messageId});                
                    ReplyContext context = (ReplyContext)mMsgContexts.get(messageId);
                    QName opQname = context.getBindingOperationQName();
                    MSMQOperation op = context.getMSMQOperation();
                    MSMQInput msmqInput = context.getMSMQInput();
                    MSMQOutput msmqOutput = context.getMSMQOutput();
                    NormalizedMessage normalizedMsg = msgXChange.getOutMessage();
                    String destinationName = msmqOutput.getMsmqMessage().getDestination();
                    
                    try {                             
                            mLogger.log(Level.INFO,
                                    "InboundMessageProcessor_BEGIN_DENORMALIZE_MESSAGE");

                        // Denormalize from NMS message to MSMQ response message
                        MSMQPayLoad responseMsg =  msmqDenormalizer.denormalize(msmqChannel,
                                                                            normalizedMsg,
                                                                            opQname,
                                                                            msmqOutput.getMsmqMessage());
                             
                        mLogger.log(Level.INFO,
                                    "InboundMessageProcessor_END_DENORMALIZE_MESSAGE");
                        mLogger.log(Level.INFO, 
                                "InboundMessageProcessor_SEND_MSMQ_REPLY_TO",
                                new Object[]{messageId, 
                        					destinationName});
                        
                        sendMSMQResponse(destinationName,responseMsg);
                        success = true;                        
                    } catch (Exception ex) {
                        mLogger.log(Level.SEVERE,
                                "InboundMessageProcessor_SEND_MSMQ_REPLY_MESSAGE_FAILED",
                                new Object [] {messageId, 
                        					   destinationName,
                                               ex});
                    }
                } else {
                    mLogger.log(Level.SEVERE,
                            "InboundMessageProcessor_NO_REPLY_CONTEXT_FOUND",
                            new Object [] {messageId});
                }                
            } else {
                mLogger.log(Level.INFO,
                        "InboundMessageProcessor_MXCH_NOT_FOUND",
                        new Object[]{messageId});
            }            
        } else { // Treat ERRROR status, Error, and Faults as errors
            if (msgXChange.getStatus().equals(ExchangeStatus.ERROR)) {
                mLogger.log(Level.SEVERE, 
                        "InboundMessageProcessor_MXCH_BAD_STATUS",
                        new Object[]{status.toString(),
                                     messageId});
            } else if (msgXChange.getError() != null) {
                mLogger.log(Level.SEVERE, 
                        "InboundMessageProcessor_MXCH_ERROR_REPLY",
                        new Object[]{messageId,
                                     msgXChange.getError()});
            } else if (msgXChange.getFault() != null) {
                try {
                    String faultMsg = writeMessage(msgXChange.getFault());
                    mLogger.log(Level.SEVERE, 
                            "InboundMessageProcessor_MXCH_FAULT_REPLY",
                            new Object[]{messageId,
                                         faultMsg});                    
                } catch (Exception ex) {
                    ;;
                }
            }            
            success = true;
        }
        
        // remove exchange from exchanges map
        if (mInboundExchanges.containsKey(messageId))  {
            mInboundExchanges.remove(messageId);           
            mLogger.log(Level.INFO, 
                        "InboundMessageProcessor_MXCH_REMOVED",
                        new Object[]{messageId});
        }
        
        // remove msmq msg context from map
        if (mMsgContexts.containsKey(messageId)) {
            mMsgContexts.remove(messageId);         
            mLogger.log(Level.INFO,
                        "InboundMessageProcessor_REPLY_CONTEXT_REMOVED",
                        new Object[]{messageId});
        }
        
        // Notify inbound message processor thread possibly waiting...
        synchronized(this) {
            notify();
        }
        
        return success;
    }
    

    private void sendInOnly(InOnly inonly, NormalizedMessage inMsg, QName bindingOpQname, Transaction tx)
            throws MessagingException {

        if (!locateServiceEndpoint(mEndpoint.getServiceName(), mEndpoint.getEndpointName())) {
            // Failed to locate provider endpoint
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_ENDPOINT_NOTFOUND", new Object[] {
                    mEndpoint.getEndpointName(), mEndpoint.getServiceName() });

            throw new MessagingException(mMessages.getString("InboundMessageProcessor_ENDPOINT_NOTFOUND", new Object[] {
                    mEndpoint.getEndpointName(), mEndpoint.getServiceName() }));
        }

        String exchangeId = inonly.getExchangeId();
        mInboundExchanges.put(exchangeId, this);

        inonly.setEndpoint(mServiceEndpoint);
        inonly.setOperation(bindingOpQname);
        try {
            inonly.setInMessage(inMsg);
            if (mLogger.isLoggable(Level.INFO)) {
                logNormalizedMessage(exchangeId, inMsg.getContent());
            }

            if (tx != null) { // propagate xa transaction if there is one
                inonly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
                suspendThreadTx(tx);
            }

            mChannel.send(inonly);
            mEndpoint.getEndpointStatus().incrementSentRequests();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_SEND_TO_NMR_FAILED", new Object[] { "InOnly",
                    ex.getLocalizedMessage() });
            mInboundExchanges.remove(exchangeId);
            throw new MessagingException(ex);
        }
    }

    private void sendInOut(InOut inout, NormalizedMessage inMsg, QName bindingOpQname)
            throws MessagingException {
        if (!locateServiceEndpoint(mEndpoint.getServiceName(), mEndpoint.getEndpointName())) {
            // Failed to locate provider endpoint
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_ENDPOINT_NOTFOUND", new Object[] {
                    mEndpoint.getEndpointName(), mEndpoint.getServiceName() });

            throw new MessagingException(mMessages.getString("InboundMessageProcessor_ENDPOINT_NOTFOUND", new Object[] {
                    mEndpoint.getEndpointName(), mEndpoint.getServiceName() }));
        }

        String exchangeId = inout.getExchangeId();
        mInboundExchanges.put(exchangeId, this);

        inout.setEndpoint(mServiceEndpoint);
        inout.setOperation(bindingOpQname);
        try {
            inout.setInMessage(inMsg);
            logNormalizedMessage(exchangeId, inMsg.getContent());
            mChannel.send(inout);
            msmqChannel.getEndpoint().getEndpointStatus().incrementSentRequests();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_SEND_TO_NMR_FAILED", new Object[] { "InOut",
                    ex.getLocalizedMessage() });
            mInboundExchanges.remove(exchangeId);
            throw new MessagingException(ex);
        }
    }

    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            StringWriter out = new StringWriter();
            StreamResult result = new StreamResult(out);
            trans.transform(msgSrc, result);
            out.flush();
            out.close();
            mLogger.log(Level.FINE, "InboundMessageProcessor_SEND_TO_NMR", new Object[] { exchangeId, out.toString() });
        } catch (Throwable t) {
            ;
            ;
        }
    }

    private boolean locateServiceEndpoint(QName serviceName, String endpointName) {
        if (mServiceEndpoint == null) {
            if (EndpointConfigurationFactory.isJBIRoutingEnabled()) {
                mServiceEndpoint = mContext.getEndpoint(serviceName, endpointName);
            } else {
                ServiceEndpoint[] allServiceEndpoints = mContext.getEndpointsForService(serviceName);
                if (allServiceEndpoints != null) {
                    int epCount = 0;
                    while (mServiceEndpoint == null && epCount < allServiceEndpoints.length) {
                        if (allServiceEndpoints[epCount].getEndpointName().equals(endpointName)) {
                            // Found our endpoint
                            mServiceEndpoint = allServiceEndpoints[epCount];
                        }
                        epCount++;
                    }
                }
            }
        }

        return (mServiceEndpoint != null);
    }

    private MessageExchange createMessageExchange(String mepType) throws MSMQException {
        MessageExchange msgEx = null;

        if (mMessageExchangeFactory == null) {
            mMessageExchangeFactory = mChannel.createExchangeFactory();
        }

        try {
            if (mepType.equals(EndpointMessageType.IN_ONLY)) {
                msgEx = mMessageExchangeFactory.createInOnlyExchange();
            } else if (mepType.equals(EndpointMessageType.IN_OUT)) {
                msgEx = mMessageExchangeFactory.createInOutExchange();
            } else {
                mLogger.log(Level.SEVERE, "InboundMessageProcessor_MXCH_CREATE_UNSUPPORTED_MEP",
                        new Object[] { mepType });

                throw new MSMQException(mMessages.getString("InboundMessageProcessor_MXCH_CREATE_UNSUPPORTED_MEP",
                        new Object[] { mepType }));
            }
        } catch (MessagingException ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor_MXCH_CREATE_FAILED", new Object[] { mepType, ex });

            throw new MSMQException(mMessages.getString("InboundMessageProcessor_MXCH_CREATE_FAILED", new Object[] {
                    mepType, ex }));
        }

        return msgEx;
    }

    private Transaction startThreadTx() throws Exception {
        Transaction tx = null;
        TransactionManager txManager = getTransactionManager();
        txManager.begin();
        tx = txManager.getTransaction();
        return tx;
    }

    private TransactionManager getTransactionManager() {
        return (TransactionManager) mContext.getTransactionManager();
    }

    // suspend thread transactional context
    private void suspendThreadTx(Transaction tx) throws Exception {
        // suspend only if current running transaction exists
        if (tx != null) {
            ((TransactionManager) mContext.getTransactionManager()).suspend();
            mLogger.log(Level.INFO, "InboundMessageProcessor_TX_SUSPEND_SUCCEEDED", new Object[] { tx.toString() });
        }
    }

    // suspend thread transactional context
    private void resumeThreadTx(Transaction tx) throws Exception {
        if (tx != null) {
            ((TransactionManager) mContext.getTransactionManager()).resume(tx);
            mLogger.log(Level.INFO, "	", new Object[] { tx.toString() });
        }
    }
    
    private void sendMSMQResponse(String dest,
            MSMQPayLoad msg){
		try{
			if(dest != null){
				msmqChannel.send(msg);
			}
		} catch (Exception ex) {
			mLogger.log(Level.WARNING, "InboundMessageProcessor_SEND_MSMQMSG", 
						new Object[]{dest, MSMQUtil.getStackTraceAsString(ex)});
		}
	
	}

	private String writeMessage(NormalizedMessage msg) throws TransformerException {
		String s = null;
		try {
		if (msg.getContent() != null) {
			ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
			TransformerFactory tFactory = TransformerFactory.newInstance();
			Transformer trans = tFactory.newTransformer();
			Source source = msg.getContent();
			StreamResult result = new StreamResult(byteArrayOutputStream);
			trans.transform(source, result);
			s = byteArrayOutputStream.toString();
		}
		} catch (Exception ex) {
			;;
		}
			return s;
	}   

}
