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

package com.sun.jbi.mqbc;

import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.BaseExchangeTemplates;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.messaging.SendFailureListener;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.mqbc.extensions.MQBCAddress;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extensions.MQBCRedelivery;
import com.sun.jbi.mqbc.extservices.ExtServiceMQMessage;
import com.sun.jbi.mqbc.extservices.MQClientAgent;
import com.sun.jbi.mqbc.extservices.MQClientConfiguration;
import com.sun.jbi.mqbc.extservices.PMO;
import com.sun.jbi.mqbc.extservices.QueueAccessOptions;
import com.sun.jbi.mqbc.extservices.UnknownCipherSuiteException;
import com.sun.jbi.mqbc.messaging.MessageProperties;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.util.HexBinary;
import net.java.hulp.measure.Probe;

/**
 * Implements the work loop for polling an MQ queue for messages and pushing
 * them out to the NMR. This is the "message pump" functionality of the BC, its
 * inbound functionality.
 * 
 * @author rchen
 * @author Noel.Ang@sun.com
 */
final class InboundMessageProcessor implements SendFailureListener, Runnable {
    /**
     * Property name used to embed callback for notifying of completed
     * message exchanges. 
     */
    static final String EXCHANGE_CONTROL_OBJ_PROPERTY_NAME =
            "MQBC_EXCHANGE_CONTROL_PROPERTY_NAME";
    
    /**
     * Property name used to embed the Service Unit for resuming/suspending
     * endpoints as part of the message exchange redelivery service quality. 
     */
    static final String SERVICE_UNIT_PROPERTY_NAME =
            "SERVICE_UNIT_PROPERTY_NAME";
    
    /**
     * Property name used to embed callback for indicating syncpoint
     * backout due for a given message exchange.
     */
    static final String SYNCPOINT_DISPOSITION_CONTROL_PROPERTY_NAME = 
            "MQBC_SYNCPOINT_DISPOSITION_CONTROL_PROPERTY_NAME";
    
    private final EventLogger mLogger;
    private final MQNormalizer mNormalizer = new MQNormalizer();
    private MessagingChannel mChannel;
    private MQComponentContext mContext;
    private final MessageExchangeFactory mMsgExchangeFactory;
    private final Endpoint mEndpoint;
    private final QName mOperationFullQName;
    private ServiceEndpoint mServiceEndpoint;
    private final ServiceUnit mServiceUnit;

    private final MQBCOperation mOperation;
    private final Boolean mSyncPoint;
    private final String mQname;
    private final String mQMgrname;
    private final MQClientAgent mClntAgnt;
    private final boolean mXaMode;
    private final String mId;
    
    private final MQBCRedelivery mRedeliverySpec;
    private final boolean mDoRetry;
    private final boolean mDoReroute;
    
    private volatile boolean mSyncpointRollback = false;
    private volatile boolean mNotifyDoneCalled = false;
    private volatile boolean isStopped = false;
    private volatile boolean isSuspended = false;
    
    /**
     * Creates a new instance of InboundMessageProcessor.
     *
     * @param context MQComponentContext object
     * @param serviceUnit Service Unit that owns the endpoint
     * @param endpoint The endpoint that this processor consumes
     * @param operationFullQName The operation this processor calls
     * @param qos List of ServiceQuality(ies) that apply to this processor's
     * endpoint.
     *
     * @throws javax.jbi.messaging.MessagingException
     */
    InboundMessageProcessor(MQComponentContext context,
                            ServiceUnit serviceUnit,
                            Endpoint endpoint,
                            QName operationFullQName,
                            List<ServiceQuality> qos)
            throws MessagingException {

        assert operationFullQName.getNamespaceURI() != null;
        assert !operationFullQName.getNamespaceURI().equals("");
        assert context != null;
        assert serviceUnit != null;
        assert endpoint != null;

        mLogger = new EventLogger(Util.getLogger(context,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);

        Map opMEPs = endpoint.getOperationMsgExchangePattern();
        if (opMEPs.get(operationFullQName) == null) {
            throw new MessagingException(I18n.msg(
                    "2000: Cannot create message exchange"
                            + " sender for operation {0}, operation"
                            + " is unknown to endpoint {1}",
                    operationFullQName.toString(),
                    endpoint.getServiceEndpointName().toString()));
        }
        
        mContext = context;
        mChannel = context.getMessagingChannel();
        mEndpoint = endpoint;
        mOperationFullQName = operationFullQName;
        mServiceUnit = serviceUnit;
        
        mOperation = (MQBCOperation)mEndpoint.getMQOperations().get(mOperationFullQName);
        mQname = mOperation.getQueueName().trim();
        mXaMode = mOperation.getTransaction();
        mSyncPoint = mXaMode || mOperation.getMQOperationInput().getMQMessage().getMQSyncPoint();
        MQBCAddress mqAddress = mEndpoint.getMQAddress();
        mQMgrname = mqAddress.getQueueManagerName();
        MQClientConfiguration mQClnConfig = new MQClientConfiguration();
        mQClnConfig.setQueueManagerName(mQMgrname);
        mQClnConfig.setHost(mqAddress.getHostName());
        mQClnConfig.setChannelName(mqAddress.getChannelName());
        mQClnConfig.setPort(mqAddress.getPortNumber());
        mQClnConfig.setQueueName(null);
        mQClnConfig.setReceiveMode(true);
        mQClnConfig.setXAMode(mXaMode);
        mQClnConfig.setCipherSuite(mqAddress.getCipherSuite());
        mQClnConfig.setSslPeerName(mqAddress.getSslPeerName());
        try {
            mClntAgnt = new MQClientAgent(mQClnConfig);
        } catch (UnknownCipherSuiteException e) {
            throw new MessagingException(e);
        }
        connectAgent();

        mMsgExchangeFactory = context.getMessagingChannel().createExchangeFactory();
        
        // Register ourself as a send failure listener
        // Note that this listener is only notified if an async send is
        // performed (e.g., Redelivery QoS waitTime > 0).  If a synchronous
        // send fails, this listener is not involved, the send will raise
        // an exception immediately.
        context.getMessagingChannel().addSendFailureListener(this);
        
        mId = mEndpoint.getServiceName().toString() + ','
                + mEndpoint.getEndpointName();

        initServiceQualities(qos);

        mRedeliverySpec = endpoint.getRedelivery(operationFullQName);
        mDoReroute = mRedeliverySpec != null && !mRedeliverySpec.getTarget().equals("");
        mDoRetry = mRedeliverySpec != null && mRedeliverySpec.getCount() > 0;

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Message exchange sender created for endpoint {0} operation {1}",
                    endpoint.getEndpointName(),
                    mOperationFullQName.toString()));
        }
    }
    
    private boolean concurrencyCheck() {
        boolean atMaxConcurrency = isAtMaximumConcurrency();
        if (atMaxConcurrency) {
            // Logging about throttled endpoints is subject to throttling, too.
            if (isPastChattyLogTime()) {
                markLogChatterTime();
                mLogger.info(NotificationEvent.SEVERITY_TYPE_MINOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2021: Aborted message processing -"
                                + " maximum {0} concurrent and outstanding" 
                                + " exchanges have been reached.",
                                getMaximumConcurrentInboundExchanges()));
            }
        }
        return atMaxConcurrency;
    }
    
    private ExtServiceMQMessage receiveMessage() {
        ExtServiceMQMessage message = null;
        try {
            message = mClntAgnt.retrieveMessage(mSyncPoint);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2002: Failed to get message from queue ''{0}''",
                            mQname),
                    e);
        }
        return message;
    }
    
    private void execute() {
        
        // Throttling check. Abort if I am at concurrency cap.
        if (concurrencyCheck()) {
            return;
        }
        
        // Ensure MQ connection is available
        if (!mClntAgnt.isConnected()) {
            if (!connectAgent()) {
                return;
            }
        }

        Probe probeInWire = Probe.fine(getClass(),
                "Inbound Message Processing (In-wire)",
                mId);
            
        Probe probeWireToNMR = Probe.fine(getClass(),
                "Inbound Message Processing (wire to NMR)",
                mId);

        // Access the queue.
        try {
            int qOpenOption = mOperation.getQueueOpenOptions();
            // Cannot set INPUT_AS_Q_DEF because attempting to do so for
            // remote queues raises an exception. Since being here means the
            // user intends the queue in regard to be read from, we must
            // resort to hope that the queue is configured correctly for reading.
            QueueAccessOptions qoption = new QueueAccessOptions(qOpenOption);
            qoption.setMQOO_OUTPUT(true);
            mClntAgnt.accessQueue(mQname,qoption.getOptions(),null);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2002: Failed to get message from queue ''{0}''",
                            mQname),
                    e);
            probeInWire.end();
            probeWireToNMR.end();
            return;
        }
        
        // Start the transaction - about to remove a message from the queue
        try {
            startTransaction();
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2005: Message could not be sent."),
                    e);
            endTransactionWithRollback();
            probeInWire.end();
            probeWireToNMR.end();
            return;
        }
            
        // Retrieve a message from the queue.
        // If no message in the queue, quit early.
        ExtServiceMQMessage qMsg = receiveMessage();
        if (qMsg == null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("No message available."));
            }
            if (mXaMode) {
                if (mLogger.isLoggable(Level.FINER)){
                    mLogger.finer(I18n.msg(
                            "No message available; ending transaction"
                                    + " in effect with a rollback."));
                }
            }
            endTransactionWithRollback();
            probeWireToNMR.end();
            probeInWire.end();
            return;
        }
        
        probeInWire.end();
            
        // Message found, resolve destination
        try {
            locateServiceEndpoint();
        }catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2005: Message could not be sent."),
                    e);
            endTransactionWithRollback();
            probeWireToNMR.end();
            return;
        }
        
        // Failed deliveries due to suspended endpoints are not subject to
        // the dead-letter rerouting.
        if (isSuspended(mEndpoint)) {
            mLogger.warning(NotificationEvent.SEVERITY_TYPE_MINOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2003: Message is available"
                            + " but not deliverable. Endpoint {0} is suspended.",
                            mEndpoint.getServiceEndpointName().toString()));
            endTransactionWithRollback();
            probeWireToNMR.end();
            return;
        }        
            
        // Suspend the transaction - getting ready to fire off the
        // exchange thru the NMR.
        try {
            suspendTransaction();
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2005: Message could not be sent."),
                    e);
            endTransactionWithRollback();
            probeWireToNMR.end();
            return;
        }
        
        // Deliver the message
        boolean interrupted = false;
        boolean completed = false;
        final int RETRIES = mDoRetry ? mRedeliverySpec.getCount() : 0;

        for (int attempt = 0;
             attempt <= RETRIES && !interrupted && !completed;
             ++attempt) {
            
            String msgId = null;
            try {
                // Sending the exchange must be done in the synchronized block
                // to avoid the situation of a reply/ack causing notify() to
                // happen before I retrieveMessage a chance to wait().
                synchronized (this) {
                    try {
                        msgId = sendMessageExchange(qMsg);
                    } finally {
                        probeWireToNMR.end();
                    }
                    
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer(I18n.msg(
                                "Send thread waiting for exchange outcome."
                                        + " Thread {0}, message id {1}",
                                Thread.currentThread().getId(), msgId));
                    }
                    
                    // Message away!
                    incrementActiveInboundCount();
                    
                    // Loop for spurious wake-ups.
                    while (!mNotifyDoneCalled) {
                        try {
                            wait();
                        } catch (InterruptedException e) {
                            interrupted = true;
                            break;
                        }
                    }
                    
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer(I18n.msg(
                                "Send thread resumed. Thread {0}, message id {1}",
                                Thread.currentThread().getId(), msgId));
                    }
                }
            }catch (MQMessagingException e) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MINOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2004: Message {0} could not be sent.",
                                e.getMsgGuid()),
                        e);
            } catch (NormalizationException e) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_WARNING,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg(
                                "2006: Message could not be sent due to a normalization error."),
                        e);
            } finally {
                mNotifyDoneCalled = false;
                
                if (msgId != null) {
                    decrementActiveInboundCount();
                    completed = true;
                }

                if (!completed) {
                    // Getting here means message delivery did not succeed.
                    // If another attempt is permissible, pause for
                    // the specified amount of time.
                    if (attempt < RETRIES)  {
                        try {
                            Thread.sleep((long) mRedeliverySpec.getDelay());
                        } catch (InterruptedException e) {
                            interrupted = true;
                        }
                    }
                }
            }
        }

        if (completed) {
            endTransactionWithSuccess();
        } else {
            // Retain message in source queue only if redirect is wanted,
            // but it failed.
            try {
                if (mDoReroute) {
                    reroute(qMsg);
                    endTransactionWithSuccess();
                } else {
                    endTransactionWithRollback();
                }
            } catch (Exception e) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2023: Failed to reroute undelivered message"
                                + " intended for queue ''{0}'', to queue ''{1}''",
                                mQname,
                                mRedeliverySpec.getTarget()),
                        e);
                endTransactionWithRollback();
            }
        }
        
        probeWireToNMR.end();
    }

    private void reroute(ExtServiceMQMessage message)
            throws Exception {

        // Access the queue.
        QueueAccessOptions qoption = new QueueAccessOptions();
        qoption.setMQOO_OUTPUT(true);
        mClntAgnt.accessQueue(mRedeliverySpec.getTarget(), qoption.getOptions(), null);

        // Put the message
        PMO pmo = new PMO();
        mClntAgnt.putMessage(message, pmo);
    }

    private boolean connectAgent() {
        boolean success = true;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Attempting to connect to MQ..."));
        }
        try {
            mClntAgnt.connect();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE,
                    NotificationEvent.SEVERITY_TYPE_MINOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2001: Connection failed"
                            + " to queue manager {0}, queue {1}",
                            mQMgrname,
                            mQname));
            success = false;
        }
        return success;
    }
    
    private void disconnectAgent() {
        mClntAgnt.invalidate();
    }

    private String sendMessageExchange(ExtServiceMQMessage qMsg)
            throws MQMessagingException, NormalizationException {
        Source input = mNormalizer.normalize(qMsg,
                mOperationFullQName,
                mEndpoint,
                true
        );

        // A unique message ID for each message (not message exchange)
        // is required for Redelivery.  Retries will not work without it.
        // Group ID is optional, however.
        String msgId = generateGuid(qMsg);
        
        BaseExchangeTemplates template = new BaseExchangeTemplates(mServiceEndpoint,
                mOperationFullQName,
                true,
                null, // Group ID
                msgId,
                input,
                mMsgExchangeFactory);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Exchange template created for endpoint {0} / operation {1} / message {2}",
                    mServiceEndpoint.getServiceName().toString(),
                    mOperationFullQName.toString(), msgId));
        }

        // MESSAGE EXCHANGE-LEVEL PROPERTIES
        // Thse properties provided to the BaseExchangeTemplates
        // populate every message exchange that the BaseExchangeTemplates
        // create (i.e., for the initial exchange + retries.
        //
        // * I pass myself so I can be notified for the reply/done/error
        //   exchange status; see notifyDone().
        //
        // * If syncpoint is involved, I also pass myself so I can be
        //   informed of rollback disposition; see setSyncpointRollback().
        //
        // * If transactions are involved, Transaction object does the
        //   equivalent of the previous.
        //
        // * Service Unit passed along for endpoint suspend/resume signaling.
        Properties mexProperties = new Properties();
        mexProperties.put(EXCHANGE_CONTROL_OBJ_PROPERTY_NAME, this);
        mexProperties.put(SERVICE_UNIT_PROPERTY_NAME, mServiceUnit);
        if (mXaMode) {
            template.setTransaction(currentTx);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg(
                        "Associating Message {0} to Transaction {1}", msgId,
                        currentTx.toString()));
            }
        } else if (mSyncPoint) {
            mexProperties.put(SYNCPOINT_DISPOSITION_CONTROL_PROPERTY_NAME, this);
        }
        template.setPropExchange(mexProperties);
        
        // NORMALIZED MESSAGE-LEVEL PROPERTIES
        // Component-specific Normalized Message Properties and
        // General Normalized Message Properties.
        // http://wiki.open-esb.java.net/Wiki.jsp?page=UsingNormalizedMessageProperties
        Properties nmProperties = new Properties();
        MessageProperties mqbcProperties = new MessageProperties();
        mqbcProperties.load(mClntAgnt.getConfiguration());
        mqbcProperties.loadSyncPoint(mSyncPoint);
        nmProperties.putAll(mqbcProperties.toProperties());
        // Only org.glassfish.openesb.messaging.messageid is mandatory
        // among the general properties
        nmProperties.setProperty(ServiceQuality.PUBLIC_MESSAGE_ID, msgId);
        template.setPropNM(nmProperties);
        
        try {
            mChannel.send(template);
            mEndpoint.getEndpointStatus().incrementSentRequests();
        } catch (MessagingException e) {
            throw new MQMessagingException(msgId, e);
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Message {0} template sent."));
        }
        
        return msgId;
    }

    // >------------------------------ begin ---------------------------------->
    // Synchronization of the following methods is needed because the methods
    // can be called from the public notifyDone().
    
    // Because of the need to increment the field (read-write compound op),
    // making it volatile is not good enough.
    private int mActiveInboundCount = 0;
    
    private volatile int mInboundConcurrencyMax = 0; // 0 = unlimited
    
    private synchronized void incrementActiveInboundCount() {
        mActiveInboundCount = Math.max(0, mActiveInboundCount + 1);
    }
    private synchronized void decrementActiveInboundCount() {
        mActiveInboundCount = Math.min(0, mActiveInboundCount - 1);
    }
    private synchronized boolean isAtMaximumConcurrency() {
        // Max concurrency == 0 means unlimited.
        return mInboundConcurrencyMax != 0
                && mActiveInboundCount == mInboundConcurrencyMax;
    }
    private synchronized int getMaximumConcurrentInboundExchanges() {
        return mInboundConcurrencyMax;
    }
    // <------------------------------ end ------------------------------------<
    

    // Timer to set down the "you're being chatty if you talk right now!" flag.
    // This is used for throttling logging.
    private Timer chattyLogTimer          = new Timer();
    private long chattyPeriod             = 60000; // milliseconds
    private volatile boolean isChattyTime = false;
    {
        // This is a repeating task to terminate the timer when this
        // InboundMessageProcessor is discarded (stopReceiving() is called) 
        chattyLogTimer.scheduleAtFixedRate(new TimerTask() {
            public void run() {
                if (isStopped) {
                    chattyLogTimer.cancel();
                }
            }
        }, 0L, 3000);
    }
    
    // Marks the start of the chatty period.
    // Schedules a timed task to mark the end of the chatty period.
    private void markLogChatterTime() {
        isChattyTime = true;
        chattyLogTimer.schedule(new TimerTask() {
            public void run() {
                isChattyTime = false;
                if (isStopped) {
                    chattyLogTimer.cancel();
                }
            }
        }, chattyPeriod);
    }
    
    private boolean isPastChattyLogTime() {
        return !isChattyTime;
    }
    
    private String generateGuid(ExtServiceMQMessage qMsg) {
        byte[] id = qMsg.getMsgHeader().getMessageId();
        String guid = HexBinary.encodeBytes(id);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "MQ message GUID created (from messageId): {0}",
                    guid));
        }
        return guid;
    }


    private void locateServiceEndpoint() throws MessagingException {
        synchronized (this) {
            QName fullServiceName = mEndpoint.getServiceName();
            String endpointName = mEndpoint.getEndpointName();
            mServiceEndpoint = mContext.getEndpoint(fullServiceName, endpointName);
            if (mServiceEndpoint != null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg(
                            "Service endpoint address located for service {0}, endpoint {1}",
                            fullServiceName.toString(), endpointName));
                }
            } else {
                throw new MessagingException(I18n.msg(
                        "2007: Service {0} Endpoint {1} address not known",
                        fullServiceName.toString(),
                        endpointName));
            }
        }
    }

    private boolean isStopped() {
        return isStopped;
    }
    
    private boolean isSuspended(Endpoint endpoint) {
        // Each processor is associated with only one endpoint.
        return isSuspended;
    }

    private TransactionManager transactionManager() throws Exception {
        synchronized (txMonitor) {
            TransactionManager tm =
                    (TransactionManager) mContext.getTransactionManager();
            if (tm == null) {
                throw new Exception(I18n.msg(
                        "2009: Transaction Manager could not be found."
                                + " Verify that Transactions are in use."));
            }
            return tm;
        }
    }

    private XAResource mqResourceManager() throws Exception {
        XAResource xa;
        try {
            xa = mClntAgnt.getXAResource();
            if (xa == null) {
                throw new Exception(I18n.msg(
                        "2010: Unable to acquire the MQ resource manager"));
            }
        } catch (Exception e) {
            throw new Exception(
                    I18n.msg("2010: Unable to acquire the MQ resource manager"),
                    e);
        }
        return xa;
    }

    private void delistResource(Transaction transaction,
                                XAResource rm,
                                int options) throws Exception {

        assert transaction != null;
        assert rm != null;
        
        try {
            boolean success = transaction.delistResource(rm, options);
            if (!success) {
                throw new Exception(I18n.msg(
                        "2019: Failed to delist MQ resource {0} from Transaction {1}",
                        rm.toString(), transaction.toString()));
            }
        } catch (Exception e) {
            throw new Exception(I18n.msg(
                    "2019: Failed to delist MQ resource {0} from Transaction {1}",
                    rm.toString(), transaction.toString()), e);
        }
    }

    private void enlistResource(Transaction transaction, XAResource rm)
            throws Exception {

        assert transaction != null;
        assert rm != null;

        try {
            boolean success = transaction.enlistResource(rm);
            if (!success) {
                throw new Exception(I18n.msg(
                        "2011: Failed to enlist MQ resource {0} for Transaction {1}",
                        rm.toString(), transaction.toString()));
            }
        } catch (Exception e) {
            throw new Exception(I18n.msg(
                    "2011: Failed to enlist MQ resource {0} for Transaction {1}",
                    rm.toString(), transaction.toString()), e);
        }
    }

    /**
     * Separate monitor for transaction-related methods so 
     * there's no need to grab the object's monitor for operations
     * that are largely orthogonal to the object's state.
     */
    private final Object txMonitor = new Object();
    private Transaction currentTx = null;
    private boolean txSuspended = false;
    
    private void startTransaction() throws Exception {
        if (mXaMode) {
            synchronized (txMonitor) {
                if (currentTx != null) {
                    throw new Exception(I18n.msg(
                            "2008: New transaction cannot be started because"
                                    + " another one is active; transaction {0}",
                            currentTx.toString()));
                } else {
                    TransactionManager txManager = transactionManager();
                    try {
                        txManager.begin();
                    } catch (Exception e) {
                        throw new Exception(I18n.msg("2012: Failed to start a new transaction"), e);
                    }
                        
                    Transaction tx = txManager.getTransaction();
                    XAResource xRA = mqResourceManager();
                    enlistResource(tx, xRA);
                    currentTx = tx;
                    txSuspended = false;

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Started Transaction {0}",
                                currentTx.toString()));
                    }
                }
            }
        }
    }
    
    // suspend thread transactional context
    private void suspendTransaction() throws Exception {
        if (mXaMode) {
            synchronized (txMonitor) {
                if (!txSuspended) {
                    TransactionManager tm = transactionManager();
                    Transaction suspendedTransaction;
                    try {
                        suspendedTransaction = tm.suspend();
                    } catch (SystemException e) {
                        throw new Exception(I18n.msg(
                                "2013: Failed to suspend current transaction"),
                                e);
                    }
                    currentTx = suspendedTransaction;
                    txSuspended = true;
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Suspended Transaction {0}",
                                currentTx.toString()));
                    }
                }
            }
        }
    }
    
    // resumes current transaction on executing thread
    private void resumeTransaction() throws Exception {
        if (mXaMode) {
            synchronized (txMonitor) {
                if (txSuspended) {
                    if (currentTx != null) {
                        TransactionManager tm = transactionManager();
                        try {
                            Transaction tx = tm.getTransaction();
                            if (tx == null || tx.getStatus() == Status
                                    .STATUS_NO_TRANSACTION) {
                                tm.resume(currentTx);
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.fine(I18n.msg(
                                            "Resumed Transaction {0}",
                                            currentTx.toString()));
                                }
                            } else {
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.fine(I18n.msg(
                                            "Skipped resumption of transaction {0}, transaction {1} is active.",
                                            currentTx.toString(), tx.toString()));
                                }
                            }
                        } catch (Exception e) {
                            throw new Exception(I18n.msg(
                                    "2014: Failed resumption of transaction {0}",
                                    currentTx.toString()), e);
                        }
                    }
                    txSuspended = false;
                }
            }
        }
    }
    
    private void rollbackTransaction() throws Exception {
        if (mXaMode) {
            synchronized (txMonitor) {
                if (currentTx != null) {
                    try {
                        currentTx.rollback();
                
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Rolled back Transaction {0}",
                                    currentTx.toString()));
                        }
                    } catch (Exception t) {
                        throw new Exception(I18n.msg(
                                "2015: Failed rollback of transaction {0}",
                                currentTx.toString()), t);
                    }
                }
            }
        }
    }
    
    private void commitTransaction() throws Exception {
        if (mXaMode) {
            synchronized (txMonitor) {
                if (currentTx != null) {
                    try {
                        currentTx.commit();
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Committed Transaction {0}",
                                    currentTx.toString()));
                        }
                    } catch (Exception t) {
                        throw new Exception(I18n.msg(
                                "2016: Failed commit of transaction {0}",
                                currentTx.toString()), t);
                    }
                }
            }
        }
    }
    
    private void endSyncpoint(boolean basedOnLastExchangeOutcome) {
        if (!basedOnLastExchangeOutcome || mSyncpointRollback) {
            if (basedOnLastExchangeOutcome) {
                mSyncpointRollback = false;
            }
            try {
                mClntAgnt.backout();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Syncpoint backout executed."));
                }
            } catch (Exception e) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_CRITICAL,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2017: Failed syncpoint backout."
                                + " Loss or duplication of message may result."),
                        e);
            }
        } else {
            try {
                mClntAgnt.commit();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Syncpoint commit executed."));
                }
            } catch (Exception e) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_CRITICAL,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2018: Failed syncpoint commit."
                                + " Loss or duplication of message may result."),
                        e);
            }
        }
    }
    
    private void endTransactionWithRollback() {
        if (mXaMode) {
            synchronized (txMonitor) {
                try {
                    if (currentTx != null) {
                        resumeTransaction();
                        XAResource xaRes = mqResourceManager();
                        delistResource(currentTx, xaRes, XAResource.TMFAIL);
                        rollbackTransaction();
                    }
                } catch (Exception e) {
                    mLogger.log(Level.SEVERE,
                            NotificationEvent.SEVERITY_TYPE_CRITICAL,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            e.getLocalizedMessage(),
                            e);
                } finally {
                    currentTx = null;
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Ended Transaction {0} with rollback.",
                            currentTx.toString()));
                }
            }
        } else if (mSyncPoint) {
            endSyncpoint(false); // unconditional rollback
        }
    }
    
    private void endTransactionWithSuccess() {
        if (mXaMode) {
            synchronized (txMonitor) {
                try {
                    if (currentTx != null) {
                        resumeTransaction();
                        XAResource xaRes = mqResourceManager();
                        if (currentTx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
                            delistResource(currentTx, xaRes, XAResource.TMFAIL);
                            rollbackTransaction();
                        } else {
                            delistResource(currentTx, xaRes, XAResource.TMSUCCESS);
                            commitTransaction();
                        }
                    }
                } catch (Exception e) {
                    mLogger.log(Level.SEVERE,
                            NotificationEvent.SEVERITY_TYPE_CRITICAL,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            e.getLocalizedMessage(),
                            e);
                } finally {
                    currentTx = null;
                }
                
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Ended Transaction {0} with commit.",
                            currentTx.toString()));
                }
            }
        } else if (mSyncPoint) {
            endSyncpoint(true); // true = commit/rollback depending on exchange outcome
        }
    }

    private void initServiceQualities(List<ServiceQuality> qualities) {
        for (ServiceQuality quality : qualities) {
            if (quality instanceof ThrottlingConfig) {
                ThrottlingConfig config = (ThrottlingConfig) quality;
                mInboundConcurrencyMax = Math.max(0,
                        config.getMaxConcurrencyLimit());
            }
        }
    }

    void useSeparateTransactionBranches(boolean use) {
        synchronized (mClntAgnt) {
            mClntAgnt.enableIsSameRMWorkaround(use);            
        }
    }
    
    public void stopReceiving() {
        isStopped = true;
        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                I18n.msg("2025: Message exchange sender STOPPED" 
                        + " for endpoint {0} operation {1}",
                        mEndpoint.getEndpointName(),
                        mOperationFullQName.toString()));
    }
    
    public void suspend(Endpoint endpoint) {
        if (endpoint == mEndpoint) {
            // Each processor is associated with only one endpoint.
            isSuspended = true;
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2026: Message exchange sender SUSPENDED" 
                            + " for endpoint {0} operation {1}",
                            mEndpoint.getEndpointName(),
                            mOperationFullQName.toString()));
        }
    }
    
    public void resume(Endpoint endpoint) {
        if (endpoint == mEndpoint) {
            // Each processor is associated with only one endpoint.
            isSuspended = false;
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2027: Message exchange sender RESUMED" 
                                    + " for endpoint {0} operation {1}",
                            mEndpoint.getEndpointName(),
                            mOperationFullQName.toString()));
        }
    }
    
    public void setSyncpointRollback() {
        mSyncpointRollback = true;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("Syncpoint rollback set.");
        }
    }
    
    public synchronized void notifyDone() {
        mNotifyDoneCalled = true;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("notifyDone() called");
        }
        notify();
    }
    
    public void run() {
        mLogger.log(Level.INFO,
                NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STARTING,
                I18n.msg("2022: Message Exchange sender started"
                        + " for service {0}, endpoint {1}.",
                        mEndpoint.getServiceName(),
                        mEndpoint.getEndpointName()));

        Long pollingInterval = mOperation.getPollingInterval();
        
        final String POLL_MSG = I18n.msg(
                "Poll complete. Pausing {0} milliseconds before next cycle.",
                pollingInterval.toString());
        
        while (!isStopped()) {
            execute();
            
            if(pollingInterval > 0) {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer(POLL_MSG);
                }
                try {
                    Thread.sleep(pollingInterval);
                } catch (InterruptedException e) {
                    // Fall-thru
                }
            }
        }

        mLogger.log(Level.INFO,
                NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                I18n.msg("2024: Work loop ending."));
        
        disconnectAgent();
    }
    
    /**
     * Handles {@link javax.jbi.messaging.MessagingException}s that occur when a
     * {@link javax.jbi.messaging.MessageExchange} cannot be sent on the {@link
     * javax.jbi.messaging.DeliveryChannel}.
     *
     * @param error The exception that is caught or prepared by {@link
     * com.sun.jbi.common.qos.messaging.MessagingChannel}.
     * @param mex The message exchange related to the specified error.
     */
    public void handleSendFailure(MessagingException error, MessageExchange mex) {
        // Note that this listener is only notified if an async send is
        // performed (e.g., Redelivery QoS waitTime > 0).  If a synchronous
        // send fails, this listener is not involved, the send will raise
        // an exception immediately.
        //
        // This means any exception handling addition/removal/modification done
        // in this method, probably should also occur for the exception handling
        // code in the catch block(s) of wherever MessageChannel.send is called
        // (and vice versa).
        mLogger.log(Level.SEVERE,
                NotificationEvent.SEVERITY_TYPE_MAJOR,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                I18n.msg("2020: Message exchange {0} rejected by channel.",
                        mex.getExchangeId()),
                error);
        endTransactionWithRollback();
    }
}
