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
package com.sun.jbi.mqbc;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;
import javax.xml.namespace.QName;

import com.ibm.mq.MQException;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.mqbc.extensions.MQBCAddress;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extservices.ExtServiceMQMessage;
import com.sun.jbi.mqbc.extservices.MQBCExtServiceException;
import com.sun.jbi.mqbc.extservices.MQClientAgent;
import com.sun.jbi.mqbc.extservices.MQClientConfiguration;
import com.sun.jbi.mqbc.extservices.PMO;
import com.sun.jbi.mqbc.extservices.QueueAccessOptions;
import com.sun.jbi.mqbc.extservices.UnknownCipherSuiteException;
import com.sun.jbi.mqbc.messaging.MessageProperties;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.nms.exchange.ExchangePattern;
import net.java.hulp.measure.Probe;


final class OutboundMessageProcessor implements Runnable {
    
    private static final String ERROR_FAULTCODE = "com.sun.jbi.crl.faultcode";
    private static final String ERROR_FAULTACTOR = "com.sun.jbi.crl.faultactor";
    private static final String ERROR_FAULTSTRING = "com.sun.jbi.crl.faultstring";
    private static final String ERROR_FAULTDETAIL = "com.sun.jbi.crl.faultdetail";
    private static final String CLIENT_FAULTCODE = "Client";
    private static final String SERVER_FAULTCODE = "Server";
    private static final String VERSION_FAULTCODE = "VersionMismatch";
    
    private EventLogger mLog;
    private final DeliveryChannel mChannel;
    private final Map<String, MQClientAgent> mConnectorsMap;
    private final MQDenormalizer mDenormalizer;
    private final MQNormalizer mNormalizer;
    private final MQComponentContext mComponentContext;
    private final MQBindingDeployer mDeployer;
    private final String mId;


    /**
     * Creates a new instance of OutboundMessageprocessor
     * 
     * @param componentContext JBI context.
     * @param chnl Messaging channel to poll.
     * @param deployer Service unit manager.
     */
    OutboundMessageProcessor(MQComponentContext componentContext,
                             DeliveryChannel chnl,
                             MQBindingDeployer deployer) {
        mId = toString();
        mComponentContext = componentContext;
        mChannel = chnl;
        mDeployer = deployer;
        mConnectorsMap = Collections.synchronizedMap(new HashMap<String, MQClientAgent>());
        mDenormalizer  = new MQDenormalizer();
        mNormalizer  = new MQNormalizer();
        mLog = new EventLogger(Util.getLogger(componentContext,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);
    }
    
    public void run() {
        MessageExchange messageExchange;
        
        while (!isStopped()) {
            
            // -- Receive a message exchange --
            try {
                messageExchange = mChannel.accept(5000);
                if (messageExchange == null) {
                    if (mLog.isLoggable(Level.FINER)) {
                        mLog.finer(I18n.msg("No activity on the provider channel."));
                    }
                    continue;
                }
            } catch (MessagingException e) {
                //check if we already stopped otherwise it is bad exception
                if (!isStopped()) {
                    mLog.log(Level.SEVERE,
                            NotificationEvent.SEVERITY_TYPE_MAJOR,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            I18n.msg("2100: An error occured while attempting"
                                    + " to accept a message exchange from the provider channel."),
                            e);
                }
                continue;
            }
                
            Probe probeNmrToWire = Probe.fine(getClass(),
                    "Outbound Message Processing (NMR to wire)",
                    mId);

            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Processing message exchange {0} received from channel...",
                        messageExchange.getExchangeId()));
            }

            // -- Resolve exchange destination --
            Endpoint destination = findDestinationEndpoint(messageExchange);
            if (destination == null) {
                String msg = I18n.msg(
                        "2101: Failed to resolve the target endpoint {0}"
                                + " specified by Message exchange {1}",
                        messageExchange.getEndpoint().getServiceName().getLocalPart(),
                        messageExchange.getExchangeId());
                String detail = I18n.msg("2102: The message exchange specifies" 
                        + " an endpoint for which no registered and" 
                        + " active service endpoint exists. The service" 
                        + " endpoint may be deactivated, or the" 
                        + " consuming component may have misconfigured" 
                        + " with the wrong endpoint address");
                mLog.severe(NotificationEvent.SEVERITY_TYPE_MINOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        msg);
                returnErrorExchange(CLIENT_FAULTCODE, msg, detail, messageExchange);
                probeNmrToWire.end();
                continue;
            }

            // -- Dispatch message exchange  --
            try {
                handleExchange(destination, messageExchange);

                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("Processed message exchange {0}.",
                            messageExchange.getExchangeId()));
                }
            } catch (MessagingException e) {
                mLog.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2103: Failed to handle message exchange {0}.",
                                messageExchange.getExchangeId()),
                        e);
            } finally {
                probeNmrToWire.end();
            }
        }
        
        // Stopping - release resources
        destroyOutboundAgents();
    }
    
    private void handleExchange(Endpoint destination, MessageExchange exchange)
            throws MessagingException {
        
        assert destination != null;
        assert exchange != null;
        
        URI pattern = exchange.getPattern();
        assert pattern != null;
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer(I18n.msg("exchange {0} - pattern {1}",
                    exchange.getExchangeId(),
                    pattern));
        }
        
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_ONLY:
                handleInOnlyRequest(destination, (InOnly) exchange);
                break;
            case IN_OUT:
                handleInOutRequest(destination, (InOut) exchange);
                break;
            default:
                handleUnsupportedExchange(exchange);
                break;
        }
    }

    private void handleInOnlyRequest(Endpoint destination, InOnly exchange)
            throws MessagingException {
        assert destination != null;
        assert exchange != null;

        // In-only Message Exchange Pattern:
        // consumer sends REQUEST to provider
        // provider sends DONE to consumer
        
        if (exchange.getRole() == MessageExchange.Role.CONSUMER) {
            processReturn(destination, exchange);
        } else {
            // exchange.getRole() == MessageExchange.Role.PROVIDER)
            processRequest(destination, exchange);
        }
    }

    private void handleInOutRequest(Endpoint destination, InOut exchange)
            throws MessagingException {
        assert destination != null;
        assert exchange != null;

        // In-Out Message Exchange Pattern:
        // consumer sends REQUEST to provider
        // provider sends RESPONSE to consumer
        // consumer sends DONE to provider
        
        if (exchange.getRole() == MessageExchange.Role.CONSUMER) {
            // Handle provider response
            processReturn(destination, exchange);
        } else {
            // exchange.getRole() == MessageExchange.Role.PROVIDER)
            // Handle consumer REQUEST or DONE
            ExchangeStatus status = exchange.getStatus();
            if (status == ExchangeStatus.DONE
                    || status == ExchangeStatus.ERROR) {
                processReturn(destination, exchange);
            } else {
                processRequest(destination, exchange);
            }
        }
    }
    
    private void handleUnsupportedExchange(MessageExchange exchange) {
        assert exchange != null;
        
        URI pattern = exchange.getPattern();
        String msg = I18n.msg("2104: Rejecting message exchange {0}"
                + " - pattern {1} is unsupported! ",
                exchange.getExchangeId(),
                pattern.toString());
        String detail = I18n.msg("2105: The message exchange specifies"
                + " an unsupported exchange pattern, {0}. {1} currently"
                + " only understands how to process in-only message"
                + " exchange pattern",
                pattern.toString(),
                mComponentContext.getComponentName());
        mLog.warning(NotificationEvent.SEVERITY_TYPE_WARNING,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                msg);
        returnErrorExchange(VERSION_FAULTCODE, msg, detail, exchange);
    }
    
    private void createErrorExchange(MessageExchange exchange,
                                     String faultCode,
                                     String faultMsg,
                                     String faultDetail,
                                     Exception cause
    )
            throws
            MessagingException {

        assert exchange !=  null;
        assert faultCode != null;
        assert faultMsg != null;
        assert faultDetail != null;
        assert cause != null;
        
        if (exchange.getStatus() == ExchangeStatus.DONE) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Exchange {0} is already DONE - skipping");
            }
        } else if (exchange.getStatus() == ExchangeStatus.ERROR) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Exchange {0} is in ERROR state already - skipping");
            }
        } else {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Setting error status on message exchange {0}"
                                + " and returning it to sender.",
                        exchange.getExchangeId()
                )
                );
            }
            exchange.setStatus(ExchangeStatus.ERROR);
            exchange.setProperty(ERROR_FAULTCODE, faultCode);
            exchange.setProperty(ERROR_FAULTACTOR,
                    mComponentContext.getComponentName()
            );
            exchange.setProperty(ERROR_FAULTSTRING, faultMsg);
            exchange.setProperty(ERROR_FAULTDETAIL, faultDetail);
        }
    }

    private void createErrorExchange(ExchangeProcessingException exception)
            throws
            MessagingException {
        
        assert exception != null;
        
        MessageExchange exchange = exception.getMessageExchange();
        assert exchange != null;
        
        String faultCode = exception.getFaultCode();
        assert faultCode != null && !"".equals(faultCode.trim());
        
        String faultDetail = exception.getFaultDetail();
        assert faultDetail != null && !"".equals(faultDetail.trim());
        
        String faultMsg = exception.getFaultMessage();
        assert faultMsg != null && !"".equals(faultMsg.trim());

        createErrorExchange(exchange,
                faultCode,
                faultMsg,
                faultDetail,
                exception
        );
    }
    
    private void returnErrorExchange(String faultCode,
                                     String faultMsg,
                                     String faultDetail,
                                     MessageExchange exchange) {
        if (exchange.getStatus() == ExchangeStatus.DONE) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Exchange {0} is already DONE - skipping");
            }
        } else if (exchange.getStatus() == ExchangeStatus.ERROR) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Exchange {0} is in ERROR state already - skipping");
            }
        } else {
            try {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg(
                            "Setting error status on message exchange {0}"
                                    + " and returning it to sender.",
                            exchange.getExchangeId()));
                }
                exchange.setStatus(ExchangeStatus.ERROR);
                exchange.setProperty(ERROR_FAULTCODE, faultCode);
                exchange.setProperty(ERROR_FAULTACTOR, mComponentContext.getComponentName());
                exchange.setProperty(ERROR_FAULTSTRING, faultMsg);
                exchange.setProperty(ERROR_FAULTDETAIL, faultDetail);
                mChannel.send(exchange);
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("Message exchange {0} returned.",
                            exchange.getExchangeId()));
                }
            } catch (MessagingException e) {
                mLog.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2106: Failed to return unprocessable"
                                + " message exchange {0} to the channel!",
                                exchange.getExchangeId()),
                        e);
            }
        }
    }

    // Handles message exchanges statuses and responses.
    private void processReturn(Endpoint destination, MessageExchange exchange)
            throws MessagingException {
        assert destination != null;
        assert exchange != null;

        Probe probeReturnToNMR = Probe.fine(getClass(),
                "Status Processing (Component to NMR)",
                mId);

        String exchangeId = exchange.getExchangeId();

        ExchangeStatus status = exchange.getStatus();
        
        // No matter what happens, any exceptions must be held in abeyance
        // until the entire method executes, because even if the suspension
        // and/or rollback fails, I need to do some signaling to free
        // up waiting threads.  If I raised an exception right away,
        // the waiting threads will wait forever.
        JBIException suspendException = null;
        Exception rollbackException = null;
            
        if (status == ExchangeStatus.DONE) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Exchange {0} status DONE, ready to commit.",
                        exchangeId));
            }
        } else if (status == ExchangeStatus.ACTIVE) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Exchange {0} status ACTIVE, ignoring.",
                        exchangeId));
            }
        } else if (status == ExchangeStatus.ERROR) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Exchange {0} status ERROR, ready to rollback.",
                        exchangeId));
            }
                
            // An exchange may have failed due to a delivery failure.
            // The Redelivery QoS library would have taken care of retries.
            // If Redelivery QoS is in effect, arriving here means the
            // On Failure action is set to Error or Suspend.
            //
            // Error On Failure: Rollback the message.
            // Suspend On Failure: Suspend the endpoint and rollback the message.
            
            try {
                executeRedeliveryRecourse(destination, exchange);
            } catch (JBIException e) {
                suspendException = e;
                // Fall-thru to do the rollback.
            }

            // Rollback transaction
            QName opName = fullyQualifiedOperationName(exchange.getOperation(),
                    destination
            );
            MQBCOperation operation =
                    resolveOperation(exchange, destination, opName);
            if (operation != null && operation.getTransaction()) {
                resumeTransaction(exchange, operation);
                rollbackTransaction(exchange);
                suspendTransaction(exchange);
            } else {
                InboundMessageProcessor processor =
                        (InboundMessageProcessor) exchange.getProperty(
                                InboundMessageProcessor.SYNCPOINT_DISPOSITION_CONTROL_PROPERTY_NAME
                        );
                if (processor != null) {
                    processor.setSyncpointRollback();
                }
            }
        }

        // If operating in transactional mode, the originator of
        // the message exchange whose reply I am processing, is
        // waiting to be told the exchange is completed.
        // See InboundMessageProcessor.execute().
        wakeupMqConsumer(exchange);
        
        probeReturnToNMR.end();
        
        // Suspend + rollback presents two potential points of failure.
        // If one exception occurs, throw it.
        // If two occurs, log one, throw the other.
        // Rollback failure is more important.
        if (suspendException != null) {
            if (rollbackException != null) {
                mLog.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2119: Failed to suspend endpoint {0},"
                                + " initiated by exchange {1}",
                                destination.getServiceEndpointName().toString(),
                                exchange.getExchangeId()),
                        suspendException);
            } else {
                throw new MessagingException(I18n.msg(
                        "2119: Failed to suspend endpoint {0},"
                                + " initiated by exchange {1}",
                        destination.getServiceEndpointName().toString(),
                        exchange.getExchangeId()), suspendException);
            }
        }
        if (rollbackException != null) {
            throw new MessagingException(I18n.msg(
                    "2120: Failed to roll back message exchange {0} "
                            + "for endpoint {1}",
                    exchange.getExchangeId(),
                    destination.getServiceEndpointName().toString()),
                    rollbackException);
        }
    }
    
    private void processRequest(Endpoint destination, InOnly inonly)
            throws
            MessagingException {
        assert destination != null;
        assert inonly != null;
        
        destination.getEndpointStatus().incrementReceivedRequests();
            
        boolean error = false;
        
        // Determine operation
        QName opName = null;
        MQBCOperation operation = null;
        if (!error) {
            opName = fullyQualifiedOperationName(inonly.getOperation(),
                    destination
            );
            try {
                operation = resolveOperation(inonly, destination, opName);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }

        // Resume transaction
        final boolean isTransacted =
                (operation != null && operation.getTransaction());
        Transaction tx = null;
        if (!error) {
            if (isTransacted) {
                try {
                    tx = resumeTransaction(inonly, operation);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }

        // Connect to EIS
        MQClientAgent agent = null;
        if (!error) {
            try {
                agent = createConnection(inonly, destination, opName);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }
        
        // Denormalize message
        // Has to be done after EIS connection because container for
        // denormalized message has to be obtained from an open connection.
        ExtServiceMQMessage message = null;
        if (!error) {
            try {
                message =
                        denormalizeRequest(inonly, destination, opName, agent);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }
        
        // Enlist MQ resource manager
        if (!error) {
            if (isTransacted) {
                try {
                    enlistResource(tx, inonly, operation, agent);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Access queue
        Probe probeInWire = null;
        if (!error) {
            probeInWire = Probe.fine(getClass(),
                    "Outbound Message Processing (in-wire)",
                    mId);
            try {
                QueueAccessOptions qoption =
                        new QueueAccessOptions(operation.getQueueOpenOptions());
                qoption.setMQOO_OUTPUT(true);
                openQueue(agent, qoption, inonly, operation);
            } catch (ExchangeProcessingException e) {
                probeInWire.end();
                createErrorExchange(e);
                error = true;
            }
        }
        
        // Send the message
        final boolean isSyncpointEnabled =
                operation.getMQOperationInput().getMQMessage().getMQSyncPoint();
        if (!error) {
            try {
                enqueueMessage(agent,
                        message,
                        inonly,
                        operation,
                        isTransacted || isSyncpointEnabled
                );
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            } finally {
                probeInWire.end();
            }
        }
        
        // De-list MQ resource manager
        if (!error) {
            if (isTransacted) {
                try {
                    delistResource(tx,
                            XAResource.TMSUCCESS,
                            inonly,
                            operation,
                            agent
                    );
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Suspend transaction
        if (!error) {
            if (isTransacted) {
                try {
                    suspendTransaction(inonly);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Send DONE status
        if (!error) {
            inonly.setStatus(ExchangeStatus.DONE);
            try {
                mChannel.send(inonly);
                destination.getEndpointStatus().incrementSentDones();
            } catch (MessagingException e) {
                String faultMsg = I18n.msg("2118: Failed to return processed"
                        + " message exchange {0} to the channel.",
                        inonly.getExchangeId()
                );
                String faultDetail = I18n.msg(
                        "The component failed to send a DONE status "
                                + " for the message exchange for operation {0},"
                                + " service {1}, endpoint {2}. The channel send failed.",
                        opName.toString(),
                        destination.getServiceName().toString(),
                        destination.getEndpointName()
                );
                createErrorExchange(inonly,
                        CLIENT_FAULTCODE,
                        faultMsg,
                        faultDetail,
                        e
                );
                error = true;
            }
        }

        // Beyond this point, the DONE status has already been sent.
        // Any exceptions that occur from this point on have to be raised.
        MessagingException exceptionToRaise = null;
        
        // Commit (syncpoint, not XA)
        if (!error) {
            if (isSyncpointEnabled && !isTransacted) { // to be transacted is to have syncpoint
                try {
                    commitSyncpoint(agent, inonly);
                } catch (ExchangeProcessingException e) {
                    exceptionToRaise = e;
                    error = true;
                }
            }
        }
        
        // Final error handling.
        // Resume, Rollback, Suspend transaction (if any); and then
        // If an ERROR status is outstanding, send it; and then
        // If an exception to be thrown is specified, throw it.
        if (error) {
            // Transactional stuff
            if (isTransacted) {
                if (tx != null) {
                    try {
                        resumeTransaction(inonly, operation);
                        rollbackTransaction(inonly); // only flagging for rollback
                        suspendTransaction(inonly);
                    } catch (ExchangeProcessingException e) {
                        createErrorExchange(e);
                    }
                }
            } else {
                if (isSyncpointEnabled) {
                    if (agent != null && agent.isConnected()) {
                        try {
                            backoutSyncpoint(agent, inonly);
                        } catch (ExchangeProcessingException e) {
                            createErrorExchange(e);
                        }
                    }
                }
            }
            
            // Send error status
            if (inonly.getStatus() == ExchangeStatus.ERROR) {
                try {
                    mChannel.send(inonly);
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.fine(I18n.msg("Message exchange {0} returned.",
                                inonly.getExchangeId()
                        )
                        );
                    }
                    destination.getEndpointStatus().incrementSentErrors();
                } catch (MessagingException e) {
                    // At this point, I have failed to send a DONE,
                    // and I have failed to send an ERROR to report the
                    // failure to send a DONE!
                    //
                    // I have no other means to notify the sender of the
                    // outcome of the exchange, UNLESS the sender is the
                    // MQ BC in consumer-proxy mode. If such is the case,
                    // the sender can be awoken out-of-band to signal that the
                    // exchange is completed/terminated.
                    //
                    // This mechanism should only be used in exceptional cases,
                    // of which this is one, because failing to wake up a
                    // MQ BC-based sender under this circumstance means there's
                    // a blocked thread that will never wake up until the
                    // application server is shut down.
                    wakeupMqConsumer(inonly);
                    exceptionToRaise = e;
                }
            }
            
            // Raise exception
            if (exceptionToRaise != null) {
                throw exceptionToRaise;
            }
        }
    }
    
    private void processRequest(Endpoint destination, InOut inout)
            throws
            MessagingException {
        assert destination != null;
        assert inout != null;
        
        destination.getEndpointStatus().incrementReceivedRequests();
            
        boolean error = false;
        boolean fault = false;
        
        // Determine operation
        QName opName = null;
        MQBCOperation operation = null;
        if (!error) {
            opName = fullyQualifiedOperationName(inout.getOperation(),
                    destination
            );
            try {
                operation = resolveOperation(inout, destination, opName);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }

        // Resume transaction
        final boolean isTransacted =
                (operation != null && operation.getTransaction());
        Transaction tx = null;
        if (!error) {
            if (isTransacted) {
                try {
                    tx = resumeTransaction(inout, operation);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }

        // Connect to EIS
        MQClientAgent agent = null;
        if (!error) {
            try {
                agent = createConnection(inout, destination, opName);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }
        
        // Enlist MQ resource manager
        if (!error) {
            if (isTransacted) {
                try {
                    enlistResource(tx, inout, operation, agent);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Access queue
        Probe probeInWire = null;
        if (!error) {
            probeInWire = Probe.fine(getClass(),
                    "Outbound Message Processing (in-wire)",
                    mId);
            try {
                QueueAccessOptions qoption =
                        new QueueAccessOptions(operation.getQueueOpenOptions());
                openQueue(agent, qoption, inout, operation);
            } catch (ExchangeProcessingException e) {
                probeInWire.end();
                Throwable t = e.getCause();
                if (!(t instanceof MQBCExtServiceException)) {
                    createErrorExchange(e);
                    error = true;
                } else {
                    try {
                        createFault(inout,
                            (MQBCExtServiceException) t,
                                destination,
                                opName
                        );
                    } catch (ExchangeProcessingException e1) {
                        createErrorExchange(e1);
                        error = true;
                    }
                    fault = !error;
                }
            }
        }
        
        // Solicit a message
        final boolean isSyncpointEnabled =
                operation.getMQOperationInput().getMQMessage().getMQSyncPoint();
        ExtServiceMQMessage message = null;
        if (!error && !fault) {
            try {
                message = dequeueMessage(agent,
                        inout,
                        operation,
                        isTransacted || isSyncpointEnabled
                );
            } catch (ExchangeProcessingException e) {
                Throwable t = e.getCause();
                if (t instanceof MQBCExtServiceException) {
                    fault = true;
                    createFault(inout,
                            (MQBCExtServiceException) t,
                            destination,
                            opName
                    );
                } else {
                    createErrorExchange(e);
                    error = true;
                }
            } finally {
                probeInWire.end();
            }
        }
        
        // De-list MQ resource manager
        if (!error) {
            if (isTransacted) {
                try {
                    delistResource(tx,
                            XAResource.TMSUCCESS, inout,
                            operation,
                            agent
                    );
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Suspend transaction
        if (!error) {
            if (isTransacted) {
                try {
                    suspendTransaction(inout);
                } catch (ExchangeProcessingException e) {
                    createErrorExchange(e);
                    error = true;
                }
            }
        }
        
        // Normalize message
        if (!error && !fault) {
            try {
                normalizeResponse(inout, message, destination, opName);
            } catch (ExchangeProcessingException e) {
                createErrorExchange(e);
                error = true;
            }
        }
        
        // Send Response
        if (!error) {
            try {
                mChannel.send(inout);
                destination.getEndpointStatus().incrementSentReplies();
            } catch (MessagingException e) {
                String faultMsg = I18n.msg("2118: Failed to return processed"
                        + " message exchange {0} to the channel.",
                        inout.getExchangeId()
                );
                String faultDetail = I18n.msg(
                        "The component failed to send the response "
                                + " for the message exchange for operation {0},"
                                + " service {1}, endpoint {2}. The channel send failed.",
                        opName.toString(),
                        destination.getServiceName().toString(),
                        destination.getEndpointName()
                );
                createErrorExchange(inout,
                        CLIENT_FAULTCODE,
                        faultMsg,
                        faultDetail,
                        e
                );
                error = true;
            }
        }

        // Beyond this point, the response has already been sent.
        // Any exceptions that occur from this point on have to be raised.
        MessagingException exceptionToRaise = null;
        
        // Commit (syncpoint, not XA)
        if (!error) {
            if (isSyncpointEnabled && !isTransacted) { // to be transacted is to have syncpoint
                try {
                    commitSyncpoint(agent, inout);
                } catch (ExchangeProcessingException e) {
                    exceptionToRaise = e;
                    error = true;
                }
            }
        }
        
        // Final error handling.
        // Resume, Rollback, Suspend transaction (if any); and then
        // If an ERROR status is outstanding, send it; and then
        // If an exception to be thrown is specified, throw it.
        if (error) {
            // Transactional stuff
            if (isTransacted) {
                if (tx != null) {
                    try {
                        resumeTransaction(inout, operation);
                        rollbackTransaction(inout); // only flagging for rollback
                        suspendTransaction(inout);
                    } catch (ExchangeProcessingException e) {
                        createErrorExchange(e);
                    }
                }
            } else {
                if (isSyncpointEnabled) {
                    if (agent != null && agent.isConnected()) {
                        try {
                            backoutSyncpoint(agent, inout);
                        } catch (ExchangeProcessingException e) {
                            createErrorExchange(e);
                        }
                    }
                }
            }
            
            // Send error status
            if (inout.getStatus() == ExchangeStatus.ERROR) {
                try {
                    mChannel.send(inout);
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.fine(I18n.msg("Message exchange {0} returned.",
                                inout.getExchangeId()
                        )
                        );
                    }
                    destination.getEndpointStatus().incrementSentErrors();
                } catch (MessagingException e) {
                    // At this point, I have failed to send a response,
                    // and I have failed to send an ERROR to report the
                    // failure to send the response!
                    //
                    // I have no other means to notify the sender of the
                    // outcome of the exchange, UNLESS the sender is the
                    // MQ BC in consumer-proxy mode. If such is the case,
                    // the sender can be awoken out-of-band to signal that the
                    // exchange is completed/terminated.
                    //
                    // This mechanism should only be used in exceptional cases,
                    // of which this is one, because failing to wake up a
                    // MQ BC-based sender under this circumstance means there's
                    // a blocked thread that will never wake up until the
                    // application server is shut down.
                    wakeupMqConsumer(inout);
                    exceptionToRaise = e;
                }
            }
            
            // Raise exception
            if (exceptionToRaise != null) {
                throw exceptionToRaise;
            }
        }
    }

    private void normalizeResponse(InOut inout,
                                   ExtServiceMQMessage message,
                                   Endpoint endpoint,
                                   QName opName
    )
            throws
            ExchangeProcessingException {

        NormalizedMessage resultMsg;

        try {
            resultMsg = inout.createMessage();
        } catch (MessagingException e) {
            String faultMsg = I18n.msg("2139: Failed to create response"
                    + " message for operation {0}, exchange {1}.",
                    opName.toString(),
                    inout.getExchangeId()
            );
            String faultDetail = I18n.msg("The component is unable to allocate"
                    + " from the message exchange, a out message container."
            );
            throw new ExchangeProcessingException(inout,
                    SERVER_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }

        try {
            mNormalizer.normalize(message, resultMsg, opName, endpoint, false);
        } catch (NormalizationException e) {
            String faultMsg = I18n.msg("2140: Response for exchange {0},"
                    + " operation {1}, could not be normalized.",
                    opName.toString(),
                    inout.getExchangeId()
            );
            String faultDetail = I18n.msg(
                    "The service response could not be converted"
                            + " to an output message for the message exchange."
            );
            throw new ExchangeProcessingException(inout,
                    SERVER_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }

        try {
            inout.setOutMessage(resultMsg);
        } catch (MessagingException e) {
            String faultMsg = I18n.msg("2141: Failed to set the output message"
                    + " for operation {0}, exchange {1}.",
                    opName.toString(),
                    inout.getExchangeId()
            );
            String faultDetail = I18n.msg(
                    "The component encountered an exception"
                            + " in attempting to set the output message"
                            + " for the message exchange."
            );
            throw new ExchangeProcessingException(inout,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private ExtServiceMQMessage denormalizeRequest(InOnly exchange,
                                                   Endpoint endpoint,
                                                   QName opName,
                                                   MQClientAgent connection
    )
            throws
            ExchangeProcessingException {
        
        assert exchange != null;
        assert endpoint != null;
        assert opName != null;
        assert connection != null;
        
        ExtServiceMQMessage mqMessage;
        try {
            mqMessage = mDenormalizer.denormalize(exchange.getInMessage(),
                    opName,
                    endpoint,
                    connection
            );
        } catch (DenormalizationException e) {
            String faultMsg = I18n.msg("2115: Message cannot be denormalized");
            String faultDetail = I18n.msg("The component received a normalized "
                    + " message for endpoint {0},{1}, that it cannot"
                    + " denormalize into an MQ-specific message.",
                    endpoint.getServiceName().toString(),
                    opName.toString()
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
        return mqMessage;
    }
    
    private void createFault(InOut inout,
                             MQBCExtServiceException exception,
                             Endpoint endpoint,
                             QName opName
    )
            throws
            ExchangeProcessingException {
        
        // Init fault
        Fault fault;
        try {
            fault = inout.createFault();
        } catch (MessagingException e) {
            String faultMsg = I18n.msg("2138: Failed to create fault"
                    + " for operation {0}, exchange {1}.",
                    opName.toString(),
                    inout.getExchangeId()
            );
            String faultDetail = I18n.msg("The component is unable to allocate"
                    + " from the message exchange, a fault container."
            );
            throw new ExchangeProcessingException(inout,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }

        // Normalize
        try {
            mNormalizer.normalizeFault(fault, exception, opName, endpoint);
        } catch (NormalizationException e) {
            String faultMsg = I18n.msg("2136: Fault for exchange {0},"
                    + " operation {1}, could not be normalized.",
                    inout.getExchangeId(),
                    opName.toString()
            );
            String faultDetail = I18n.msg(
                    "The service exception could not be converted"
                            + " to a fault outcome for the message exchange."
            );
            throw new ExchangeProcessingException(inout,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }

        // Return
        try {
            inout.setFault(fault);
        } catch (MessagingException e) {
            String faultMsg = I18n.msg("2137: Failed to set the fault message"
                    + " for operation {0}, exchange {1}.",
                    opName.toString(),
                    inout.getExchangeId()
            );
            String faultDetail = I18n.msg(
                    "The component encountered an exception"
                            + " in attempting to set the fault response"
                            + " for the message exchange."
            );
            throw new ExchangeProcessingException(inout,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private void backoutSyncpoint(MQClientAgent connection,
                                  MessageExchange exchange
    )
            throws
            ExchangeProcessingException {
        
        assert connection != null;
        assert exchange != null;

        try {
            connection.backout();
        } catch (MQException e) {
            String faultMsg = I18n.msg(
                    "2127: Failed to syncpoint-backout the sent message."
            );
            String faultDetail = I18n.msg("The component encountered an error"
                    + " during the attempt to direct the target queue"
                    + " manager to backout the delivered message."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg
            );
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private void commitSyncpoint(MQClientAgent connection,
                                 MessageExchange exchange
    )
            throws
            ExchangeProcessingException {
        
        assert connection != null;
        assert exchange != null;
        
        try {
            connection.commit();
        } catch (MQException e) {
            String faultMsg = I18n.msg(
                    "2117: Failed to syncpoint-commit the sent message."
            );
            String faultDetail = I18n.msg("The component encountered an error"
                    + " during the attempt to direct the target queue"
                    + " manager to commit the delivered message."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg
            );
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private MQBCOperation resolveOperation(MessageExchange exchange,
                                           Endpoint endpoint,
                                           QName opName
    )
            throws
            ExchangeProcessingException {

        assert endpoint != null;
        Map operations = endpoint.getMQOperations();

        assert operations != null;
        assert opName != null;
        MQBCOperation operation = (MQBCOperation) operations.get(opName);
        if (operation == null) {
            String faultMsg = I18n.msg("2123: Unknown operation {2}"
                    + " for destination service {0}, endpoint {1}.",
                    endpoint.getServiceName().toString(),
                    endpoint.getEndpointName(),
                    opName
            );
            String faultDetail = I18n.msg("The specified operation {0} is not"
                    + " one of the operations defined for the service endpoint."
                    + " The problem may be due to a component configuration"
                    + " error."
            );
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail
            );
        }
        return operation;
    }

    private ExtServiceMQMessage dequeueMessage(MQClientAgent connection,
                                               InOut inout,
                                               MQBCOperation operation,
                                               boolean syncpoint
    )
            throws
            ExchangeProcessingException {
        
        assert connection != null;
        assert inout != null;
        assert operation != null;

        // User overrides via Normalized Message properties:
        {
            MessageProperties compProps = new MessageProperties();
            NormalizedMessage message = inputMessage(inout);
            if (message != null) {
                compProps.load(message);
            }
            
            // syncpoint
            Object override =
                    compProps.property(MessageProperties.PROPERTY_SYNCPOINT);
            if (override != null) {
                syncpoint = Boolean.valueOf(override.toString());
            }
        }
        
        ExtServiceMQMessage message;
        try {
            message = connection.retrieveMessage(syncpoint);
        } catch (Exception e) {
            String faultMsg = I18n.msg(
                    "2129: Failed to retrieve message from queue {0}",
                    operation.getQueueName()
            );
            String faultDetail = I18n.msg(
                    "The component encountered an error trying to retrieve"
                            + " a message from the MQ queue."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(inout,
                    SERVER_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
        return message;
    }

    private void enqueueMessage(MQClientAgent connection,
                                ExtServiceMQMessage mqMessage,
                                MessageExchange exchange,
                                MQBCOperation operation,
                                boolean syncpoint
    )
            throws
            ExchangeProcessingException {
        
        assert connection != null;
        assert mqMessage != null;
        assert exchange != null;
        assert operation != null;

        // User overrides via Normalized Message properties:
        {
            MessageProperties compProps = new MessageProperties();
            NormalizedMessage message = inputMessage(exchange);
            if (message != null) {
                compProps.load(message);
            }
            
            // syncpoint
            Object override =
                    compProps.property(MessageProperties.PROPERTY_SYNCPOINT);
            if (override != null) {
                syncpoint = Boolean.valueOf(override.toString());
            }
        }
        
        try {
            PMO pmo = new PMO();
            pmo.setMQPMO_SYNCPOINT(syncpoint);
            connection.putMessage(mqMessage, pmo);
        } catch (Exception e) {
            String faultMsg = I18n.msg(
                    "2116: Failed to put message in queue {0}",
                    operation.getQueueName()
            );
            String faultDetail = I18n.msg(
                    "The component encountered an error in"
                            + " attempting to put the message into the MQ queue."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    SERVER_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private void openQueue(MQClientAgent connection,
                           QueueAccessOptions options,
                           MessageExchange exchange,
                           MQBCOperation operation
    )
            throws
            ExchangeProcessingException {

        assert connection != null;
        assert options != null;
        assert exchange != null;
        assert operation != null;
        
        String queueName = operation.getQueueName();
        
        // User overrides via Normalized Message properties:
        {
            MessageProperties compProps = new MessageProperties();
            NormalizedMessage message = inputMessage(exchange);
            if (message != null) {
                compProps.load(message);
            }
            
            // queue name
            Object override =
                    compProps.property(MessageProperties.PROPERTY_QUEUE);
            if (override != null) {
                queueName = override.toString();
            }
        }
        
        try {
            connection.accessQueue(queueName, options.getOptions(), null);
        } catch (Exception e) {
            String faultMsg = I18n.msg("2111: Failed to access queue {0}",
                    operation.getQueueName()
            );
            String faultDetail = I18n.msg(
                    "The component encountered an error in"
                            + " attempting to access the target MQ queue."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private MQClientAgent createConnection(MessageExchange exchange,
                                           Endpoint destination,
                                           QName opName
    )
            throws
            ExchangeProcessingException {
        
        assert exchange != null;
        assert destination != null;
        assert opName !=  null;
        
        MQClientAgent agent;
        try {
            agent = createOutboundAgent(destination, opName, exchange);
        } catch (Exception e) {
            String faultMsg = I18n.msg("2114: Unable to allocate connection"
                    + " for endpoint {0},{1}",
                    destination.getServiceName().toString(),
                    opName.toString()
            );
            String faultDetail = I18n.msg("The component cannot establish a "
                    + " connection to service the message."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg
            );
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
        
        return agent;
    }

    private Transaction resumeTransaction(MessageExchange exchange,
                                          MQBCOperation operation
    )
            throws
            ExchangeProcessingException {
        
        assert exchange != null;
        assert operation != null;
        
        String faultMsg;
        String faultDetail;
        
        Transaction tx = (Transaction) exchange.getProperty(
                MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        if (tx != null) {
            try {
                resumeThreadTx(tx);
            } catch (Exception e) {
                faultMsg =
                        I18n.msg("2113: Transaction resumption failed");
                faultDetail = I18n.msg("The component encountered an error"
                        + " in the attempt to resume a transaction."
                );
                mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        faultMsg
                );
                throw new ExchangeProcessingException(exchange,
                        CLIENT_FAULTCODE,
                        faultMsg,
                        faultDetail,
                        e
                );
            }
        } else {
            if (!exchange.isTransacted()) {
                faultMsg = I18n.msg(
                        "2108: Message exchange {0} is not transacted"
                                + ", but target service operation"
                                + " is transactional.",
                        exchange.getExchangeId()
                );
                faultDetail = I18n.msg("The service operation to which the"
                        + " message exchange is directed, is transactional,"
                        + " but the message exchange itself is not."
                        + " If transacted operations are intended, then the"
                        + " component from which the message exchange"
                        + " originates is not configured correctly. If"
                        + " non-transacted message exchanges are intended,"
                        + " then the service {0} must be corrected.",
                        exchange.getEndpoint().getServiceName().toString()
                );
            } else {
                faultMsg = I18n.msg(
                        "2110: Message exchange {0} is transacted"
                                + " but has no associated transaction context!",
                        exchange.getExchangeId()
                );
                faultDetail = I18n.msg("The message exchange is"
                        + " received in a transactional environment, but a"
                        + " transaction context was not found associated"
                        + " with the exchange (MessageExchange.JTA_TRANSACTION_PROPERTY_NAME)."
                        + " The component from which this message exchange"
                        + " originated may not be configured correctly for"
                        + " transactional exchanges."
                );
            }
            mLog.severe(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail
            );
        }
        return tx;
    }

    private Transaction suspendTransaction(MessageExchange exchange)
            throws
            ExchangeProcessingException {
        
        assert exchange != null;
        
        try {
            return suspendThreadTx();
        } catch (SystemException e) {
            String faultMsg = I18n.msg("2125: Transaction suspension failed.");
            String faultDetail = I18n.msg("The component encountered an error"
                    + " in the attempt to suspend the current transaction."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private void rollbackTransaction(MessageExchange exchange)
            throws
            ExchangeProcessingException {
        
        assert exchange != null;
        
        TransactionManager tm = transactionManager();
        if (tm != null) {
            try {
                rollback(tm.getTransaction(), exchange.getExchangeId());
            } catch (Exception e) {
                String faultMsg =
                        I18n.msg("2128: Transaction mark-rollback failed.");
                String faultDetail = I18n.msg(
                        "The component encountered an error"
                                + " in the attempt to mark the transaction"
                                + " as rollback-only."
                );
                mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        faultMsg);
                throw new ExchangeProcessingException(exchange,
                        CLIENT_FAULTCODE,
                        faultMsg,
                        faultDetail,
                        e
                );
            }
        }
    }

    private void delistResource(Transaction tx,
                                int status,
                                MessageExchange exchange,
                                MQBCOperation operation,
                                MQClientAgent resource
    )
            throws
            ExchangeProcessingException {

        assert tx != null;
        assert exchange != null;
        assert operation != null;
        assert resource != null;
        assert (status == XAResource.TMSUCCESS || status == XAResource.TMSUSPEND
                || status == XAResource.TMFAIL);
        
        try {
            XAResource xar = resource.getXAResource();
            tx.delistResource(xar, status);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "MQ resource {0} delisted with transaction {1}",
                        xar.toString(),
                        tx.toString()));
            }
        } catch (Exception e) {
            String faultMsg = I18n.msg(
                    "2124: Transaction delistment of MQ resource manager failed."
            );
            String faultDetail = I18n.msg("The component encountered an error"
                    + " when it attempted to delist the target MQ"
                    + " resource from the transaction."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg);
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }
    
    private void enlistResource(Transaction tx,
                                MessageExchange exchange,
                                MQBCOperation operation,
                                MQClientAgent connection
    )
            throws
            ExchangeProcessingException {

        assert tx != null;
        assert exchange != null;
        assert operation != null;
        assert connection != null;
        
        try {
            XAResource xar = connection.getXAResource();
            tx.enlistResource(xar);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "MQ resource {0} enlisted with transaction {1}",
                        xar.toString(),
                        tx.toString()
                )
                );
            }
        } catch (Exception e) {
            String faultMsg = I18n.msg(
                    "2109: Transaction enlistment of MQ resource manager failed."
            );
            String faultDetail = I18n.msg(
                    "The component encountered an error"
                            + " when it attempted to enlist the target MQ"
                            + " resource into the transaction."
            );
            mLog.warning(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    faultMsg
            );
            throw new ExchangeProcessingException(exchange,
                    CLIENT_FAULTCODE,
                    faultMsg,
                    faultDetail,
                    e
            );
        }
    }

    private void executeRedeliveryRecourse(Endpoint destination,
                                           MessageExchange exchange
    )
            throws
            JBIException {

        ServiceEndpoint serviceEndpoint = Redelivery.getEndpoint(exchange);
        
        EndpointInfo endpointInfo = EndpointInfo.valueOf(
                serviceEndpoint,
                false); // false = consumer endpoint
        
        RedeliveryConfig config =
                mComponentContext.getMessagingChannel().getServiceQuality(
                        endpointInfo, RedeliveryConfig.class);
        
        if (config != null
                && config.getFailure() == RedeliveryConfig.Failure.suspend) {
            // Suspend endpoint.
            final ServiceUnit serviceUnit = (ServiceUnit) exchange.getProperty(
                    InboundMessageProcessor.SERVICE_UNIT_PROPERTY_NAME);
            if (serviceUnit != null) {
                mLog.warning(NotificationEvent.SEVERITY_TYPE_WARNING,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("2107: Suspending endpoint {0}"
                                + " as directed by redelivery configuration."
                                + " Message exchange {1} is the initiator",
                                destination.getServiceEndpointName().toString(),
                                exchange.getExchangeId()));
                serviceUnit.suspend(destination);
            } else {
                throw new JBIException(I18n.msg(
                        "2126: Cannot suspend endpoint {0}"
                                + " as directed by redelivery configuration,"
                                + " because initiator message exchange {1}"
                                + " lacks a service unit reference",
                        destination.getServiceEndpointName().toString(),
                        exchange.getExchangeId()));
            }
        }
    }
    
    private void rollback(Transaction tx, String exchangeId)
            throws
            Exception {
        assert exchangeId != null;

        TransactionManager tm = transactionManager();

        if (tx != null && tm != null) {
            try {
                Transaction currTx = tm.getTransaction();
                if (currTx == null) {
                    resumeThreadTx(tx);
                }
                tx.setRollbackOnly();
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer(I18n.msg("Transaction {0} set to rollback due to"
                            + " a message exchange processing error.",
                            tx.toString()
                    )
                    );
                }
            } catch (Exception e) {
                mLog.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg(
                                "2112: Error occured while processing message exchange {0}",
                                exchangeId
                        ),
                        e
                );
                throw e;
            }
        }
    }
    
    // resume thread transactional context
    private void resumeThreadTx(Transaction tx) throws Exception {
        TransactionManager tm = transactionManager();
        if (tx != null && tm != null) {
            Transaction currentTx = tm.getTransaction();
            if (currentTx == null || currentTx != tx) {
                tm.resume(tx);
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("Transaction {0} resumed.",
                            tx.toString()
                    )
                    );
                }
            } else {
                mLog.fine(I18n.msg(
                        "Transaction {0} not resumed - already active.",
                        tx.toString()
                )
                );
            }
        }
    }
    
    // suspend thread transactional context
    private Transaction suspendThreadTx() throws SystemException {
        // suspend only if current running transaction exists
        TransactionManager tm = transactionManager();
        Transaction tx = null;
        if (tm != null) {
            tx = tm.suspend();
            if (mLog.isLoggable(Level.FINE)) {
                if (tx !=  null) {
                    mLog.fine(I18n.msg("Transaction {0} suspended.",
                            tx.toString()));
                } else {
                    mLog.fine(I18n.msg("No transaction to suspend!"));
                }
            }
        }
        return tx;
    }
    
    private TransactionManager transactionManager() {
        return (TransactionManager) mComponentContext.getTransactionManager();
    }
    
    private void wakeupMqConsumer(MessageExchange exchange) {
        assert exchange != null;
        
        Object callback = exchange.getProperty(InboundMessageProcessor
                .EXCHANGE_CONTROL_OBJ_PROPERTY_NAME);
        if (callback != null) {
            synchronized (callback) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg(
                            "Exchange {0} has a callback - notifying callback",
                            exchange.getExchangeId()
                    )
                    );
                }

                // Do not call notify(). Its effect is indistinguishable from
                // spurious wake-ups, which are possible. notifyDone signals
                // that the wake-up is intentional.
                ((InboundMessageProcessor) callback).notifyDone();
            }
        }
    }
    
    private String clientMapKey(Endpoint destination,
                                QName opName,
                                MQClientConfiguration connectionInfo
    ) {
        assert destination != null;
        assert opName != null;
        
        final String serviceName = destination.getServiceName().toString();
        final String endpointName = destination.getEndpointName();
        final String endpointType = EndpointImpl.endpointTypeToString(
                destination.getEndpointType());

        Map ops = destination.getMQOperations();
        if (ops == null) {
            mLog.severe(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2121: Destination {0},{1} defines no usable operation.",
                            serviceName,
                            endpointName));
            return null;
        }
        
        final MQBCOperation operation = (MQBCOperation) ops.get(opName);
        if (operation == null) {
            mLog.severe(NotificationEvent.SEVERITY_TYPE_MAJOR,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("2122: Destination {0},{1} does not know operation {2}",
                            serviceName,
                            endpointName,
                            opName.toString()));
            return null;
        }
        
        
        StringBuffer keyBuffer = new StringBuffer();
        keyBuffer.append(serviceName);
        keyBuffer.append(":");
        keyBuffer.append(endpointName);
        keyBuffer.append(":");
        keyBuffer.append(endpointType);
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getHost());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getPort());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getQueueManagerName());
        keyBuffer.append(":");
        // queue name is not part of the key because an MQ connection
        // is to the queue manager, and may access any queue in the QM's scope.
        //keyBuffer.append(connectionInfo.getQueueName());
        //keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getChannelName());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getCipherSuite());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getSslPeerName());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getXAMode());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getUser());
        keyBuffer.append(":");
        keyBuffer.append(connectionInfo.getPassword());
        keyBuffer.append(":");
        keyBuffer.append(operation.getQueueOpenOptions());
        
        String key = keyBuffer.toString();
        
        if (mLog.isLoggable(Level.FINER)) {
            keyBuffer.delete(0, keyBuffer.length());
            keyBuffer.append(serviceName);
            keyBuffer.append(":");
            keyBuffer.append(endpointName);
            keyBuffer.append(":");
            keyBuffer.append(endpointType);
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getHost());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getPort());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getQueueManagerName());
            keyBuffer.append(":");
            // queue name is not part of the key because an MQ connection
            // is to the queue manager, and may access any queue in the QM's scope.
            //keyBuffer.append(connectionInfo.getQueueName());
            //keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getChannelName());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getCipherSuite());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getSslPeerName());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getXAMode());
            keyBuffer.append(":");
            keyBuffer.append(connectionInfo.getUser());
            keyBuffer.append(":");
            keyBuffer.append(operation.getQueueOpenOptions());
            mLog.finer(I18n.msg("MQAgent client map key (password omitted): {0}",
                    keyBuffer.toString()
            )
            );
        }
        
        return key;
        
    }
    
    private QName fullyQualifiedOperationName(QName name, Endpoint destination) {
        assert name != null;
        assert destination != null;
        
        String namespace = name.getNamespaceURI();
        if (namespace == null || namespace.equals("")) {
            name = new QName(
                    destination.getServiceEndpoint().getServiceName().getNamespaceURI(),
                    name.getLocalPart());
        }
        
        return name;
    }

    private MQClientAgent createOutboundAgent(Endpoint destination,
                                              QName opName,
                                              MessageExchange exchange
    )
            throws
            UnknownCipherSuiteException,
            MessagingException,
            MQBCExtServiceException {
        
        assert destination != null;

        MQClientAgent agent;

        opName = fullyQualifiedOperationName(opName, destination);
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine(I18n.msg("Begin looking for outbound agent"
                    + " to handle destination {0},{1}, operation {2}",
                    destination.getServiceName().toString(),
                    destination.getEndpointName(),
                    opName.toString()));
        }
        
        final MQBCAddress mqAddress = destination.getMQAddress();
        final MQBCOperation operation = (MQBCOperation) destination.getMQOperations().get(opName);
        
        // MQ connection configuration
        MQClientConfiguration config = new MQClientConfiguration();
        config.setQueueManagerName(mqAddress.getQueueManagerName());
        config.setQueueName(operation.getQueueName());
        config.setHost(mqAddress.getHostName());
        config.setChannelName(mqAddress.getChannelName());
        config.setCipherSuite(mqAddress.getCipherSuite());
        config.setSslPeerName(mqAddress.getSslPeerName());
        config.setXAMode(operation.getTransaction());
        config.setPort(mqAddress.getPortNumber());
        config.setUser(mqAddress.getUserName());
        config.setPassword(mqAddress.getPassword());
        config.setReceiveMode(false);

        // Override (static) configuration obtained from the service definition
        // with user-specified values carried as NormalizedMessage properties.
        MessageProperties compProps = new MessageProperties();
        NormalizedMessage message = inputMessage(exchange);
        if (message != null) {
            compProps.load(message);
            config.adopt(compProps);
        }

        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer(I18n.msg("\nqueue manager: {0}" + "\nqueue name: {1}"
                    + "\nhost: {2}" + "\nchannel: {3}" + "\nport: {4}"
                    + "\ntransacted? {5}" + "\ncipher suite: {6}"
                    + "\nssl peer name: {7}",
                    config.getQueueManagerName(),
                    config.getQueueName(),
                    config.getHost(),
                    config.getChannelName(),
                    config.getPort(),
                    config.getXAMode(),
                    config.getCipherSuite(),
                    config.getSslPeerName()));
        }

        String searchKey = clientMapKey(destination, opName, config);
        if (searchKey == null) {
            throw new MessagingException(I18n.msg("2123: Unknown operation {2}"
                    + " for destination service {0}, endpoint {1}.",
                    destination.getServiceName().toString(),
                    destination.getEndpointName(),
                    opName.toString()
            )
            );
        }
        
        synchronized (mConnectorsMap) {
            agent = mConnectorsMap.get(searchKey);
            if (agent != null) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("Found and reusing an MQ client, key {0}",
                            searchKey));
                }
            } else {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("No suitable, existing MQ client found."
                            + " Creating a new one, key {0}", searchKey));
                }
                agent = new MQClientAgent(config);
                agent.enableIsSameRMWorkaround(
                        mComponentContext.getUseSeparateTransactionBranches()
                );
                agent.connect();
                mConnectorsMap.put(searchKey, agent);
            }
        }

        return agent;
    }
    
    private NormalizedMessage inputMessage(MessageExchange exchange) {
        assert exchange != null
                && (exchange instanceof InOnly || exchange instanceof InOut);
        
        if (exchange instanceof InOnly) {
            return ((InOnly) exchange).getInMessage();
        } else {
            return ((InOut) exchange).getInMessage();
        }
    }

    private void destroyOutboundAgents() {
        synchronized (mConnectorsMap) {
            for (MQClientAgent agent : mConnectorsMap.values()) {
                agent.invalidate();
            }
            mConnectorsMap.clear();
        }
    }

    private Endpoint findDestinationEndpoint(MessageExchange exchange) {
        // When an endpoint is redirected, the QoS facility
        // must readdress the exchange before it's sent. Once set,
        // it cannot be reset with the address provided by the component.
        // To get the original address, we call Redelivery.getEndpoint.
        // This method will also handle non-redelivered exchanges transparently
        // (e.g., when you aren't using the Redelivery QoS in your implementation).
        ServiceEndpoint serviceEndpoint = Redelivery.getEndpoint(exchange);
        QName serviceName = serviceEndpoint.getServiceName();
        String endpointName = serviceEndpoint.getEndpointName();
        
        Endpoint destination = null;
        
        FIND_DESTINATION:
        for (ServiceUnit serviceUnit : mDeployer.getServiceUnits()) {
            for (Endpoint endpoint : serviceUnit.getEndpoints()) {
                if (serviceName.equals(endpoint.getServiceName())
                        && endpointName.equals(endpoint.getEndpointName())) {
                    destination = endpoint;
                    break FIND_DESTINATION;
                }
            }
        }
        return destination;
    }
    
    private volatile boolean isStopped = false;
    
    void useSeparateTransactionBranches(boolean use) {
        synchronized (mConnectorsMap) {
            for (MQClientAgent agent : mConnectorsMap.values()) {
                agent.enableIsSameRMWorkaround(use);            
            }
        }
    }
    
    
    
    public boolean isStopped() {
        return isStopped;
    }
    
    /**
     * Stop the processor thread.
     */
    public void stopReceiving() {
        isStopped = true;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine(I18n.msg("Processor stopped"));
        }
    }
}
