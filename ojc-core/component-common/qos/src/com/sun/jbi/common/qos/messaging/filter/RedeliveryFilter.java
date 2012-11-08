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
 * @(#)RedeliveryFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.filter;

import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.ExchangeTemplates;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.qos.redelivery.Redirect;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.util.Util;

/**
 * A {@link ExchangeFilter} implementation for redelivery.
 * @author Kevan Simpson
 */
public class RedeliveryFilter extends AbstractExchangeFilter {
    private static final Logger mLog = Logger.getLogger(RedeliveryFilter.class.getName());
    private static final String LOOPBACK_ENDPOINT_NAME = "redeliveryLoopback";
    private static final int INFINITE_RETRIES = -1;
    private ServiceEndpoint mLoopbackEndpt;
    private Map<String, RedeliveryStatus> mStatusMap;
    // for timestamping
    private Calendar mCal = Calendar.getInstance();
    private Timer mTimer;
    private volatile boolean mClosed = false;
    private BaseMessagingChannel mChannel;

    public static RedeliveryFilter createRedeliveryFilter(BaseMessagingChannel base)
            throws MessagingException {
        try {
            // activate loopback endpoint
            ServiceEndpoint loopbackEndpt = base.getContext().activateEndpoint(new QName(Redelivery.REDELIVERY_NS,
                    base.getContext().getComponentName()),
                    LOOPBACK_ENDPOINT_NAME);
            return new RedeliveryFilter(base, loopbackEndpt);
        } catch (JBIException jbi) {
            String msg = I18n.loc(
                    "QOS-6006: Failed to activate redelivery endpoint: {0}",
                    jbi.getMessage());
            Logger.getLogger(RedeliveryFilter.class.getName()).log(Level.WARNING, msg, jbi);
            throw new MessagingException(msg, jbi);
        }
    }

    protected RedeliveryFilter(BaseMessagingChannel base, ServiceEndpoint loopback) {
        super(base);
        mChannel = base;
        mLoopbackEndpt = loopback;
        mStatusMap = Collections.synchronizedMap(new HashMap<String, RedeliveryStatus>());
        mTimer = new Timer(base.getContext().getComponentName() + "-MessagingChannelTimer");
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#acceptsServiceQuality(com.sun.jbi.common.qos.ServiceQuality) */
    protected boolean acceptsServiceQuality(ServiceQuality srvcQual) {
        return (srvcQual instanceof RedeliveryConfig);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#close() */
    @Override
    public void close() {
        mClosed = true;
        if (mTimer != null) {
            mTimer.cancel();
            mTimer = null;
        }
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processIncomingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processIncomingExchange(MessageExchange msg, Object... params)
            throws MessagingException {
        String guid = Redelivery.getUniqueId(msg);
        
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("incoming GUID " + guid + " for msg " + msg);
        }

        if (!Util.isEmpty(guid)) {
            RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);
            RedeliveryConfig config = (RedeliveryConfig) getServiceQuality(msg);
            if (config != null && status != null) {
                if (mLoopbackEndpt.equals(msg.getEndpoint())) {
                    return processLoopbackMessage(guid, msg, config, status);
                } // update the status with error, if present
                else if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
                    //Re-factorred as it is not possible to set property on inactive MEx
                    //Redelivery.setRedeliveryStatus(msg, msg.getError());
                    // updated msg, now update status map
                    //status = Redelivery.getRedeliveryStatus(msg);

                    if (status.hasFailed()) {
                        mStatusMap.remove(guid);
                        mChannel.removeTemplates(guid);
                    } else {
                        // update internal status cache
                        mStatusMap.put(guid, status);
                        //When ExchangeTemplates is set\
                        ExchangeTemplates et = getBase().getTemplates(guid);
                        if (et != null) {
                            //create the message and send it
                            getBase().sendMessage(et.createExchange());
                            //returning null would make the accept next msg from NMR
                            return null;
                        }
                    }
                } else if (!msg.getRole().equals(Role.PROVIDER)) {
                    // ACTIVE or DONE indicate successful invocation
                    mStatusMap.remove(guid);
                    mChannel.removeTemplates(guid);
                    // TODO log removal of status at FINEST
                }
            }
        }

        return msg;
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processOutgoingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processOutgoingExchange(MessageExchange msg, Object... params)
            throws MessagingException {
        String guid = Redelivery.getUniqueId(msg);
        
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("outgoing GUID " + guid + " for msg " + msg);
        }
        
        if (!Util.isEmpty(guid)) {
            RedeliveryConfig config = (RedeliveryConfig) getServiceQuality(msg);
            if (config == null) {
                mChannel.removeTemplates(guid);
                return msg;
            }

            long ts = mCal.getTimeInMillis();   // timestamp for this send()
            RedeliveryStatus status = mStatusMap.get(guid);
            if (status != null) {
                long lastRetryTime = status.getLastRetryTime();
                // more tries?
                boolean infiniteRetries = INFINITE_RETRIES == config.getMaxRetries();
                if (status.getRemainingRetries() > 0 || infiniteRetries) {
                    int attempts = status.getTotalRetries(),
                            remaining = (infiniteRetries) ? Integer.MAX_VALUE // infinite left
                            : status.getRemainingRetries() - 1;
                    if (infiniteRetries && attempts == Integer.MAX_VALUE) {
                        attempts = 0;   // reset and warn user
                        log().warning(I18n.loc(
                                "QOS-6056: Message Exchange {0} has been sent unsuccessfully {1} times; resetting redelivery counter.",
                                msg.getExchangeId(), String.valueOf(Integer.MAX_VALUE)));
                    }
                    Redelivery.setRedeliveryStatus(msg,
                            ts,
                            attempts + 1, // how many done plus this one
                            remaining, // how many left
                            status.getError(),
                            false);  // hasn't failed yet
                    // then send, check retry interval first
                    if (config.getRetryInterval() > 0) {
                        // has enough time elapsed?
                        long elapsed = ts - lastRetryTime;
                        if (config.getRetryInterval() > elapsed) {
                            Long wait = ((Long) params[0]);
                            SendTask task = new SendTask(this, msg, wait,
                                    config.getRetryInterval() - elapsed);
                            mTimer.schedule(task, task.getDelay());
                            return null;
                        }
                    }
                } else if (msg.getRole().equals(Role.CONSUMER)) {	// apply on-failure action
                    mChannel.removeTemplates(guid);
                    Redelivery.setRedeliveryStatus(msg,
                            status.getLastRetryTime(),
                            status.getTotalRetries(),
                            status.getRemainingRetries(),
                            status.getError(), true);
                    switch (config.getFailure()) {
                        case redirect: {
                            if (msg instanceof InOnly) {
                                Redirect redirect = config.getRedirect();
                                if (redirect != null) {	// config cannot be null
                                    ServiceEndpoint endpt = getBase().getContext().getEndpoint(
                                            redirect.getEndpoint().getServiceName(),
                                            redirect.getEndpoint().getEndpointName());
                                    if (endpt == null) {
                                        String err = I18n.loc(
                                                "QOS-6001: Redelivery to error endpoint aborted, endpoint not active.");
                                        log().warning(err);
                                        throw new MessagingException(err);
                                    }
                                    // set the original endpoint for later retrieval...
                                    Redelivery.setEndpoint(msg, msg.getEndpoint());
                                    // ...and redirect the exchange to the error endpoint
                                    msg.setEndpoint(endpt);
                                    if (redirect.getOperation() != null) {
                                        msg.setOperation(redirect.getOperation());
                                    }
                                } else {
                                    String err = I18n.loc(
                                            "QOS-6002: Redelivery to error endpoint aborted, no configured endpoint.");
                                    log().warning(err);
                                    throw new MessagingException(err);
                                }
                            } else {
                                throw unsupportedRedeliveryPattern(
                                        msg, Failure.redirect);
                            }
                            break;
                        }
                        case error:
                        case suspend: {
                            if (msg instanceof InOnly || msg instanceof InOut) {
                                // start the loopback process
                                prepareLoopback(guid, msg);
                                break;
                            } else {
                                throw unsupportedRedeliveryPattern(
                                        msg, config.getFailure());
                            }
                        }
                        case delete: {
                            if (msg instanceof InOnly) {
                                // start the loopback process
                                prepareLoopback(guid, msg);
                                break;
                            } else {
                                throw unsupportedRedeliveryPattern(
                                        msg, config.getFailure());
                            }
                        }
                    }
                }
            } else {	// first time sending, look for config
                if (config != null) {
                    Redelivery.setRedeliveryStatus(msg,
                            ts,
                            0, // haven't redelivered yet
                            config.getMaxRetries(),
                            null,
                            false);
                    status = Redelivery.getRedeliveryStatus(msg);
                    mStatusMap.put(guid, status);
                }
            }
        }

        return msg;
    }

    /**
     * Processes exchanges routed through this channel's internal loopback endpoint.
     *
     * @param guid The unique message id.
     * @param msg The exchange instance.
     * @param config The non-<code>null</code> redelivery configuration.
     * @param status The non-<code>null</code> redelivery status of the specified message.
     * @return The next accepted message exchange.
     * @throws MessagingException if an error occurs processing exchange or
     *                            redelivery configuration is invalid.
     */
    protected MessageExchange processLoopbackMessage(String guid, MessageExchange msg,
            RedeliveryConfig config, RedeliveryStatus status)
            throws MessagingException {
        if (Role.PROVIDER.equals(msg.getRole())) {  // provisioner of loopback
            //get error from last attempt
            Exception lastAttemptError = Redelivery.getRedeliveryError(msg);

            String err = null;
            ExchangeStatus exchangeStatus = ExchangeStatus.ERROR;
            // validation of failure to exchange pattern
            switch (config.getFailure()) {
                case delete: {
                    if (msg instanceof InOnly) {
                        err = I18n.loc(
                                "QOS-6009: Redelivery attempts exhausted, delete message: {0} for endpoint {1}, error on last attempt: {2}",
                                msg.getExchangeId(), Redelivery.getEndpoint(msg), lastAttemptError);
                        exchangeStatus = ExchangeStatus.DONE;
                    }
                    break;
                }
                case suspend: {
                    if (msg instanceof InOnly || msg instanceof InOut) {
                        err = I18n.loc(
                                "QOS-6008: Redelivery attempts exhausted, suspend message: {0} for end point {1}, error on last attempt: {2}",
                                msg.getExchangeId(), Redelivery.getEndpoint(msg), lastAttemptError);
                    }
                    break;
                }
                case error: {
                    if (msg instanceof InOnly || msg instanceof InOut) {
                        err = I18n.loc(
                                "QOS-6019: Redelivery attempts exhausted, send aborted for message exchange: {0} for end point {1}, error on last attempt: {2}",
                                msg.getExchangeId(), Redelivery.getEndpoint(msg), lastAttemptError);
                    }
                    break;
                }
                default: {
                    err = I18n.loc(
                            "QOS-6007: Unsupported failure option {0} for exchange: {1} for end point {2}, error on last attempt: {3}",
                            String.valueOf(config.getFailure()), msg.getExchangeId(), Redelivery.getEndpoint(msg), lastAttemptError);
                    log().warning(err);
                    throw new MessagingException(err);
                }
            }

            if (err == null) {
                throw unsupportedRedeliveryPattern(msg, config.getFailure());
            }

            if (log().isLoggable(Level.FINER)) {
                log().finer(err);
            }
            // set delete error on exchange's redelivery status properties
            // set status on exchange, per systemic quality requirement
            msg.setStatus(exchangeStatus);
            if (exchangeStatus.equals(ExchangeStatus.ERROR)) {
                Exception ex = new Exception(err, lastAttemptError);
                Redelivery.setRedeliveryStatus(msg, ex);
                msg.setError(ex);
            } else {
                Redelivery.setRedeliveryStatus(msg, null);
            }
            // send message back, making it a consumer (i.e. the loopback)
            getBase().getDeliveryChannel().send(msg);
            //by return null here, it would process next MEx from MessagingChannel 
            return null;
        } else {  // Consumer role, we can pass msg back to client
            // remove status
            mStatusMap.remove(guid);
            return msg;
        }
    }

    protected void prepareLoopback(String guid, MessageExchange msg) throws MessagingException {
        // probably don't need this isActive check...
        if (ExchangeStatus.ACTIVE.equals(msg.getStatus())) {
            // start the loopback process...store actual service endpoint...
            Redelivery.setEndpoint(msg, msg.getEndpoint());
            // ...prior to sending exchange to loopback
            msg.setEndpoint(mLoopbackEndpt);
        }
    }

    boolean isClosed() {
        return mClosed;
    }

    private static class SendTask extends TimerTask {

        private final RedeliveryFilter mFilter;
        private final MessageExchange mMessage;
        private final Long mWait;
        private final long mDelay;

        public SendTask(RedeliveryFilter filter, MessageExchange msg,
                Long wait, long delay) {
            mFilter = filter;
            mMessage = msg;
            mWait = wait;
            mDelay = delay;
        }

        /** @see java.util.TimerTask#run() */
        public void run() {
            boolean done = false;
            while (!done) {
                try {
                    if (mFilter.isClosed()) {
                        // log...
                        if (mFilter.log().isLoggable(Level.FINE)) {
                            mFilter.log().log(Level.FINE, I18n.format(
                                    "QOS-3002: DeliveryChannel is closed, aborting further retry attempts for {0}",
                                    Redelivery.getEndpoint(mMessage)));
                        }
                        done = true;
                        break;
                    } else if (!isQualityInstalled()) {
                        // log...
                        if (mFilter.log().isLoggable(Level.FINE)) {
                            mFilter.log().log(Level.FINE, I18n.format(
                                    "QOS-3003: Service Quality has been uninstalled, aborting further retry attempts for {0}",
                                    Redelivery.getEndpoint(mMessage)));
                        }
                        done = true;
                        break;
                    }
                    if (mWait == FilterBase.ASYNC_SEND) {
                        mFilter.getBase().getDeliveryChannel().send(mMessage);
                        done = true;    // we be done!
                    } else if (mWait > 0) {
                        done = mFilter.getBase().getDeliveryChannel().sendSync(mMessage, mWait);
                    } else {
                        done = mFilter.getBase().getDeliveryChannel().sendSync(mMessage);
                    }
                } catch (MessagingException me) {
                    // log...
                    if (mFilter.log().isLoggable(Level.FINE)) {
                        mFilter.log().log(Level.FINE,
                                I18n.loc("QOS-6020: Attempting redelivery for {0}, send failed: {1}",
                                mMessage.getExchangeId(), me.getMessage()),
                                me);
                    }

                    // update redelivery status, notify listeners
                    done = updateStatus(mMessage);
                    mFilter.getBase().notifyListeners(me, mMessage);
                }
            }
        }

        protected boolean isQualityInstalled() {
            return (mFilter.getServiceQuality(mMessage) != null);
        }

        protected boolean updateStatus(MessageExchange msg) {
            String guid = Redelivery.getUniqueId(msg);
            if (!Util.isEmpty(guid)) {
                RedeliveryConfig config = (RedeliveryConfig) mFilter.getServiceQuality(msg);
                if (config != null) {
                    long ts = mFilter.mCal.getTimeInMillis();   // timestamp for this send()
                    RedeliveryStatus status = mFilter.mStatusMap.get(guid);
                    if (status != null) {
                        long lastRetryTime = status.getLastRetryTime();
                        // more tries?
                        if (status.getRemainingRetries() > 0) {
                            Redelivery.setRedeliveryStatus(msg,
                                    ts,
                                    status.getTotalRetries() + 1, // how many done
                                    status.getRemainingRetries() - 1, // how many left
                                    status.getError(),
                                    false);  // hasn't failed yet
                            // then send, check retry interval first
                            if (config.getRetryInterval() > 0) {
                                // has enough time elapsed?
                                long elapsed = ts - lastRetryTime;
                                if (config.getRetryInterval() > elapsed) {
                                    try {
                                        Thread.sleep(config.getRetryInterval() - elapsed);
                                    } catch (Exception e) { /* ignore */ }
                                    return false;
                                }
                            } else {
                                return true;
                            }
                        }
                    }
                }
            }
            return false;   // try again
        }

        public long getDelay() {
            return mDelay;
        }
    }
}
