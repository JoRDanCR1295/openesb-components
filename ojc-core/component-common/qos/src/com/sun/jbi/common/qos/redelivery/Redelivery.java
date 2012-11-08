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
 * @(#)Redelivery.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.redelivery;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.util.Util;

/**
 * Utility for redelivery systemic quality.
 * @author Kevan Simpson
 */
public class Redelivery {

    public static final String REDELIVERY_NS = "http://www.sun.com/jbi/qos/redelivery";
    private static final String REDELIVERY_LAST_RETRY_TIME = "com.sun.jbi.common.qos.redelivery.lastRetryTime";
    private static final String REDELIVERY_TOTAL_RETRIES = "com.sun.jbi.common.qos.redelivery.totalRetries";
    private static final String REDELIVERY_REMAINING_RETRIES = "com.sun.jbi.common.qos.redelivery.remainingRetries";
    private static final String REDELIVERY_ERROR_DETAILS = "com.sun.jbi.common.qos.redelivery.errorDetails";
    private static final String REDELIVERY_FAILURE = "com.sun.jbi.common.qos.redelivery.failure";
    private static final String REDELIVERY_ORIGINAL_ENDPOINT = "com.sun.jbi.common.qos.redelivery.originalEndpoint";

    /**
     * Fetches the original endpoint destination of the specified exchange,
     * after redelivery attempts to that endpoint have failed and the exchange
     * has been redirected (such as to a dead-letter queue) <b>or</b> the
     * exchange's own endpoint, if redirection has not occurred.
     *
     * @param msg The exchange to evaluate for its original endpoint.
     * @return a {@link ServiceEndpoint} or <code>null</code>.
     */
    public static ServiceEndpoint getEndpoint(MessageExchange msg) {
        if (msg != null) {
            ServiceEndpoint endpt = (ServiceEndpoint) msg.getProperty(REDELIVERY_ORIGINAL_ENDPOINT);
            return (endpt == null) ? msg.getEndpoint() : endpt;
        }

        return null;
    }

    /**
     * Stores the original endpoint destination of the specified exchange,
     * after redelivery attempts to that endpoint have failed and the exchange
     * has been redirected, as a property on the exchange for later retrieval.
     * 
     * @param msg The exchange to evaluate for its original endpoint.
     */
    public static void setEndpoint(MessageExchange msg, ServiceEndpoint endpt) {
        if (msg != null) {
            msg.setProperty(REDELIVERY_ORIGINAL_ENDPOINT, endpt);
        }
    }

    /**
     * Fetches the unique id of a message, comprised of an exchange's
     * {@link ServiceQuality#MESSAGE_ID} and (optionally)
     * {@link ServiceQuality#GROUP_ID}, delimited with a dot ('.').
     * 
     * @param msg The exchange to evaluate for a unique id.
     * @return The unique message id or <code>null</code>.
     */
    public static String getUniqueId(MessageExchange msg) {
        if (msg == null) {
            return null;
        }

        String messageId = (String) msg.getProperty(ServiceQuality.MESSAGE_ID);
        if (Util.isEmpty(messageId)) {
            return null;
        } else {
            String groupId = (String) msg.getProperty(ServiceQuality.GROUP_ID);
            return (Util.isEmpty(groupId)) ? messageId : messageId + "." + groupId;
        }
    }

    /**
     * Sets a unique message id on the specified exchange.
     * 
     * @param msg A message exchange.
     * @param msgId A unique message id.
     */
    public static void setUniqueId(MessageExchange msg, String msgId) {
        setUniqueId(msg, msgId, null);
    }

    /**
     * Sets a unique message id, comprised of message and group ids, on the
     * specified exchange.
     * 
     * @param msg A message exchange.
     * @param msgId A message id, unique within a group.
     * @param groupId A group id.
     */
    public static void setUniqueId(MessageExchange msg, String msgId, String groupId) {
        if (msg != null && !Util.isEmpty(msgId)) {
            msg.setProperty(ServiceQuality.MESSAGE_ID, msgId);
            msg.setProperty(ServiceQuality.GROUP_ID, groupId);

            NormalizedMessage in = ExchangeUtil.getInMessage(msg);
            if (in != null) {
                in.setProperty(ServiceQuality.PUBLIC_MESSAGE_ID, msgId);
                in.setProperty(ServiceQuality.PUBLIC_GROUP_ID, groupId);
            }
        }
        // else {	// TODO do we do something here?  log?
    }

    /**
     * Instantiates a {@link RedeliveryConfig} instance.
     * 
     * @param maxRetries The maximum number of redelivery attempts.
     * @param retryInterval The interval to delay between redelivery attempts.
     * @param failure The action that occurs when redelivery attempts are exhausted.
     * @param redirect The endpoint and operation to which a message is sent or <code>null</code>.
     * @return a redelivery configuration.
     */
    public static RedeliveryConfig createRedeliveryConfig(int maxRetries, long retryInterval,
            Failure failure, Redirect redirect) {
        return new RDConfig(maxRetries,
                retryInterval,
                (failure == null) ? Failure.error : failure,
                redirect);
    }

    /**
     * Sets a redelivery-related exception on a message exchange.
     * 
     * @param msg A message exchange involved in a failed delivery attempt.
     * @param err The exception that caused redelivery to fail.
     */
    public static void setRedeliveryStatus(MessageExchange msg, Exception err) {
        if (msg != null) {
            msg.setProperty(REDELIVERY_ERROR_DETAILS, err);
        }
    }

    public static Exception getRedeliveryError(MessageExchange msg) {
        Exception err = null;
        if (msg != null) {
            err = (Exception) msg.getProperty(REDELIVERY_ERROR_DETAILS);
        }
        return err;
    }

    /**
     * Updates redelivery status associated with the specified exchange.
     * 
     * @param msg A message exchange.
     * @param timestamp The timestamp of the last redelivery attempt.
     * @param total Total number of redelivery attempts that should occur.
     * @param remaining Total number of redelivery attempts remaining.
     * @param err The most recent exception that caused redelivery to fail.
     * @param failed A flag to indicate whether redelivery has failed.
     */
    public static void setRedeliveryStatus(MessageExchange msg, long timestamp,
            int total, int remaining,
            Exception err, boolean failed) {
        if (msg != null) {
            msg.setProperty(REDELIVERY_LAST_RETRY_TIME, String.valueOf(timestamp));
            msg.setProperty(REDELIVERY_TOTAL_RETRIES, String.valueOf(total));
            msg.setProperty(REDELIVERY_REMAINING_RETRIES, String.valueOf(remaining));
            msg.setProperty(REDELIVERY_ERROR_DETAILS, err);
            msg.setProperty(REDELIVERY_FAILURE, Boolean.toString(failed));
        }
    }

    /**
     * Queries an exchange for its redelivery status.
     * 
     * @param msg A message exchange used in a redelivery attempt.
     * @return The redelivery status of the message.
     */
    public static RedeliveryStatus getRedeliveryStatus(MessageExchange msg) {
        if (msg == null) {
            return null;
        }

        // check status of redelivery for given exchange
        String guid = getUniqueId(msg);
        if (!Util.isEmpty(guid)) {
            String total = (String) msg.getProperty(REDELIVERY_TOTAL_RETRIES),
            remain = (String) msg.getProperty(REDELIVERY_REMAINING_RETRIES);

            if (!Util.isEmpty(total) && !Util.isEmpty(remain)) {
                return new RDStatus(
                        Util.parseLong((String) msg.getProperty(REDELIVERY_LAST_RETRY_TIME), -1),
                        Util.parseInt(total, -1),
                        Util.parseInt(remain, -1),
                        msg.getError(),
                        Boolean.valueOf(String.valueOf(msg.getProperty(REDELIVERY_FAILURE))).booleanValue());
            }
        }

        return null;
    }

    /* Default implementation of RedeliveryConfig. */
    private static class RDConfig implements RedeliveryConfig {

        private Redirect mRedirect;
        private int mMaxRetries;
        private long mRetryInterval;
        private Failure mFailure;

        public RDConfig(int maxRetries, long retryInterval,
                Failure failure, Redirect redirect) {
            mRedirect = redirect;
            mMaxRetries = maxRetries;
            mRetryInterval = retryInterval;
            mFailure = failure;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryConfig#getFailure() */
        public Failure getFailure() {
            return mFailure;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryConfig#getMaxRetries() */
        public int getMaxRetries() {
            return mMaxRetries;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryConfig#getRedirect() */
        public Redirect getRedirect() {
            return mRedirect;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryConfig#getRetryInterval() */
        public long getRetryInterval() {
            return mRetryInterval;
        }
    }

    /* Default implementation of RedeliveryStatus. */
    private static class RDStatus implements RedeliveryStatus {

        private int mTotal, mRemaining;
        private long mLastRetry;
        private Exception mLastError;
        private boolean mFailed;

        public RDStatus(long lastRetry, int total, int remaining,
                Exception err, boolean failed) {
            mLastRetry = lastRetry;
            mTotal = total;
            mRemaining = remaining;
            mLastError = err;
            mFailed = failed;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryStatus#getError() */
        public Exception getError() {
            return mLastError;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryStatus#getLastRetryTime() */
        public long getLastRetryTime() {
            return mLastRetry;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryStatus#getRemainingRetries() */
        public int getRemainingRetries() {
            return mRemaining;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryStatus#getTotalRetries() */
        public int getTotalRetries() {
            return mTotal;
        }

        /** @see com.sun.jbi.common.qos.redelivery.RedeliveryStatus#hasFailed() */
        public boolean hasFailed() {
            return mFailed;
        }
    }
}
