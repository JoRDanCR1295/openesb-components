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
 * @(#)ThrottlingFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.filter;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingLeakyConfig;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Logger;

/**
 * 
 * @author Kevan Simpson
 */
public class ThrottlingFilter extends AbstractExchangeFilter {

    private Map<EndpointInfo, ThrottlingEndpointInfo> mEndptThrottleMap;
    private static Timer timer;

    static {
        timer = new Timer("Leaky Bucket Throttling Timer", true);
    }

    /**
     * @param base
     */
    public ThrottlingFilter(FilterBase base) {
        super(base);
        mEndptThrottleMap = new HashMap<EndpointInfo, ThrottlingEndpointInfo>();
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#acceptsServiceQuality(com.sun.jbi.common.qos.ServiceQuality) */
    protected boolean acceptsServiceQuality(ServiceQuality srvcQual) {
        return (srvcQual instanceof ThrottlingConfig);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processIncomingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processIncomingExchange(MessageExchange msg,
                                                   Object... params)
            throws MessagingException {
//        Logger.getLogger("throttling").info(
//                "We're going to process INCOMING (" + msg.getExchangeId() + ")");

        if (msg.getRole().equals(MessageExchange.Role.PROVIDER))
            return msg;

        ServiceEndpoint endpt = msg.getEndpoint();
        EndpointInfo info = EndpointInfo.valueOf(endpt, false);

        ThrottlingEndpointInfo throttling = mEndptThrottleMap.get(info);
        if (throttling != null && !(throttling instanceof ThrottlingLeakyEndpointInfo)) {
//            Logger.getLogger("throttling").info(
//                    "Checking if we have someting to release");
            MessageExchange throttled = throttling.release();
            if (throttled != null)
                try {
                    Logger.getLogger("throttling").info(
                            "Trying to send (" + throttled.getExchangeId() + ")");
                    getBase().sendMessage(throttled);
                } catch (MessagingException e) {
                    Logger.getLogger("throttling").info(
                            "Error at sending (" + throttled.getExchangeId() + "): " + e);
                    getBase().notifyListeners(e, throttled);
                }
        }

        return msg;
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processOutgoingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processOutgoingExchange(MessageExchange msg,
                                                   Object... params)
            throws MessagingException {
//        Logger.getLogger("throttling").info(
//                "We're going to process OUTGOING (" + msg.getExchangeId() + ")");
        /*
         * this throttling helps protecting downstream components. If consumer puts
         * too many MEx it still could run into OutofMemory. Inbound components/BCs needs to
         * use protocal specific way to limit the load.
         */
        if (msg.getRole().equals(MessageExchange.Role.PROVIDER))
            return msg;
        else if (!msg.getStatus().equals(ExchangeStatus.ACTIVE))
            return msg;

        ThrottlingConfig config = (ThrottlingConfig) getServiceQuality(msg);
        if (config == null)
            return msg;

        ServiceEndpoint endpt = msg.getEndpoint();
        EndpointInfo info = EndpointInfo.valueOf(endpt, false);

        ThrottlingEndpointInfo throttling = mEndptThrottleMap.get(info);
        if (throttling == null) {
            if (config instanceof ThrottlingLeakyConfig) {
                long delay =
                        1000000000 / ((ThrottlingLeakyConfig) config).
                        getLeakRate();
                throttling = new ThrottlingLeakyEndpointInfo(delay);
            } else
                throttling = new ThrottlingEndpointInfo();
            mEndptThrottleMap.put(info, throttling);
        }

        return throttling.throttle(msg, config);
    }

    private static class ThrottlingEndpointInfo {

        long outstandingMEx = 0;
        private Queue<MessageExchange> waitingMEx =
                new LinkedList<MessageExchange>();

        synchronized MessageExchange throttle(MessageExchange msg, ThrottlingConfig config) {
            outstandingMEx++;
            int limit = config.getMaxConcurrencyLimit();
            boolean alreadyThrottled =
                    msg.getProperty(
                    "com.sun.jbi.common.qos.throttling.throttled") == null ? false : (Boolean) msg.
                    getProperty("com.sun.jbi.common.qos.throttling.throttled");
            if (!alreadyThrottled)
                if (outstandingMEx > limit && limit > 0) {
                    waitingMEx.add(msg);
                    Logger.getLogger("throttling").info("Msg throttled. (" + msg.
                            getExchangeId() + ")");
                    return null;
                } else
                    Logger.getLogger("throttling").info("No need to throttle, skipping (" + msg.
                            getExchangeId() + ")");
            else {
                Logger.getLogger("throttling").info("Msg already throttled, skipping (" + msg.
                        getExchangeId() + ")");
                msg.setProperty("com.sun.jbi.common.qos.throttling.throttled",
                        false);
            }
            return msg;
        }

        synchronized MessageExchange release() {
            outstandingMEx--;
            if (waitingMEx.size() > 0) {
                MessageExchange msg = waitingMEx.remove();
                msg.setProperty("com.sun.jbi.common.qos.throttling.throttled",
                        true);
                Logger.getLogger("throttling").info(
                        "Msg released (" + msg.getExchangeId() + ")");
                return msg;
            }
            return null;
        }
    }

    private class ThrottlingLeakyEndpointInfo extends ThrottlingEndpointInfo {

        long lastRelease;
        long delay;
        private Queue<MessageExchange> waitingMEx =
                new LinkedList<MessageExchange>();

        public ThrottlingLeakyEndpointInfo(long delay) {
            this.delay = delay;
        }

        synchronized MessageExchange throttle(MessageExchange msg, ThrottlingConfig config) {
            boolean alreadyThrottled =
                    msg.getProperty(
                    "com.sun.jbi.common.qos.throttling.throttled") == null ? false : (Boolean) msg.
                    getProperty("com.sun.jbi.common.qos.throttling.throttled");
            if (alreadyThrottled) {
                Logger.getLogger("throttling").info("Msg already throttled, skipping (" + msg.
                        getExchangeId() + ")");
                msg.setProperty("com.sun.jbi.common.qos.throttling.throttled",
                        false);
                return msg;
            } else {
                long currentTime = System.nanoTime();
                if (currentTime - lastRelease > delay && waitingMEx.isEmpty()) {
                    lastRelease = currentTime;
                    Logger.getLogger("throttling").info("No need to throttle, skipping (" +
                            msg.getExchangeId() + ")");
                    return msg;
                } else {
                    Logger.getLogger("throttling").info("Msg throttled. (" +
                            msg.getExchangeId() + ")");
                    waitingMEx.add(msg);
                    timer.schedule(new LeakyBucketTimerTask(),
                            (delay * waitingMEx.size() - currentTime + lastRelease) / 1000000);
                    return null;

                }
            }
        }

        synchronized MessageExchange release() {
            if (waitingMEx.size() > 0) {
                lastRelease = System.nanoTime();
                MessageExchange msg = waitingMEx.remove();
                msg.setProperty("com.sun.jbi.common.qos.throttling.throttled",
                        true);
                Logger.getLogger("throttling").info(
                        "Msg released (" + msg.getExchangeId() + ")");
                return msg;
            }

            return null;
        }
    }

    private class LeakyBucketTimerTask extends TimerTask {

        @Override
        public void run() {
            for (ThrottlingEndpointInfo throttling : mEndptThrottleMap.values())
                if (throttling instanceof ThrottlingLeakyEndpointInfo) {
                    MessageExchange throttled =
                            ((ThrottlingLeakyEndpointInfo) throttling).release();
                    if (throttled != null)
                        try {
                            Logger.getLogger("throttling").info(
                                    "Trying to send (" + throttled.getExchangeId() + ")");

                            getBase().sendMessage(throttled);
                        } catch (MessagingException e) {
                            Logger.getLogger("throttling").info(
                                    "Error at sending (" + throttled.
                                    getExchangeId() + "): " + e);
                            getBase().notifyListeners(e, throttled);
                        }
                }
        }
    }
}
