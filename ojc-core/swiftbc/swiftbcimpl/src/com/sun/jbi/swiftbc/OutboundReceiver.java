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
 * @(#)OutboundReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;

import com.sun.jbi.internationalization.Messages;

/**
 * Wait for responses/requests from the service engine and use thread pools to process them. This
 * receiver not only processes "outbound" requests initiated by the Service engine, but also
 * responds to requests this adapter has sent the SE.
 */
public class OutboundReceiver implements Runnable {

    // Pool settings for processing SE "outbound" requests initated by the SE.
    int mOutboundCorePoolSize = 16;

    int mOutboundKeepAliveTime = 60 * 10;

    TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    int mOutboundMaxPoolSize = Integer.MAX_VALUE;

    ThreadPoolExecutor mOutboundPooledExecutor;

    // Pool settings for processing SE "outbound" requests initated by the SE.
    private DeliveryChannel mChannel;

    private Collection mServiceUnits;

    private ComponentContext mComponentContext;

    private RuntimeConfiguration mRuntimeConfig;

    private AtomicBoolean mMonitor;

    private Map<String, InboundReplyContext> mInboundExchanges;

    private static final Logger mLog = Messages.getLogger(OutboundReceiver.class);

    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setThreads(newVal.intValue());
                }
            }
        }
    };

    /**
     * Construct a receiver for a given binding channel and the relevant
     * 
     * @param context the component context associated with this binding component
     * @param bc the binding channel to receive from
     * @param serviceUnits collection of service units
     * @param runtimeConfig the runtime configuration bean
     */
    public OutboundReceiver(ComponentContext context, DeliveryChannel bc, Collection serviceUnits,
            RuntimeConfiguration runtimeConfig) {
        mComponentContext = context;
        mChannel = bc;
        mServiceUnits = serviceUnits;
        mRuntimeConfig = runtimeConfig;
        // Apply existing configuration. This will start workers if the
        // current number is too low. It will stop workers if there are
        // too many working.
        Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);

        mOutboundPooledExecutor = new ThreadPoolExecutor(mOutboundCorePoolSize, mOutboundMaxPoolSize,
                mOutboundKeepAliveTime, mOutboundTimeUnit, new LinkedBlockingQueue());
        mOutboundPooledExecutor.prestartAllCoreThreads();

        mMonitor = new AtomicBoolean(false);
        // mServiceUnits = serviceUnits;
        mInboundExchanges = InboundMessageDelegator.getInboundExchanges();
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    public void run() {
        MessageExchange exchange = null;
        mLog.info("OutboundReceiver.Started_Receiver");
        try {
            OutboundMessageProcessor proc = new OutboundMessageProcessor(mComponentContext, mChannel, mServiceUnits,
                    mInboundExchanges, mRuntimeConfig);

            do {
                exchange = mChannel.accept(5000);
                if (exchange != null) {
                    if (mLog.isLoggable(Level.INFO)) {
                        mLog.log(Level.INFO, "OutboundReceiver.Got_a_message_in_Swift_binding", exchange.getExchangeId());
                    }

                    OutboundAction action = new OutboundAction(proc, exchange);
                    mOutboundPooledExecutor.execute(action);
                }
            } while (mMonitor.get() != Boolean.TRUE);
        } catch (Exception e) {
            if (mLog.isLoggable(Level.INFO)) {
                mLog.log(Level.INFO, "OutboundReceiver.Exception_in_Swift_outbound_receiver", e.getMessage());
            }
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mOutboundCorePoolSize = threadCount;
        if (mLog.isLoggable(Level.INFO)) {
            mLog.log(Level.INFO, "OutboundReceiver_OUTBOUND_MSG_PROCESSOR_THREADS", new Object[] { new Integer(
                    threadCount) });
        }
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
        mLog.info("OutboundReceiver.Stopping_the_Receiver_Thread");
        mMonitor.set(Boolean.TRUE);
    }

    /**
     * Package protected method. Used solely for JUnit test purposes
     */

    ThreadPoolExecutor getOutboundPooledExecutor() {
        return mOutboundPooledExecutor;
    }

    AtomicBoolean getMonitor() {
        return mMonitor;
    }
}
