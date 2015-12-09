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

package org.glassfish.openesb.databasebc;

import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.*;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.common.qos.messaging.MessagingChannel;

/**
 * Wait for responses/requests from the service engine and
 * use thread pools to process them.
 *
 * This receiver not only processes "outbound" requests initiated by the
 * Service engine, but also responses to requests this adapter has sent the SE.
 */
class OutboundReceiver implements Runnable {
    private static final Messages mMessages = Messages.getMessages(OutboundReceiver.class);
    public static final Logger mLogger = Messages.getLogger(OutboundReceiver.class);

    // Pool settings for processing SE "outbound" requests initated by the SE.
    int mOutboundCorePoolSize;
    int mOutboundKeepAliveTime = 60 * 10;
    TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    int mOutboundMaxPoolSize = Integer.MAX_VALUE;
    ThreadPoolExecutor mOutboundPooledExecutor;
    private MessagingChannel mChannel;
    private AtomicBoolean mMonitor;
    private Map mEndpoints;
    private JDBCComponentContext mContext;
    private Map mInboundExchanges;

    // Inner class to handle configuration change notifications
    public final NotificationListener listener = new NotificationListener() {
       // @Override
            public void handleNotification(Notification notification, Object obj) {
                if (notification instanceof AttributeChangeNotification) {
                    AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                    String attrName = attrNotif.getAttributeName();

                    if (attrName.equals("Threads")) {
                        Integer newVal = (Integer) (attrNotif.getNewValue());
                        setThreads(newVal.intValue());
                    }
                }
            }
        };

    /**
     * Construct a receiver for a given binding channel and the relevant endpoints
     * @param bc the binding channel to receive from
     * @param endpoints EndointBean instances with information about the endpoints
     */
    public OutboundReceiver(final MessagingChannel bc, final Map endpoints,
        final RuntimeConfiguration runtimeConfig, final JDBCComponentContext context) {
        mChannel = bc;
        mContext = context;

        // Apply existing configuration
        final Integer threadCount = runtimeConfig.getThreads();

        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }

        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
        mOutboundPooledExecutor = new ThreadPoolExecutor(mOutboundCorePoolSize,
                mOutboundMaxPoolSize, mOutboundKeepAliveTime,
                mOutboundTimeUnit, new LinkedBlockingQueue(),
                Executors.defaultThreadFactory());
        mOutboundPooledExecutor.prestartAllCoreThreads();
        mInboundExchanges = InboundMessageProcessor.getInboundExchanges();
        mMonitor = new AtomicBoolean(false);
        mEndpoints = endpoints;
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    //@Override
    public void run() {
        MessageExchange mExchange = null;
        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("DBBC_R00664.OR_Started"));

        try {
            do {
                mExchange = mChannel.accept(500);

                if (mExchange != null) {
                    if (OutboundReceiver.mLogger.isLoggable(Level.FINEST)) {
                        OutboundReceiver.mLogger.log(Level.FINEST,
                            OutboundReceiver.mMessages.getString("DBBC_R00665.OR_Accepted") +
                            mExchange.getExchangeId());
                    } else if (OutboundReceiver.mLogger.isLoggable(Level.FINE)) {
                        OutboundReceiver.mLogger.log(Level.FINE,
                            OutboundReceiver.mMessages.getString("DBBC_R00665.OR_Accepted"));
                    }

                    final OutboundMessageProcessor proc = new OutboundMessageProcessor(mChannel,
                            mExchange, mEndpoints, mContext, mInboundExchanges);
                    //final Thread task = new Thread(proc);
                    //task.start();

                    mOutboundPooledExecutor.execute(proc);
                }
            } while (mMonitor.get() != Boolean.TRUE);
        } catch (final Exception e) {
            OutboundReceiver.mLogger.log(Level.SEVERE,
                OutboundReceiver.mMessages.getString("DBBC_E00666.OR_Exception") + e.getMessage());
        }

        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("DBBC_R00667.OR_Shutdown"));
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(final int threadCount) {
        mOutboundCorePoolSize = threadCount;

        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("DBBC_R00669.OR_Stop"));
	mOutboundPooledExecutor.shutdownNow();
        mMonitor.set(Boolean.TRUE);
    }
}
