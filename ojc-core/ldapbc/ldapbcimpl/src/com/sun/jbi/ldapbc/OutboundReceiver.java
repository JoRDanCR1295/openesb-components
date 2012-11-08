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

package com.sun.jbi.ldapbc;

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

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.util.AlertsUtil;


/**
 * Wait for responses/requests from the service engine and
 * use thread pools to process them.
 *
 * This receiver not only processes "outbound" requests initiated by the
 * Service engine, but also responses to requests this adapter has sent the SE.
 */
class OutboundReceiver implements Runnable {
    private static final Messages mMessages = Messages.getMessages(OutboundReceiver.class);
    private static final Logger mLogger = Messages.getLogger(OutboundReceiver.class);

    // Pool settings for processing SE "outbound" requests initated by the SE.
    int mOutboundCorePoolSize;
    int mOutboundKeepAliveTime = 60 * 10;
    TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    int mOutboundMaxPoolSize = Integer.MAX_VALUE;
    ThreadPoolExecutor mOutboundPooledExecutor;
    private DeliveryChannel mChannel;
    private AtomicBoolean mMonitor;
    private Map mEndpoints;
    private ComponentContext mContext;
    private Map mInboundExchanges;
	private RuntimeConfiguration mRuntimeConfig;
    // Inner class to handle configuration change notifications
    private final NotificationListener listener = new NotificationListener() {
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
    public OutboundReceiver(final DeliveryChannel bc, final Map endpoints,
        final RuntimeConfiguration runtimeConfig, final ComponentContext context) {
        mChannel = bc;
        mContext = context;
		mRuntimeConfig = runtimeConfig;
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
                Executors.privilegedThreadFactory());
        mOutboundPooledExecutor.prestartAllCoreThreads();
        mInboundExchanges = InboundMessageProcessor.getInboundExchanges();
        mMonitor = new AtomicBoolean(false);
        mEndpoints = endpoints;
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    public void run() {
        MessageExchange mExchange = null;
        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("LDAPBC-R00424.OR_Started"));

        try {
            do {
                mExchange = mChannel.accept(500);

                if (mExchange != null) {
                    if (OutboundReceiver.mLogger.isLoggable(Level.INFO)) {
                        OutboundReceiver.mLogger.log(Level.INFO,
                            OutboundReceiver.mMessages.getString("LDAPBC-R00425.OR_Accepted") +
                            mExchange.getExchangeId());
                    }

                    final OutboundMessageProcessor proc = new OutboundMessageProcessor(mChannel,
                            mExchange, mEndpoints, mContext, mInboundExchanges,mRuntimeConfig);
                    final Thread task = new Thread(proc);
                    task.start();

                    // mOutboundPooledExecutor.execute(proc);
                }
            } while (mMonitor.get() != Boolean.TRUE);
        } catch (final Exception e) {
            
            String exMsg = OutboundReceiver.mMessages.getString("OR_Exception") + e.getMessage();
            OutboundReceiver.mLogger.log(Level.INFO,

                OutboundReceiver.mMessages.getString("LDAPBC-E00405.OR_Exception" , new Object[] {e.getMessage()}));
			AlertsUtil.getAlerter().critical(exMsg, 
										LDAPBindingLifeCycle.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"LDAPBC-E00430");
			
        }

        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("LDAPBC-R00426.OR_Shutdown"));
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
        OutboundReceiver.mLogger.log(Level.INFO, OutboundReceiver.mMessages.getString("LDAPBC-R00427.OR_Stop"));
        mMonitor.set(Boolean.TRUE);
    }
}
