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

package com.sun.jbi.msmqbc;

import java.util.Map;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;

import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.AttributeChangeNotification;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.msmqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.msmqbc.msmq.ChannelManager;
import com.sun.jbi.msmqbc.util.MSMQUtil;

/**
 * Wait for responses/requests from the service engine and use thread pools to process them. The
 * receiver not only processes "outbound" requests initiated by the Service engine, but also
 * responds to requests this adapter has sent the SE.
 *
 * @author Sun Microsystems
 */
public class OutboundReceiver implements Runnable {

    private static final Messages mMessages = Messages.getMessages(OutboundReceiver.class);

    private static final Logger mLogger = Messages.getLogger(OutboundReceiver.class);

	// Pool settings for processing SE "Outbound" requests initated by the SE.
    private int mOutboundCorePoolSize = 16;

	private int mOutboundKeepAliveTime = 60 * 10;

	private TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;

	// By using an unbounded queue the max pool size becomes irrelevant
    private int mOutboundMaxPoolSize = Integer.MAX_VALUE;

	private ThreadPoolExecutor mOutboundPooledExecutor;

    private DeliveryChannel mChannel;

    private Collection mServiceUnits;

    private AtomicBoolean mMonitor;

    private ChannelManager mChannelMgr;

    private ComponentContext mContext;

	private Map mInboundExchanges;

    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
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
	 * @param context the component context associated with this binding component
	 * @param dc the delivery channel to receive from
     * @param serviceUnits collection of service units
	 * @param runtimeConfig the runtime configuration bean
	 * @param channelmanager corresponding this msmq channel


	 */
    public OutboundReceiver(ComponentContext context, DeliveryChannel dc, Collection serviceUnits,
            RuntimeConfiguration runtimeConfig, ChannelManager channelMgr) {

		mChannel = dc;
        mServiceUnits = serviceUnits;		
		mContext = context;
        mChannelMgr = channelMgr;

		// Apply existing configuration. This will start workers if the
        // current number is too low. It will stop workers if there are
        // too many working.
		Integer threadCount = runtimeConfig.getThreads();
		if (threadCount != null) {
			setThreads(threadCount.intValue());
		}

		mOutboundPooledExecutor = new ThreadPoolExecutor(mOutboundCorePoolSize,
				mOutboundMaxPoolSize, mOutboundKeepAliveTime,
				mOutboundTimeUnit, new LinkedBlockingQueue(), Executors
						.privilegedThreadFactory());
		mOutboundPooledExecutor.prestartAllCoreThreads();

		// Subscribe for changes to the configuration
		runtimeConfig.addNotificationListener(listener, null, null);

		mInboundExchanges = InboundMessageProcessor.getInboundExchanges();
        mMonitor = new AtomicBoolean();
	}

	/**
	* Main receiver loop to process replies and requests of SEs
	*/
    public void run() {
		OutboundMessageProcessor proc = null;
		MessageExchange mExchange = null;
        mLogger.info("OutboundReceiver_STARTED_RECEIVER");
        try {
            do {
				mExchange = mChannel.accept(500);
				if (mExchange != null) {
					if (mLogger.isLoggable(Level.INFO)) {
						mLogger.log(Level.INFO,mMessages.getString("OutboundReceiver_ACCEPTED") + mExchange.getExchangeId());
					}
					proc = new OutboundMessageProcessor(mContext, mChannel,
									mServiceUnits, mChannelMgr, mInboundExchanges, mExchange);
					mOutboundPooledExecutor.execute(proc);
				}
            } while (mMonitor.get() != Boolean.TRUE);
        }
        catch (Exception e) {
			mLogger.log(Level.INFO, "OutboundReceiver.EXCEPTION_IN_RECEIVING",
					MSMQUtil.getStackTraceAsString(e));
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mOutboundCorePoolSize = threadCount;
		mLogger.log(Level.INFO, "OutboundReceiver_SET_THREADS",
			 new Object[]{new Integer(threadCount)});
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
        mLogger.info("OutboundReceiver_STOPPING_RECEIVING");
        mMonitor.set(Boolean.TRUE);
		//wait for currently assigned threads to complete.
		mOutboundPooledExecutor.shutdown();
    }
}
