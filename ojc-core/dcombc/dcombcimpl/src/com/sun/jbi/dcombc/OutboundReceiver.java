/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

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

import com.sun.jbi.dcombc.mbeans.RuntimeConfiguration;
import com.sun.jbi.dcombc.dcom.ChannelManager;
import com.sun.jbi.dcombc.util.DCOMUtil;

/**
 * Wait for responses/requests from the service engine and use thread pools to process them. The
 * receiver not only processes "outbound" requests initiated by the Service engine, but also
 * responds to requests this adapter has sent the SE.
 *
 * @author Chandrakanth Belde
 */
public class OutboundReceiver implements Runnable {
	/**
	 *
	 */
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
	 * @param channelmanager corresponding this dcom channel
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
        mLogger.info("OutboundReceiver.STARTED_RECEIVER");
        try {
            do {
				mExchange = mChannel.accept(500);
				if (mExchange != null) {
					if (mLogger.isLoggable(Level.INFO)) {
						mLogger.log(Level.INFO,mMessages.getString("OutboundReceiver.ACCEPTED") + mExchange.getExchangeId());
					}
					proc = new OutboundMessageProcessor(mContext, mChannel,
									mServiceUnits, mChannelMgr, mInboundExchanges, mExchange);
					mOutboundPooledExecutor.execute(proc);
				}
            } while (mMonitor.get() != Boolean.TRUE);
        }
        catch (Exception e) {
			mLogger.log(Level.INFO, "OutboundReceiver.EXCEPTION_IN_RECEIVING",
					DCOMUtil.getStackTraceAsString(e));
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mOutboundCorePoolSize = threadCount;
		mLogger.log(Level.INFO, "OutboundReceiver.SET_THREADS",
			 new Object[]{new Integer(threadCount)});
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
        mLogger.info("OutboundReceiver.STOPPING_RECEIVING");
        mMonitor.set(Boolean.TRUE);
		//wait for currently assigned threads to complete.
		mOutboundPooledExecutor.shutdown();
    }
}
