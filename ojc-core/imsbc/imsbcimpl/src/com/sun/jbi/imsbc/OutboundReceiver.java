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

package com.sun.jbi.imsbc;

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
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.imsbc.util.IMSUtil;
import com.sun.jbi.imsbc.util.AlertsUtil;

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
    private int mOutboundCorePoolSize;

	private int mOutboundKeepAliveTime = 60 * 10;

	private TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;

	// By using an unbounded queue the max pool size becomes irrelevant
    private int mOutboundMaxPoolSize = Integer.MAX_VALUE;

	private ThreadPoolExecutor mOutboundPooledExecutor;

    private MessagingChannel mChannel;

    private Collection mServiceUnits;

    private AtomicBoolean mMonitor;

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
	 * @param channelmanager corresponding this ims channel


	 */
    public OutboundReceiver(ComponentContext context, MessagingChannel dc, Collection serviceUnits,
            RuntimeConfiguration runtimeConfig) {

		mChannel = dc;
        mServiceUnits = serviceUnits;		
		mContext = context;

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
		if (mLogger.isLoggable(Level.INFO))
			mLogger.info(mMessages.getString("IMSBC-R00428.OMP_Started_Receiver"));
        try {
            do {
				mExchange = mChannel.accept(500);
				if (mExchange != null) {
					if (mLogger.isLoggable(Level.INFO)) {
						mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00429.OMP_Mxch_Accepted") + mExchange.getExchangeId());
					}
					proc = new OutboundMessageProcessor(mContext, mChannel,
									mServiceUnits, mInboundExchanges, mExchange);
					mOutboundPooledExecutor.execute(proc);
				}
            } while (mMonitor.get() != Boolean.TRUE);
        }
        catch (Exception e) {
			String exMsg =  mMessages.getString("IMSBC-E00430.OMP_Exception_In_Receiving", IMSUtil.getStackTraceAsString(e));
			mLogger.log(Level.INFO, exMsg);
			AlertsUtil.getAlerter().critical(exMsg, 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-E00430");
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mOutboundCorePoolSize = threadCount;
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00431.OMP_Set_Threads",
				 new Object[]{new Integer(threadCount)}));
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.info( mMessages.getString("IMSBC-R00432.OMP_Stop_Receiving"));
        mMonitor.set(Boolean.TRUE);
		//wait for currently assigned threads to complete.
		mOutboundPooledExecutor.shutdown();
    }
}
