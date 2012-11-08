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
 * @(#)InboundReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.LinkedBlockingQueue;

import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;

import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.Endpoint.EndpointType;
import com.sun.jbi.msmqbc.msmq.Channel;
import com.sun.jbi.msmqbc.msmq.ChannelManager;
import com.sun.jbi.msmqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.exception.ChannelManagerException;

/**
 * Controls MSMQ MessageConsumer MessageListeners for consumer (inbound) Endpoints. Manages
 * MessageListener thread pool. Creates MessageListeners for deployed consumer endpoints which
 * receives MSMQ Messages and sends the message received to NMR destined for the inbound endpoint.
 *
 * @author Sun Microsystems
 */
public class InboundReceiver {

    private static final long serialVersionUID = 3256727264572813369L;

    private static final Messages mMessages = Messages.getMessages(InboundReceiver.class);

    private static final Logger mLogger = Messages.getLogger(InboundReceiver.class);

    // Pool settings for processing SE "Inbound" requests initated by the SE.
    private int mInboundCorePoolSize = 16;

    private int mInboundKeepAliveTime = 60 * 10;

    private TimeUnit mInboundTimeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    private int mInboundMaxPoolSize = Integer.MAX_VALUE;

    private ThreadPoolExecutor mInboundPooledExecutor;

    private ComponentContext mContext;

    private DeliveryChannel mChannel;

    private ChannelManager msmqChannelMgr;

    private Map mActiveInboundMessageProcessors; // <EndpointName+OperationName,
													// InboundMessageProcessor>

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
     * Construct a InboundReceiver
     *
     * @param context The BC ComponentContext
     * @param dc The BC DeliveryChannel
     * @param runtimeConfig The RuntimeConfiguration MBean
     * @param msmqChannelMgr The MSMQ ChannelManager
     */
    public InboundReceiver(ComponentContext context,
							DeliveryChannel dc,
							RuntimeConfiguration runtimeConfig,
							ChannelManager channelMgr) {
        mContext = context;
        mChannel = dc;
	msmqChannelMgr = channelMgr;
        mActiveInboundMessageProcessors = Collections.synchronizedMap(new HashMap());

        // Apply existing configuration
        Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }

		mInboundPooledExecutor = new ThreadPoolExecutor(mInboundCorePoolSize,
												mInboundMaxPoolSize,
												mInboundKeepAliveTime,
												mInboundTimeUnit,
												new LinkedBlockingQueue(),
												Executors.defaultThreadFactory());
        mInboundPooledExecutor.prestartAllCoreThreads();

        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }

    /**
     * Construct a InboundReceiver
     *
     * @param context The BC ComponentContext
     * @param dc The BC DeliveryChannel
     * @param runtimeConfig The RuntimeConfiguration MBean
     * @param msmqChannelMgr The MSMQ ChannelManager
     * @param inboundMsgProcessors Map used to add inbound message processors
     */
    protected InboundReceiver(ComponentContext context,
								DeliveryChannel dc,
								RuntimeConfiguration runtimeConfig,
								ChannelManager channelMgr,
								Map inboundMsgProcessors) {
        mContext = context;
        mChannel = dc;
        msmqChannelMgr = channelMgr;
        mActiveInboundMessageProcessors = inboundMsgProcessors;

	// Apply existing configuration
        Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }

		mInboundPooledExecutor = new ThreadPoolExecutor(mInboundCorePoolSize,
											mInboundMaxPoolSize,
											mInboundKeepAliveTime,
											mInboundTimeUnit,
											new LinkedBlockingQueue(),
											Executors.defaultThreadFactory());
		mInboundPooledExecutor.prestartAllCoreThreads();

        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }

    /**
     * Given an Endpoint, start message consumer threads for each defined msmq destination on the
     * msmq operation
     *
     * @param endpoint The service endpoint
     */
    public void addInboundMessageProcessors(Endpoint endpoint)
				throws ChannelManagerException, MessagingException {
		mLogger.log(Level.INFO,"InboundReceiver_ADD_IN_PROCESSOR",
				new Object[]{endpoint.toString()});
        synchronized (mActiveInboundMessageProcessors) {
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                Map msmqOps = endpoint.getMSMQOperations();
                Iterator opQnameIt = msmqOps.keySet().iterator();
                // Gather all operations (which contains the msmq destination)
                while (opQnameIt.hasNext()) {
                    QName opname = (QName) opQnameIt.next();
                    String key = getUniqueKey(endpoint, opname);
                    if (!mActiveInboundMessageProcessors.containsKey(key)) {
                        Channel msmqChannel = msmqChannelMgr.lookup(endpoint, opname);
                        InboundMessageProcessor proc = new InboundMessageProcessor(mContext,endpoint, mChannel, msmqChannel);
                        mInboundPooledExecutor.execute(proc);
                        mActiveInboundMessageProcessors.put(key, proc);
                    }
                }
            }
        }
    }

    /**
     * Given an Endpoint, stop and remove message consumer threads for each msmq operation
     *
     * @param endpoint The service endpoint
     */
    public void removeInboundMessageProcessors(Endpoint endpoint) {
        mLogger.log(Level.INFO, "InboundReceiver_REMOVE_IN_PROCESSOR",
			new Object[]{endpoint.toString()});
        synchronized (mActiveInboundMessageProcessors) {
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                Map msmqOps = endpoint.getMSMQOperations();
                Iterator opQnameIt = msmqOps.keySet().iterator();
                // Gather all operations (which contains the msmq destination)
                while (opQnameIt.hasNext()) {
                    QName opname = (QName) opQnameIt.next();
                    String key = getUniqueKey(endpoint, opname);
                    if (mActiveInboundMessageProcessors.containsKey(key)) {
                        InboundMessageProcessor proc = (InboundMessageProcessor) mActiveInboundMessageProcessors.get(key);
                        proc.stopReceiving();
                        mActiveInboundMessageProcessors.remove(key);
                    }
                }
            }
        }
    }

    /**
     * Set the number or processing threads to use
     *
     * @param threadCount The number of threads
     */
    public void setThreads(int threadCount) {
        mLogger.log(Level.INFO, "InboundReceiver_INBOUND_SET_THREADS",
                new Object[] { new Integer(threadCount) });
        mInboundCorePoolSize = threadCount;
        if (mInboundPooledExecutor != null) {
            mInboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        mLogger.log(Level.INFO, "InboundReceiver_STOP");
        synchronized (mActiveInboundMessageProcessors) {
            Iterator keyIter = mActiveInboundMessageProcessors.keySet().iterator();
            while (keyIter.hasNext()) {
                String key = (String) keyIter.next();
                InboundMessageProcessor proc = (InboundMessageProcessor) mActiveInboundMessageProcessors.get(key);
                proc.stopReceiving();
            }
        }

		//wait for currently assigned threads to complete.
		mInboundPooledExecutor.shutdown();
        mActiveInboundMessageProcessors.clear();
    }

	/**
	 * Get the UniqueKey using given endpoint and operation
	 */
    private String getUniqueKey(Endpoint endpoint, QName operation) {
        return endpoint.getServiceName() + endpoint.getEndpointName() + operation.toString();
    }
}
