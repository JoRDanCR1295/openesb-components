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

package com.sun.jbi.httpsoapbc;

import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import com.sun.jbi.httpsoapbc.management.HTTPManagementMBean;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.internationalization.Messages;

/**
 * Wait for responses/requests from the service engine and 
 * use thread pools to process them. 
 * 
 * The receiver not only processes "out-bound" requests initiated by the 
 * Service engine, but also responses to requests this adapter has sent the SE.
 */             
public class OutboundReceiver implements Runnable {
    private static final Messages mMessages =
        Messages.getMessages(OutboundReceiver.class);
        
    // Pool settings for processing SE "out-bound" requests initiated by the SE.
    int corePoolSize = 5;
    int keepAliveTime = 60 * 10;
    TimeUnit timeUnit = TimeUnit.SECONDS;
    // By using an unbounded queue the max pool size becomes irrelevant
    int mOutboundMaxPoolSize = Integer.MAX_VALUE;
    int mInboundReplyMaxPoolSize = Integer.MAX_VALUE;
    
    ThreadPoolExecutor mOutboundPooledExecutor;
    private ThreadPoolQueue mOutboundPooledExecutorQ; 
    
    
    ThreadPoolExecutor inboundReplyPooledExecutor;
    private ThreadPoolQueue inboundReplyPooledExecutorQ;  
    
    private MessagingChannel mChannel;
    private Logger mLog;
    private Object mMonitor;
    private Map mEndpoints;
    private Map mInboundExchanges;
     
    OutboundMessageProcessorPool mProcessorPool;
    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_OUTBOUND_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setOutboundThreads(newVal.intValue());
                }
                if (attrName.equals(RuntimeConfiguration.CONFIG_INBOUND_REPLY_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setInboundReplyThreads(newVal.intValue());
                }
                
            }
        }
    };

	 
    
    /**
     * Construct a receiver for a given binding channel and the relevant endpoints
     * @param bc the binding channel to receive from
     * @param endpoints EndointBean instances with information about the endpoints
     */
    public OutboundReceiver(MessagingChannel bc, Map endpoints, RuntimeConfiguration runtimeConfig, HTTPManagementMBean managementMBean) {
        mLog = Messages.getLogger(OutboundReceiver.class);
        mChannel = bc;
        
        // Apply existing configuration
        Integer threadCount = runtimeConfig.getOutboundThreads();        
        if (threadCount != null) {
            setOutboundThreads(threadCount.intValue());
        }
        threadCount = runtimeConfig.getInboundReplyThreads();        
        if (threadCount != null) {
            setInboundReplyThreads(threadCount.intValue());
        }
        
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);        

        mOutboundPooledExecutorQ = new ThreadPoolQueue();
	mOutboundPooledExecutor = new ThreadPoolExecutor(corePoolSize,mOutboundMaxPoolSize, keepAliveTime, timeUnit, mOutboundPooledExecutorQ, new DaemonThreadFactory());
        mOutboundPooledExecutorQ.setThreadPoolExecutor(mOutboundPooledExecutor);
        
        
        inboundReplyPooledExecutorQ = new ThreadPoolQueue();
        inboundReplyPooledExecutor = new ThreadPoolExecutor(corePoolSize,mInboundReplyMaxPoolSize, keepAliveTime, timeUnit, inboundReplyPooledExecutorQ, new DaemonThreadFactory("HTTPBC-InboundReply-"));
        inboundReplyPooledExecutorQ.setThreadPoolExecutor(inboundReplyPooledExecutor);
        
        
        
        mOutboundPooledExecutor.prestartAllCoreThreads();
        inboundReplyPooledExecutor.prestartAllCoreThreads();

               
        mMonitor = new Object();
        mEndpoints = endpoints;
        mInboundExchanges = InboundMessageProcessor.getInboundExchanges();
        
        mProcessorPool = new OutboundMessageProcessorPool(corePoolSize*2, mOutboundMaxPoolSize+mInboundReplyMaxPoolSize, mChannel, mEndpoints, mInboundExchanges, managementMBean);        
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    public void run() {
        MessageExchange exchange = null;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Receiver started");
        }

        try {
            do {
                exchange = mChannel.accept(5000);
                if (exchange != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Got a message with SOAP binding; id: " + exchange.getExchangeId());
                    }
                    try {
                	boolean outbound = exchange.getRole().equals(MessageExchange.Role.PROVIDER);
                        OutboundAction action = new OutboundAction(mProcessorPool, exchange);
                        exchange = null;
                        if(outbound){
                            mOutboundPooledExecutor.execute(action);                            
                        }else{
                            inboundReplyPooledExecutor.execute(action);
                        }
                            
                        //mLog.log(Level.INFO, String.valueOf( mOutboundPooledExecutor.getPoolSize()) + "---" +  String.valueOf( mOutboundPooledExecutor.getTaskCount())
                        //		+ "---" +  String.valueOf( mOutboundPooledExecutor.getCompletedTaskCount()));
                    } catch (Exception e) {
                        String error = mMessages.getString("HTTPBC-S00800.Failed_to_process_message_exchange");
                        mLog.log(Level.SEVERE, error, e);
                        AlertsUtil.getAlerter().critical(error, 
                                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                         null, 
                                                         AlertsUtil.getServerType(),
                                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                         NotificationEvent.EVENT_TYPE_ALERT,
                                                         "HTTPBC-S00800");
                    }
                }
            } while (mMonitor != null);
        } catch (Exception e) {
            String error = mMessages.getString("HTTPBC-S00801.Failed_to_accept_from_NMR");
            mLog.log(Level.SEVERE, error, e);
            AlertsUtil.getAlerter().critical(error, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-S00801");
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setOutboundThreads(int threadCount) {
        mOutboundMaxPoolSize = threadCount;
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setMaximumPoolSize(mOutboundMaxPoolSize);
        }
    } 
    
    /**
     * Set the number or processing threads to use
     */
    public void setInboundReplyThreads(int threadCount) {
        mInboundReplyMaxPoolSize = threadCount;
               
        if (inboundReplyPooledExecutor != null) {
            inboundReplyPooledExecutor.setMaximumPoolSize(mInboundReplyMaxPoolSize);
        }
  
    } 
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Stopping the receiver thread");
        }
        inboundReplyPooledExecutor.shutdown();
        mOutboundPooledExecutor.shutdown();
        
        mMonitor = null;
    }

    private static class DaemonThreadFactory implements ThreadFactory {
        private AtomicLong threadNumber = new AtomicLong(1);
        
        private String name = "HTTPBC-OutboundReceiver-";
        
        /**
	 * @param name
	 */
	public DaemonThreadFactory(String name) {
	    super();
	    this.name = name;
	}
	
	public DaemonThreadFactory() {
	    super();
	}

	public Thread newThread(Runnable r) {
            Thread daemonThread = new Thread(r, name + getThreadNumber());
            daemonThread.setDaemon(Boolean.TRUE);
            return daemonThread;
        }
        
        private long getThreadNumber(){
        	long l = threadNumber.getAndIncrement();
        	//TODO what happens if l reaches Long.MAX_VALUE
        	return l;
        }
        
    }
}
