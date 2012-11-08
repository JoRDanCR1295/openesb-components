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

package com.sun.jbi.hl7bc;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicLong;
import javax.jbi.messaging.ExchangeStatus;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.connection.HL7BCConnectionManager;
import com.sun.jbi.hl7bc.extensions.TcpRoleEnum;
import com.sun.jbi.hl7bc.extservice.server.OutboundTcpServerHL7ConnectorPool;
import com.sun.jbi.hl7bc.management.HL7BCManagementMBean;
import com.sun.jbi.hl7bc.I18n;

/**
 * Wait for responses/requests from the service engine and use thread pools to process them. This
 * receiver not only processes "outbound" requests initiated by the Service engine, but also
 * responds to requests this adapter has sent the SE.
 */
public class OutboundReceiver implements Runnable {

    private static final Logger mLog = Logger.getLogger(OutboundReceiver.class.getName());

    // Pool settings for processing SE "outbound" requests initated by the SE.
    int corePoolSize = 5;

    int keepAliveTime = 60 * 10;

    TimeUnit timeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    int mOutboundMaxPoolSize = Integer.MAX_VALUE;
    int mInboundReplyMaxPoolSize = Integer.MAX_VALUE;

    ThreadPoolExecutor mOutboundPooledExecutor;
    private ThreadPoolQueue mOutboundPooledExecutorQ; 
    ThreadPoolExecutor mInboundReplyPooledExecutor;
    private ThreadPoolQueue mInboundReplyPooledExecutorQ;  

    // Pool settings for processing SE "outbound" requests initated by the SE.
    private MessagingChannel mChannel;

    private Collection mServiceUnits;

    private ComponentContext mComponentContext;

    private RuntimeConfiguration mRuntimeConfig;

    private AtomicBoolean mMonitor;

    private Map<String, InboundReplyContext> mInboundExchanges;
    OutboundMessageProcessorPool mProcessorPool;

    private final OutboundTcpServerHL7ConnectorPool mOutboundServerPool;
    
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
                if (attrName.equals(RuntimeConfiguration.CONFIG_INBOUNDREPLY_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setInboundReplyThreads(newVal.intValue());
                }
                else if (attrName.equals(RuntimeConfiguration.CONFIG_POOL_MIN_SZ)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setMinPoolSize(newVal.intValue());
                }else if (attrName.equals(RuntimeConfiguration.CONFIG_POOL_MAX_SZ)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setMaxPoolSize(newVal.intValue());
                }                
                else if (attrName.equals(RuntimeConfiguration.CONFIG_CONN_MAX_IDLE_TIMEOUT)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setMaxIdleTimeout(newVal.intValue());
                }                
            }
        }
    };

    /**
     * Construct a receiver for a given binding channel and the relevant
     * 
     * @param context the component context associated with this binding component
     * @param messagingChannel the messaging channel to receive from
     * @param serviceUnits collection of service units
     * @param runtimeConfig the runtime configuration bean
     * @param outboundServerPool The server pool to use for outbound connections 
     */
    public OutboundReceiver(ComponentContext context, MessagingChannel messagingChannel, Collection serviceUnits,
            RuntimeConfiguration runtimeConfig, OutboundTcpServerHL7ConnectorPool outboundServerPool) {
        mComponentContext = context;
        mChannel = messagingChannel;
        mServiceUnits = serviceUnits;
        mRuntimeConfig = runtimeConfig;
        mOutboundServerPool = outboundServerPool;
        
        // Apply existing configuration. This will start workers if the
        // current number is too low. It will stop workers if there are
        // too many working.
        Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        threadCount = runtimeConfig.getInboundReplyThreads();        
        if (threadCount != null) {
            setInboundReplyThreads(threadCount.intValue());
        }
        if(!mRuntimeConfig.isAlwaysCreatesNewConnEnabled()){
            
            Integer minPoolSize = runtimeConfig.getConnectionPoolMinSize();
            if(minPoolSize != null){
                setMinPoolSize(minPoolSize.intValue());
            }
            
            Integer maxPoolSize = runtimeConfig.getConnectionPoolMaxSize();
            if(maxPoolSize != null){
                setMaxPoolSize(maxPoolSize.intValue());
            }
            
            Integer maxIdleTimeout = runtimeConfig.getConnectionMaxIdleTimeout();
            if(maxIdleTimeout != null){
                setMaxIdleTimeout(maxIdleTimeout.intValue());
            }
        }
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);

        mOutboundPooledExecutorQ = new ThreadPoolQueue();
	    mOutboundPooledExecutor = new ThreadPoolExecutor(corePoolSize,mOutboundMaxPoolSize, keepAliveTime, timeUnit, mOutboundPooledExecutorQ, new DaemonThreadFactory());
        mOutboundPooledExecutorQ.setThreadPoolExecutor(mOutboundPooledExecutor);
        
        
        mInboundReplyPooledExecutorQ = new ThreadPoolQueue();
        mInboundReplyPooledExecutor = new ThreadPoolExecutor(corePoolSize,mInboundReplyMaxPoolSize, keepAliveTime, timeUnit, mInboundReplyPooledExecutorQ, new DaemonThreadFactory("HL7BC-InboundReply-"));
        mInboundReplyPooledExecutorQ.setThreadPoolExecutor(mInboundReplyPooledExecutor);

        mOutboundPooledExecutor.prestartAllCoreThreads();
        mInboundReplyPooledExecutor.prestartAllCoreThreads();

        mMonitor = new AtomicBoolean(false);
        // mServiceUnits = serviceUnits;
        mInboundExchanges = InboundMessageDelegator.getInboundExchanges();
		mProcessorPool = new OutboundMessageProcessorPool(corePoolSize*2, mOutboundMaxPoolSize+mInboundReplyMaxPoolSize, mComponentContext, mChannel, mServiceUnits, mInboundExchanges, mRuntimeConfig, outboundServerPool);
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    public void run() {
        MessageExchange exchange = null;
        //mLog.info(I18n.msg("I0116: Started Outbound receiver"));
        try {
            //OutboundMessageProcessor proc = new OutboundMessageProcessor(mComponentContext, mChannel, mServiceUnits,
                    //mInboundExchanges, mRuntimeConfig);

            do {
                exchange = mChannel.accept(5000);
                if (exchange != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, I18n.msg("I0117: Received a message in HL7 binding {0}", exchange.getExchangeId()));
                    }
                    try {
                		boolean outbound = exchange.getRole().equals(MessageExchange.Role.PROVIDER);
                        OutboundAction action = new OutboundAction(mProcessorPool, exchange);
                        exchange = null;
                        if(outbound){
                            mOutboundPooledExecutor.execute(action);                            
                        }else{
                            mInboundReplyPooledExecutor.execute(action);
                        }
                    } catch (Exception e) {
                        mLog.log(Level.SEVERE, I18n.msg("E0333: Failed_to_process_message_exchange {0}", e.getMessage()));
                    }

                   // OutboundAction action = new OutboundAction(proc, exchange);
                   // mOutboundPooledExecutor.execute(action);
                }
            } while (mMonitor.get() != Boolean.TRUE);
        } catch (Exception e) {
            if (mLog.isLoggable(Level.INFO)) {
                mLog.log(Level.INFO, I18n.msg("I0118: Exception in HL7 outbound receiver {0}", e.getMessage()));
            }
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mOutboundMaxPoolSize = threadCount;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0119: Setting {0} OutboundMessageProcessor Threads", new Integer(
                    threadCount) ));
        }
        if (mOutboundPooledExecutor != null) {
            mOutboundPooledExecutor.setMaximumPoolSize(threadCount);
        }
    } 
    
    /**
     * Set the number or processing threads to use
     */
    public void setInboundReplyThreads(int threadCount) {
        mInboundReplyMaxPoolSize = threadCount;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0178: Setting {0} InboundReply  Threads", new Integer(
                    threadCount) ));
        }
               
        if (mInboundReplyPooledExecutor != null) {
            mInboundReplyPooledExecutor.setMaximumPoolSize(mInboundReplyMaxPoolSize);
        }  
    } 
    
    /**
     * Set the minimum no. of connection pool size
     */
    public void setMinPoolSize(int val) {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0180: Setting {0} OutboundMessageProcessor MinPoolSize", new Integer(
                    val) ));
        }
        HL7BCConnectionManager.setMinPoolSize(val);
    }    

    /**
     * Set the minimum no. of connection pool size
     */
    public void setMaxPoolSize(int val) {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0181: Setting {0} OutboundMessageProcessor MaxPoolSize", new Integer(
                    val) ));
        }
        HL7BCConnectionManager.setMaxPoolSize(val);
    }    

    /**
     * Set the minimum no. of connection pool size
     */
    public void setMaxIdleTimeout(int val) {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0182: Setting {0} OutboundMessageProcessor MaxIdleTimeout", new Integer(
                    val) ));
        }
        HL7BCConnectionManager.setMaxIdleTimeout(val);
    }     

    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
        mLog.info(I18n.msg("I0120: Stopping the Outbound Receiver Thread"));
        mMonitor.set(Boolean.TRUE);
    }

    /**
     * Package protected method. Used solely for JUnit test purposes
     */

    ThreadPoolExecutor getOutboundPooledExecutor() {
        return mOutboundPooledExecutor;
    }

    ThreadPoolExecutor getInboundReplyPooledExecutor() {
        return mInboundReplyPooledExecutor;
    }

    AtomicBoolean getMonitor() {
        return mMonitor;
    }

    private static class DaemonThreadFactory implements ThreadFactory {
        private AtomicLong threadNumber = new AtomicLong(1);
        
        private String name = "HL7BC-OutboundReceiver-";
        
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

	public void addOutboundMessageProcessor(Endpoint theEndpoint) {
        if (theEndpoint.getHL7Address().getTcpRole() == TcpRoleEnum.SERVER) {
        	mOutboundServerPool.tryToStartListenerForEndpoint(theEndpoint);
        }
	}

	public void removeOutboundMessageProcessor(Endpoint theEndpoint) {
        if (theEndpoint.getHL7Address().getTcpRole() == TcpRoleEnum.SERVER) {
            mOutboundServerPool.shutdownSocketsAssociatedWithEndpoint(theEndpoint);
        }
	}
	
}
