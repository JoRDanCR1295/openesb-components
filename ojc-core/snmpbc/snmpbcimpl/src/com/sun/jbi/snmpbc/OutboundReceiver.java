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

package com.sun.jbi.snmpbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpbc.mbeans.RuntimeConfigurationImpl;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * get msgs from NMR
 * 
 * @author echou
 */             
public class OutboundReceiver implements Runnable, NotificationListener {
 
    private ComponentContext mComponentContext;
    private SNMPServiceUnitManager suManager;
    private RuntimeConfigurationImpl runtimeConfig;
    private Map<String, InboundMessageProcessorListenerEndpoint> inboundMessageExchanges;
    
    private Thread thisThread = null;
    private ThreadPoolExecutor pool;
    
    private static final Messages mMessages =
        Messages.getMessages(OutboundReceiver.class);
    private static final Logger mLog = Logger.getLogger(OutboundReceiver.class.getName());
    
    
    /**
     * Construct a receiver for a given binding channel and the relevant endpoints
     * 
     * @param context 
     * @param suManager 
     * @param runtimeConfig 
     * @param inboundMessageExchanges 
     */
    public OutboundReceiver(ComponentContext context,
            SNMPServiceUnitManager suManager,
            RuntimeConfigurationImpl runtimeConfig,
            Map<String, InboundMessageProcessorListenerEndpoint> inboundMessageExchanges) {
        mComponentContext = context;
        this.suManager = suManager;
        this.runtimeConfig = runtimeConfig;
        this.inboundMessageExchanges = inboundMessageExchanges;
        
        pool = new ThreadPoolExecutor(runtimeConfig.getOutboundThreadPoolCoreSize(),
                runtimeConfig.getOutboundThreadPoolMaxSize(),
                runtimeConfig.getOutboundThreadPoolKeepAliveInMillis(),
                TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable> ());
        
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(this, null, null);
    }
    
    
    public void startReceiving() {
        mLog.log(Level.INFO, "starting OutboundReceiver");
        ThreadFactory factory = Executors.privilegedThreadFactory();
        thisThread = factory.newThread(this);
        thisThread.start();
    }
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        mLog.log(Level.INFO, "stopping OutboundReceiver");
        
        thisThread.interrupt();
        
        try {
            runtimeConfig.removeNotificationListener(this);
        } catch (Exception e) {
            mLog.log(Level.WARNING, "exception when remove OutboundReceiver as listener:", e);
        }
    }

    public void run() {
        try {
            while (true) {
                MessageExchange msgExchange = mComponentContext.getDeliveryChannel().accept();
                if (msgExchange != null) {
                    pool.execute(new OutboundMessageProcessor(
                            msgExchange,
                            mComponentContext,
                            runtimeConfig.getTestMode(),
                            suManager,
                            inboundMessageExchanges));
                }
            }
        } catch (MessagingException me) {
            // assumes JBI framework will wrap InterruptedException inside MessagingException
            if (me.getCause() instanceof InterruptedException) {
                // because JBI framework does not call Thread.interrupted() method
                Thread.interrupted();
                //mLog.log(Level.INFO, "caught InterruptedException");
            } else {
                mLog.log(Level.SEVERE, "error during DeliveryChannel.accept() ", me);
            }
        } catch (Exception e) {
            mLog.log(Level.SEVERE, "error in OutboundReceiver ", e);
        } finally {
            pool.shutdown();
            try {
                pool.awaitTermination(5000L, TimeUnit.MILLISECONDS);
            } catch (InterruptedException ie) {
                // do nothing
            }
        }
    }

    public void handleNotification(Notification notification, Object handback) {
        if (notification instanceof AttributeChangeNotification) {
            AttributeChangeNotification acn =(AttributeChangeNotification) notification;
            String attrName = acn.getAttributeName();
            if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_MAX_SIZE.equals(attrName)) {
                int newVal = (Integer) acn.getNewValue();
                pool.setMaximumPoolSize(newVal);
            } else if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_CORE_SIZE.equals(attrName)) {
                int newVal = (Integer) acn.getNewValue();
                pool.setCorePoolSize(newVal);
            } else if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS.equals(attrName)) {
                long newVal = (Long) acn.getNewValue();
                pool.setKeepAliveTime(newVal, TimeUnit.MILLISECONDS);
            }
        }
    }
    
}
