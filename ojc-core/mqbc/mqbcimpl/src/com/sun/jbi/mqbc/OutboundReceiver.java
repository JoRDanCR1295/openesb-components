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

package com.sun.jbi.mqbc;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.AttributeChangeNotification;
import javax.management.ListenerNotFoundException;
import javax.management.Notification;
import javax.management.NotificationListener;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.mqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;


/**
 * Wait for responses/requests from the service engine and 
 * use thread pools to process them. 
 * 
 * Thir receiver not only processes "outbound" requests initiated by the 
 * Service engine, but also responds to requests this adapter has sent the SE.
 */             
final class OutboundReceiver {
 
    private EventLogger mLogger;
    
    private final DeliveryChannel mChannel;
    private final List<OutboundMessageProcessor> mWorkers;
    private final MQComponentContext mComponentContext;
    private final MQBindingDeployer mDeployer;
    private final NotificationListener mListener;
    private final RuntimeConfiguration mRuntimeConfig;

    OutboundReceiver(MQComponentContext context,
                     MQBindingDeployer deployer,
                     RuntimeConfiguration runtimeConfig) {
        assert context != null;
        assert deployer != null;
        assert runtimeConfig != null;

        mLogger = new EventLogger(Util.getLogger(context,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);
        mChannel = context.getMessagingChannel();
        mComponentContext = context;
        mDeployer = deployer;
        mRuntimeConfig = runtimeConfig;
        mWorkers = new ArrayList<OutboundMessageProcessor>();
        mListener = createListener();
      
        Integer threadCount = runtimeConfig.getOutboundThreads();
        if (threadCount != null) {
            setThreads(threadCount);
        }
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(mListener, null, null);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Component {0} message exchange receiver created ({1} threads)",
                    context.getComponentName(),
                    mWorkers.size()));
        }
    }

    private NotificationListener createListener() {
        return new NotificationListener() {
            public void handleNotification(Notification notification, Object obj) {
                if (notification instanceof AttributeChangeNotification) {
                    AttributeChangeNotification attrNotif =
                            (AttributeChangeNotification) notification;
                    String attrName = attrNotif.getAttributeName();
                    if (RuntimeConfiguration.OUTBOUND_THREADS_PROPERTY
                            .equals(attrName)) {
                        try {
                            Integer threads = Integer.valueOf(String.valueOf(
                                    attrNotif.getNewValue()));
                            OutboundReceiver.this.setThreads(threads);
                        } catch (NumberFormatException e) {
                            // ignore notification on attribute parsing error
                        }
                    } else if (RuntimeConfiguration.SEPARATE_TRX_BRANCHES_PROPERTY
                            .equals(attrName)) {
                        Boolean doSeparateBranches = (Boolean) attrNotif.getNewValue();
                        if (doSeparateBranches != null) {
                            OutboundReceiver.this.setSeparateTransactionBranches(
                                    doSeparateBranches
                            );
                        }
                    }
                }
            }
        };
    }

    private void setSeparateTransactionBranches(boolean doSeparateBranches) {
        mComponentContext.setUseSeparateTransactionBranches(doSeparateBranches);
        
        synchronized (mWorkers) {
            for (OutboundMessageProcessor processor : mWorkers) {
                processor.useSeparateTransactionBranches(doSeparateBranches);
            }
            if (mLogger.isLoggable(Level.CONFIG)) {
                if (doSeparateBranches) {
                    mLogger.config(NotificationEvent.SEVERITY_TYPE_INFO,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            I18n.msg("Message Exchange receivers now set to"
                                    + " always use separate transaction branches."
                            )
                    );
                } else {
                    mLogger.config(NotificationEvent.SEVERITY_TYPE_INFO,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            I18n.msg("Message Exchange receiver now set to"
                                    + " join transaction branches."
                            )
                    );
                }
            }
        }
    }
    
    /**
     * Set the number or processing threads to use. This will start workers if
     * the current number is too low.  It will stop workers if there are too
     * many working.
     *
     * @param threadCount Thread count.
     */
    private void setThreads(int threadCount) {
        assert threadCount >= 0;
        
        synchronized (mWorkers) {
            int delta = threadCount - mWorkers.size();
            int previousSize = mWorkers.size();
            
            while (delta != 0) {
                if (delta < 0) {
                    OutboundMessageProcessor proc = mWorkers.remove(0);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg(
                                "Releasing a MEx-processor to reduce pool size,"
                                        + " now {0}, target {1}",
                                mWorkers.size(),
                                threadCount));
                    }
                    if (proc != null) {
                        proc.stopReceiving();
                    }
                    delta += 1;

                } else {
                    OutboundMessageProcessor proc =
                            new OutboundMessageProcessor(mComponentContext,
                                    mChannel, mDeployer);
                    mWorkers.add(proc);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg(
                                "Creating a MEx-processor to increase pool size,"
                                        + " now {0}, target {1}",
                                mWorkers.size(),
                                threadCount));
                    }
                    new Thread(proc).start();
                    delta -= 1;
                }
            }
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.config(NotificationEvent.SEVERITY_TYPE_INFO,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg(
                                "Message Exchange receiver threads changed from {0} to {1}",
                                previousSize,
                                mWorkers.size()));
            }
        }
    }

    /**
     * Call this method to free up relevant resources. Calling this method
     * renders the object permanently disposed; it cannot be "started" again. 
     */
    public void stop() {
        setThreads(0);
        try {
            mRuntimeConfig.removeNotificationListener(mListener);
        } catch (ListenerNotFoundException e) {
            // Ignore exception and continue with shutdown.
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Message Exchange receivers stopped."));
        }
    }
}
