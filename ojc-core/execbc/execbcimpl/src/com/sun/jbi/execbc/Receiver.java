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
 * @(#)Receiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.DeliveryChannel;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;

/**
 * This class is responsible for starting and stopping
 * the outbound message processor threads.
 */
class Receiver {
    public static final String EXECBC_OUTBOUND_THREAD_NAME_PREFIX="execbc-ob-";
    private DeliveryChannel mChannel;
    private Map mServiceUnits;
    private Map mInboundExchanges;
    private List mWorkers;
    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                        (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals("MaximumSessions")) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setMaximumSessions(newVal.intValue());
                }
            }
        }
    };
    
    /**
     * Constructor
     *
     * @param channel the delivery channel to receive from
     * @param serviceUnits the service units deployed
     * @param runtimeConfig the runtime configuration bean
     */
    public Receiver(DeliveryChannel channel,
            Map serviceUnits,
            RuntimeConfiguration runtimeConfig) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mWorkers = new ArrayList();
        mInboundExchanges = InboundMessageProcessor.getInboundExchanges();
        
        /**
         * Apply existing configuration.
         * This will start workers if the current number is too low.
         * It will stop workers if there are too many working.
         */
        Integer threadCount = runtimeConfig.getMaximumSessions();
        if (threadCount != null) {
            setMaximumSessions(threadCount.intValue());
        }
        // Subscribe to changes in the runtime configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }
    
    /**
     * Set the number or processing threads to use
     */
    public void setMaximumSessions(int threadCount) {
        int activeThreads = mWorkers.size();
        if (threadCount < activeThreads) {
            for (int ii = 0; ii < activeThreads - threadCount; ii++) {
                OutboundMessageProcessor proc =
                        (OutboundMessageProcessor)mWorkers.remove(0);
                proc.stopReceiving();
            }
        } else if (threadCount > activeThreads) {
            // Create and start all threads
            for (int ii = 0; ii < threadCount - activeThreads; ii++) {
                OutboundMessageProcessor proc =
                        new OutboundMessageProcessor(mChannel,
                        mServiceUnits,
                        mInboundExchanges);
                Thread t = new Thread(proc);
                t.setName(EXECBC_OUTBOUND_THREAD_NAME_PREFIX.concat(t.getName()));
                t.start();
                mWorkers.add(proc);
            }
        }
    }
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        setMaximumSessions(0);
    }
    
    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    List getWorkers() {
        return mWorkers;
    }
}
