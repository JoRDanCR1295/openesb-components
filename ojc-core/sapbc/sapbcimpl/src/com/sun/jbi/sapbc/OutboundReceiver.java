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

package com.sun.jbi.sapbc;

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
class OutboundReceiver { 
    /**
     * Constructor
     *
     * @param channel the delivery channel to receive from
     * @param serviceUnits the service units deployed
     * @param runtimeConfig the runtime configuration bean
     */
    public OutboundReceiver(DeliveryChannel channel, 
                    Map<String, ServiceUnit> serviceUnits,
                    RuntimeConfiguration runtimeConfig) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mWorkers = new ArrayList<OutboundMessageProcessor>();

        /**
          * Apply existing configuration. 
          * This will start workers if the current number is too low.
          * It will stop workers if there are too many working.
          */ 
        Integer threadCount = runtimeConfig.getThreads();        
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        // Subscribe to changes in the runtime configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }

    /**
     * Set the number or processing threads to use
     * @param threadCount Number of total threads
     */
    public void setThreads(int threadCount) {
    	int activeThreads = mWorkers.size();
        if (threadCount < activeThreads) {
            for (int ii = 0; ii < activeThreads - threadCount; ii++) {
                OutboundMessageProcessor proc = mWorkers.remove(0);
                proc.stopReceiving();
            }
        } else if (threadCount > activeThreads) {
            // Create and start all threads
            for (int ii = 0; ii < threadCount - activeThreads; ii++) {
                OutboundMessageProcessor proc = new OutboundMessageProcessor(mChannel, mServiceUnits);
                new Thread(proc).start();
                mWorkers.add(proc);
            }
        }
    }
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        setThreads(0);
    }
    
    /** 
      * Package protected method.
      * Used solely for JUnit test purposes
      * @return List of workers
      */
    List getWorkers () {
        return mWorkers;
    }

    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = 
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals("Threads")) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setThreads(newVal.intValue());
                }
            }
        }
    };
    
    private DeliveryChannel mChannel;
    private Map<String, ServiceUnit> mServiceUnits;
    private List<OutboundMessageProcessor> mWorkers;
}
