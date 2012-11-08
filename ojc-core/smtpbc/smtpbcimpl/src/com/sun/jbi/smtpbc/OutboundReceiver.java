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

package com.sun.jbi.smtpbc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.mbeans.RuntimeConfiguration;

/**
 * Wait for responses/requests from the service engine and 
 * use thread pools to process them. 
 * 
 * Thir receiver not only processes "outbound" requests initiated by the 
 * Service engine, but also responds to requests this adapter has sent the SE.
 */             
public class OutboundReceiver {

	private static final Logger mLog =
            Messages.getLogger(OutboundReceiver.class);     

    // Pool settings for processing SE "outbound" requests initated by the SE.
    private DeliveryChannel mChannel;
    private Collection mServiceUnits;
    private MessageStore mMessageStore;
    private List mWorkers;

    // Inner class to handle configuration change notifications
    private final NotificationListener listener = new NotificationListener() {
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
    
    /**
     * Construct a receiver for a given binding channel and the relevant
     * endpoints
     * @param bc the binding channel to receive from
     * @param endpoints EndointBean instances with information about the
     * endpoints
     */
    public OutboundReceiver(final DeliveryChannel bc,
                            final Collection serviceUnits,
                            final RuntimeConfiguration runtimeConfig,
                            final MessageStore messageStore) {
        mChannel = bc;
        mMessageStore = messageStore;
        mServiceUnits = serviceUnits;
        mWorkers = new ArrayList();

        // Apply existing configuration.  This will start workers if the
        // current number is too low.  It will stop workers if there are
        // too many working.
        final Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(final int threadCount) {
        if (threadCount < mWorkers.size()) {
            for (int ii = 0; ii < mWorkers.size() - threadCount; ii++) {
                final OutboundMessageProcessor proc =
                    (OutboundMessageProcessor)mWorkers.remove(ii);
                proc.stopReceiving();
            }
        } else if (threadCount > mWorkers.size()) {
            // Create and start all threads
            for (int ii = 0; ii < threadCount - mWorkers.size(); ii++) {
                final OutboundMessageProcessor proc =
                    new OutboundMessageProcessor(mChannel,
                                                 mServiceUnits,
                                                 mMessageStore);
                new Thread(proc).start();
                mWorkers.add(proc);
            }
        }
    }
    
    /**
     * Stops all processing threads.
     */
    public void stopReceiving() {
        if (OutboundReceiver.mLog.isLoggable(Level.INFO)) {
            OutboundReceiver.mLog.log(Level.INFO, "OR_Stopping_ReceiverThread");
        }
        setThreads(0);
    }
}
