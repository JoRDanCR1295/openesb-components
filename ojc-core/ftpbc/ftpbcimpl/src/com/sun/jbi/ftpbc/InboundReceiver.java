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
package com.sun.jbi.ftpbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessagingException;

import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import javax.xml.namespace.QName;

import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class handles and starts the inbound message processors.
 *
 */
public class InboundReceiver {

    private static final Messages mMessages =
            Messages.getMessages(InboundReceiver.class);
    private static Logger mLogger = Messages.getLogger(InboundReceiver.class);
    private ComponentContext mContext;
    private MessagingChannel mChannel;
    private Map mActivatedInboundMsgProcs;
    private RuntimeConfiguration mRtCfg;
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

    /**
     * Constructor.
     *
     * @param context       the component context associated with this binding component.
     * @param channel            the delivery channel
     * @param runtimeConfig the initial runtime configuration
     */
    public InboundReceiver(ComponentContext context,
            MessagingChannel channel,
            RuntimeConfiguration runtimeConfig) {
        mContext = context;
        mChannel = channel;
        mActivatedInboundMsgProcs = Collections.synchronizedMap(new HashMap());

        mRtCfg = runtimeConfig;
        // Apply existing configuration
        Integer threadCount = runtimeConfig.getOutboundThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        // Subscribe to changes in the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
    }

    /**
     * Start a new inbound message processing thread for each
     * FTP BC binding operation defined in the wsdl given the end point.
     *
     * @param endpoint A service end point.
     */
    public void addInboundMessageProcessor(Endpoint endpoint) throws IBProcCreationException, FaultException, MessagingException {
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getOperations();
                Set entrySet = operations.entrySet();
                for (Iterator it = entrySet.iterator(); it.hasNext();) {
                    Entry entry = (Entry) it.next();
                    QName opname = (QName) entry.getKey();

                    String key = getKeyForIBProcessor(endpoint, opname);
                    if (!mActivatedInboundMsgProcs.containsKey(key)) {
                        InboundMessageProcessor proc = new InboundMessageProcessor(mContext,
                                mChannel,
                                endpoint,
                                opname,
                                mRtCfg);
                        // Start the inbound message processor thread
                        Thread t = new Thread(proc);
                        t.setName("ftpbc-ib-".concat(t.getName()));
                        t.start();
                        // Store the thread in the map
                        mActivatedInboundMsgProcs.put(key, proc);
                    }
                }
            }
        }
    }

    /**
     * Stops and removes the inbound message processor for each
     * FTP BC binding operation per the given end point.
     *
     * @param endpoint A service end point.
     */
    public void removeInboundMessageProcessor(Endpoint endpoint) {
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getOperations();
                Set entrySet = operations.entrySet();
                for (Iterator it = entrySet.iterator(); it.hasNext();) {
                    Entry entry = (Entry) it.next();
                    QName opname = (QName) entry.getKey();
                    String key = getKeyForIBProcessor(endpoint, opname);
                    if (mActivatedInboundMsgProcs.containsKey(key)) {
                        InboundMessageProcessor proc =
                                (InboundMessageProcessor) mActivatedInboundMsgProcs.get(key);
                        // Stop the inbound message processor thread
                        proc.stopReceiving();
                        // Remove the thread from the map
                        mActivatedInboundMsgProcs.remove(key);
                    }
                }
            }
        }
    }

    /**
     * Set the number or processing threads to be used
     *
     * @param threadCount The number of threads
     */
    public void setThreads(int threadCount) {
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    public Map getActivatedInboundMsgProcs() {
        return mActivatedInboundMsgProcs;
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setActivatedInboundMsgProcs(Map activatedProcs) {
        mActivatedInboundMsgProcs = activatedProcs;
    }

    // added for redelivery - suspend / resume
    public boolean suspendProcessor(String key) {
        boolean result = true;
        synchronized (mActivatedInboundMsgProcs) {
            if (mActivatedInboundMsgProcs.containsKey(key)) {
                InboundMessageProcessor proc =
                        (InboundMessageProcessor) mActivatedInboundMsgProcs.get(key);
                proc.suspend();
            } else {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001013.IB_PROC_NOT_FOUND_FOR_KEY", new Object[]{key}));
                }
                result = false;
            }
        }
        return result;
    }

    public boolean resumeProcessor(String key) {
        boolean result = true;
        synchronized (mActivatedInboundMsgProcs) {
            if (mActivatedInboundMsgProcs.containsKey(key)) {
                InboundMessageProcessor proc =
                        (InboundMessageProcessor) mActivatedInboundMsgProcs.get(key);
                proc.resume();
            } else {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001013.IB_PROC_NOT_FOUND_FOR_KEY", new Object[]{key}));
                }
                result = false;
            }
        }
        return result;
    }

    public static final String getKeyForIBProcessor(Endpoint ep, QName operation) {
        return ep.getServiceName() + ep.getEndpointName() + operation.toString();
    }
}
