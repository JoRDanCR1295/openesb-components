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
package com.sun.jbi.filebc;

import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import javax.xml.namespace.QName;

/**
 * This class handles and starts the inbound message processors.
 *
 */
public class InboundReceiver {

    public static final String FILEBC_INBOUND_THREAD_NAME_PREFIX = "filebc-ib-";
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private Map mActivatedInboundMsgProcs;
    private Map<String, Endpoint> mActiveEndpoints;
    private Map<String, Endpoint> mInActiveEndpoints;
    private RuntimeConfiguration mRuntimeConfig;
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
     * @param dc            the delivery channel
     * @param runtimeConfig the initial runtime configuration
     */
    public InboundReceiver(ComponentContext context,
            DeliveryChannel channel,
            RuntimeConfiguration runtimeConfig) {
        mContext = context;
        mChannel = channel;
        mActivatedInboundMsgProcs = Collections.synchronizedMap(new HashMap());
        mActiveEndpoints = Collections.synchronizedMap(new HashMap<String, Endpoint>());
        mInActiveEndpoints = Collections.synchronizedMap(new HashMap<String, Endpoint>());
        mRuntimeConfig = runtimeConfig;

        // Apply existing configuration
        Integer threadCount = runtimeConfig.getThreads();
        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }
        // Subscribe to changes in the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
        
    }

    /**
     * Start a new inbound message processing thread for each
     * File binding operation defined in the wsdl given the end point.
     *
     * @param endpoint A service end point.
     */
    public boolean addInboundMessageProcessor(Endpoint endpoint) throws IBProcCreationException, FaultException, MessagingException {
        boolean started = false;
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getFileOperations();
                Set entrySet = operations.entrySet();
                for (Iterator it = entrySet.iterator(); it.hasNext();) {
                    Entry entry = (Entry) it.next();
                    QName opname = (QName) entry.getKey();

                    //String key = endpoint.getServiceName() + endpoint.getEndpointName() + opname.toString();
                    String key = endpoint.getUniqueName() + ":" + opname.toString();
                    if (!mActivatedInboundMsgProcs.containsKey(key)) {
                        InboundMessageProcessor proc = new InboundMessageProcessor(mContext,
                                mChannel,
                                endpoint,
                                opname,mRuntimeConfig.getIBWorkerThreads());
                        // Start the inbound message processor thread
                        Thread t = new Thread(proc);
                        t.setName(FILEBC_INBOUND_THREAD_NAME_PREFIX.concat(t.getName()));
                        t.start();
                        // Store the thread in the map
                        mActivatedInboundMsgProcs.put(key, proc);
                        started = true;
                    }
                }

                if (started) {
                    mActiveEndpoints.put(endpoint.getUniqueName(), endpoint);
                    mInActiveEndpoints.remove(endpoint.getUniqueName());
                }
            }
        }
        return started;
    }

    /**
     * Stops and removes the inbound message processor for each
     * File binding operation per the given end point.
     *
     * @param endpoint A service end point.
     */
    public boolean removeInboundMessageProcessor(Endpoint endpoint) {
        boolean stopped = false;
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getFileOperations();
                Set entrySet = operations.entrySet();
                for (Iterator it = entrySet.iterator(); it.hasNext();) {
                    Entry entry = (Entry) it.next();
                    QName opname = (QName) entry.getKey();

                    //String key = endpoint.getServiceName() + endpoint.getEndpointName() + opname.toString();
                    String key = endpoint.getUniqueName() + ":" + opname.toString();
                    if (mActivatedInboundMsgProcs.containsKey(key)) {
                        InboundMessageProcessor proc =
                                (InboundMessageProcessor) mActivatedInboundMsgProcs.get(key);

                        // Stop the inbound message processor thread
                        proc.stopReceiving();
                        // Remove the thread from the map
                        mActivatedInboundMsgProcs.remove(key);
                        stopped = true;
                    }
                }

                if (stopped) {
                    mActiveEndpoints.remove(endpoint.getUniqueName());
                    mInActiveEndpoints.put(endpoint.getUniqueName(), endpoint);
                }
            }
        }
        return stopped;
    }

    public boolean suspend(String endpointName) {
        Endpoint endpoint = mActiveEndpoints.get(endpointName);
        if (endpoint == null) {
            return false;
        }

        return removeInboundMessageProcessor(endpoint);

    }

    public boolean resume(String endpointName) throws MessagingException, IBProcCreationException, FaultException {
        Endpoint endpoint = mInActiveEndpoints.get(endpointName);
        if (endpoint == null) {
            return false;
        }

        return addInboundMessageProcessor(endpoint);
    }

    public boolean isEndpointActive(String endpointName) {
        if (mActiveEndpoints.get(endpointName) != null) {
            return true;
        }
        return false;
    }

    public Set<String> getActiveEndpointNames() {
        return mActiveEndpoints.keySet();
    }

    public Set<String> getInActiveEndpointNames() {
        return mInActiveEndpoints.keySet();
    }

    /**
     * Set the number or processing threads to be used
     *
     * @param The number of threads
     */
    public void setThreads(int threadCount) {
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    Map getActivatedInboundMsgProcs() {
        return mActivatedInboundMsgProcs;
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setActivatedInboundMsgProcs(Map activatedProcs) {
        mActivatedInboundMsgProcs = activatedProcs;
    }
}
