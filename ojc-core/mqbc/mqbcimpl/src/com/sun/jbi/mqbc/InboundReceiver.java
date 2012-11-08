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


package com.sun.jbi.mqbc;


import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.xml.namespace.QName;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.mqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;


/**
 * *
 * *
 *  @author       Alexander Fung
 *  @version      
 * *
 */
final class InboundReceiver 
{

    private final EventLogger mLogger;
    private final MQComponentContext mContext;
    private final Map<String, InboundMessageProcessor> mActivatedInboundMsgProcs; 
    private final Map<String, InboundMessageProcessor> mPausedInboundMsgProcs; 
    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (RuntimeConfiguration.SEPARATE_TRX_BRANCHES_PROPERTY
                        .equals(attrName)) {
                    Boolean doSeparateBranches = (Boolean) attrNotif.getNewValue();
                    if (doSeparateBranches != null) {
                        InboundReceiver.this.setSeparateTransactionBranches(
                                doSeparateBranches
                        );
                    }
                }
            }
        }
    };

    InboundReceiver(MQComponentContext context,
                    RuntimeConfiguration runtimeConfig) {
        assert context != null;
        assert runtimeConfig != null;

        mLogger = new EventLogger(Util.getLogger(context,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);
        mContext = context;
        mActivatedInboundMsgProcs = new HashMap<String, InboundMessageProcessor>();
        mPausedInboundMsgProcs = new HashMap<String, InboundMessageProcessor>();
        // Subscribe to changes in the configuration
        runtimeConfig.addNotificationListener(listener, null, null);
      
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg(
                    "Component {0} message exchange sender created.",
                    context.getComponentName()));
        }
    }
    
    private String makeProcessorMapKey(Endpoint endpoint, QName opName) {
        assert endpoint != null;
        assert opName != null;

        if (opName.getNamespaceURI() == null
                || opName.getNamespaceURI().length() == 0) {
            opName
                    = new QName(endpoint.getServiceEndpointName().getNamespaceURI(),
                    opName.getLocalPart());
        }
        return endpoint.getServiceName().toString() + endpoint.getEndpointName()
                + ':' + opName.toString();
    }
    
    private void setSeparateTransactionBranches(boolean doSeparateBranches) {
        mContext.setUseSeparateTransactionBranches(doSeparateBranches);
        
        synchronized (mActivatedInboundMsgProcs) {
            for (InboundMessageProcessor processor : mActivatedInboundMsgProcs.values()) {
                processor.useSeparateTransactionBranches(doSeparateBranches);
            }
            if (mLogger.isLoggable(Level.CONFIG)) {
                if (doSeparateBranches) {
                    mLogger.config(NotificationEvent.SEVERITY_TYPE_INFO,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            I18n.msg("Message Exchange senders now set to"
                                    + " always use separate transaction branches."
                            )
                    );
                } else {
                    mLogger.config(NotificationEvent.SEVERITY_TYPE_INFO,
                            NotificationEvent.OPERATIONAL_STATE_STARTED,
                            I18n.msg("Message Exchange senders now set to"
                                    + " join transaction branches."
                            )
                    );
                }
            }
        }
    }
    
    /**
     * Start a new inbound message processing thread for each
     * File binding operation defined in the wsdl given the end point.
     *
     * @param serviceUnit Service unit that owns the endpoint. 
     * @param endpoint A service end point.
     * @param qos List of ServiceQuality(ies) designated for the endpoint.
     * @throws Exception if any error occurs.
     */
    void addInboundMessageProcessor(ServiceUnit serviceUnit,
                                    Endpoint endpoint,
                                    List<ServiceQuality> qos) throws Exception {
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getMQOperations();
                for (Object anEntrySet : operations.entrySet()) {
                    Entry entry = (Entry) anEntrySet;
                    QName opname = (QName) entry.getKey();
                    String key = makeProcessorMapKey(endpoint, opname);
                    if (!mActivatedInboundMsgProcs.containsKey(key)) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Adding MEx-processor for endpoint-operation {0}",
                                    key));
                        }
                        InboundMessageProcessor proc
                                = new InboundMessageProcessor(mContext,
                                serviceUnit,
                                endpoint,
                                opname,
                                qos);
                        // Start the inbound message processor thread
                        new Thread(proc).start();
                        // Store the thread in the map
                        mActivatedInboundMsgProcs.put(key, proc);
                    } else {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Endpoint-operation {0} skipped, already assigned a MEx-processor",
                                    key));
                        }
                    }
                }                
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg(
                            "MEx-processor request for {0} ignored, not an inbound endpoint",
                            endpoint.getServiceName().toString()
                                    + endpoint.getEndpointName()));
                }
            }
        }
    }

    /**
     * Stops and removes the inbound message processor for each 
     * File binding operation per the given end point.
     *
     * @param endpoint A service end point.
     */
    void removeInboundMessageProcessor(Endpoint endpoint) {
        synchronized (mActivatedInboundMsgProcs) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                Map operations = endpoint.getMQOperations();
                Set entrySet = operations.entrySet();
                for (Object anEntrySet : entrySet) {
                    Entry entry = (Entry) anEntrySet;
                    QName opname = (QName) entry.getKey();
                    String key = makeProcessorMapKey(endpoint, opname);
                    if (mActivatedInboundMsgProcs.containsKey(key)) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Removing MEx-processor for endpoint-operation {0}",
                                    key));
                        }

                        InboundMessageProcessor proc =
                                mActivatedInboundMsgProcs.get(key);

                        // Stop the inbound message processor thread
                        proc.stopReceiving();
                        // Remove the thread from the map
                        mActivatedInboundMsgProcs.remove(key);
                    } else {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Endpoint-operation {0} skipped, no assigned a MEx-processor",
                                    key));
                        }
                    }
                }                
            } else {
                mLogger.fine(I18n.msg(
                        "MEx-processor request for {0} ignored, not an inbound endpoint",
                        endpoint.getServiceName().toString()
                                + endpoint.getEndpointName()));
            }
        }
    }

    void suspendInboundMessageProcessor(Endpoint endpoint)
            throws JBIException {
        
        assert endpoint != null;
        
        if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
            synchronized (mActivatedInboundMsgProcs) {
            synchronized (mPausedInboundMsgProcs) {    
                Map operations = endpoint.getMQOperations();
                Set entrySet = operations.entrySet();
                for (Object anEntrySet : entrySet) {
                    Entry entry = (Entry) anEntrySet;
                    QName opname = (QName) entry.getKey();
                    String key = makeProcessorMapKey(endpoint, opname);
                    if (!mActivatedInboundMsgProcs.containsKey(key)) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Endpoint-operation {0} skipped for suspend - not active",
                                    key));
                        }
                    } else {
                        if (mPausedInboundMsgProcs.containsKey(key)) {
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.fine(I18n.msg(
                                        "Endpoint-operation {0} skipped for suspend - already suspended",
                                        key));
                            }
                        } else {
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.fine(I18n.msg(
                                        "Pausing MEx-processor for endpoint-operation {0}",
                                        key));
                            }
                            InboundMessageProcessor proc
                                    = mActivatedInboundMsgProcs.get(key);
                            // Suspend the inbound message processor
                            proc.suspend(endpoint);
                            mPausedInboundMsgProcs.put(key, proc);
                        }
                    }
                }                
            }
        }
        }
    }

    public void resumeInboundMessageProcessor(Endpoint endpoint)
            throws JBIException {
        
        assert endpoint != null;

        if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
            synchronized (mActivatedInboundMsgProcs) {
            synchronized (mPausedInboundMsgProcs) {    
                Map operations = endpoint.getMQOperations();
                Set entrySet = operations.entrySet();
                for (Object anEntrySet : entrySet) {
                    Entry entry = (Entry) anEntrySet;
                    QName opname = (QName) entry.getKey();
                    String key = makeProcessorMapKey(endpoint, opname);
                    if (!mActivatedInboundMsgProcs.containsKey(key)) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.fine(I18n.msg(
                                    "Endpoint-operation {0} skipped for resume - not active",
                                    key));
                        }
                    } else {
                        if (!mPausedInboundMsgProcs.containsKey(key)) {
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.fine(I18n.msg(
                                        "Endpoint-operation {0} skipped for resume - not suspended",
                                        key));
                            }
                        } else {
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.fine(I18n.msg(
                                        "Resuming MEx-processor for endpoint-operation {0}",
                                        key));
                            }
                            InboundMessageProcessor proc
                                    = mActivatedInboundMsgProcs.get(key);
                            // Resume the inbound message processor
                            proc.resume(endpoint);
                            mPausedInboundMsgProcs.remove(key);
                        }
                    }
                }                
            }
        }
        }
    }

    /**
     * Call this method to free up relevant resources. Calling this method
     * renders the object permanently disposed; it cannot be "started" again. 
     */
    public void stop() {
        synchronized (mActivatedInboundMsgProcs) {
            for (InboundMessageProcessor proc : mActivatedInboundMsgProcs
                    .values()) {
                proc.stopReceiving();
            }
            mActivatedInboundMsgProcs.clear();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.msg("Message Exchange senders stopped."));
            }
        }
    }
}
