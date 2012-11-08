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

package com.sun.jbi.ldapbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.extensions.LDAPOperationInput;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.*;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;


/**
 * Wait for responses/requests from the service engine and
 * use thread pools to process them.
 *
 * Thir receiver not only processes "Inbound" requests initiated by the
 * Service engine, but also responses to requests this adapter has sent the SE.
 */
class InboundReceiver {
    private static final Messages mMessages = Messages.getMessages(InboundReceiver.class);
    private static final Logger mLogger = Messages.getLogger(InboundReceiver.class);

    // Pool settings for processing SE "Inbound" requests initated by the SE.
    private int mInboundCorePoolSize;
    private final int mInboundKeepAliveTime = 60 * 10;
    private final TimeUnit mInboundTimeUnit = TimeUnit.SECONDS;

    // By using an unbounded queue the max pool size becomes irrelevant
    private final int mInboundMaxPoolSize = Integer.MAX_VALUE;
    private ThreadPoolExecutor mInboundPooledExecutor;
    private final DeliveryChannel mChannel;
    private final ComponentContext mContext;

    // This is removed since never used
    //private final Map mEndpoints;
    private final Map mActivatedInboundMsgProcs;

    /**
     *
     */
    private final NotificationListener listener = new NotificationListener() {
            public void handleNotification(Notification notification, Object obj) {
                if (notification instanceof AttributeChangeNotification) {
                    AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                    String attrName = attrNotif.getAttributeName();

                    if (attrName.equals("Threads")) {
                        Integer newVal = (Integer) (attrNotif.getNewValue());
                        setThreads(newVal.intValue());
                    }
                }
            }
        };

    /**
     * Construct a receiver for a given binding channel and the relevant endpoints
     * @param bc the binding channel to receive from
     * @param endpoints EndointBean instances with information about the endpoints
     */
    public InboundReceiver(final DeliveryChannel bc, final Map endpoints,
        final RuntimeConfiguration runtimeConfig, final ComponentContext context) {
        mChannel = bc;
        mContext = context;

        // Apply existing configuration
        final Integer threadCount = runtimeConfig.getThreads();

        if (threadCount != null) {
            setThreads(threadCount.intValue());
        }

        // Subscribe for changes to the configuration
        runtimeConfig.addNotificationListener(listener, null, null);

        mInboundPooledExecutor = new ThreadPoolExecutor(mInboundCorePoolSize,
                mInboundMaxPoolSize, mInboundKeepAliveTime, mInboundTimeUnit,
                new LinkedBlockingQueue(), Executors.privilegedThreadFactory());
        mInboundPooledExecutor.prestartAllCoreThreads();
        mActivatedInboundMsgProcs = Collections.synchronizedMap(new HashMap());

        //mEndpoints = endpoints;
    }

    /**
     * Main receiver loop to process replies and requests of SEs
     */
    public void addInboundMessageProcessor(final EndpointImpl endpoint)
        throws FaultException, MessagingException {
        synchronized (mActivatedInboundMsgProcs) {
            InboundReceiver.mLogger.log(Level.INFO, InboundReceiver.mMessages.getString("LDAPBC-R00419.IR_Started "));

            final Definition defwsdl = (Definition) endpoint.getValueObj(EndpointImpl.DESCRIPTOR);
            final String serviceName = endpoint.getValue(EndpointImpl.SERVICE_NAME);
            final String endpointName = endpoint.getValue(EndpointImpl.ENDPOINT_NAME);

            List ldapOper = new ArrayList();
            ldapOper = getLDAPOperations(defwsdl, serviceName, endpointName);

            for (final Iterator it = ldapOper.iterator(); it.hasNext();) {
                final QName opname = (QName) it.next();
                final String key = endpointName + serviceName + opname.toString();

                if (!mActivatedInboundMsgProcs.containsKey(key)) {
                    try {
                        final InboundMessageProcessor proc = new InboundMessageProcessor(mChannel,
                                endpoint, mContext, opname);
                        final Thread task = new Thread(proc);
                        task.start();
                        // Store the thread in the map
                        mActivatedInboundMsgProcs.put(key, proc);
                    } catch (final Exception e) {
                        InboundReceiver.mLogger.log(Level.INFO,
                            InboundReceiver.mMessages.getString("LDAPBC-R00420.IR_Exception") +
                            e.getMessage());
                    }
                }
            } // for

            InboundReceiver.mLogger.log(Level.INFO, InboundReceiver.mMessages.getString("LDAPBC-R00421.IR_Shutdown"));
        }
    }

    /**
    * Stops and removes the inbound message processor for each
    * File binding operation per the given end point.
    *
    * @param endpoint A service end point.
    */
    public void removeInboundMessageProcessor(final EndpointImpl endpoint) {
        synchronized (mActivatedInboundMsgProcs) {
            InboundReceiver.mLogger.log(Level.INFO, InboundReceiver.mMessages.getString("LDAPBC-R00422.IR_Remove"));

            final Definition defwsdl = (Definition) endpoint.getValueObj(EndpointImpl.DESCRIPTOR);
            final String serviceName = endpoint.getValue(EndpointImpl.SERVICE_NAME);
            final String endpointName = endpoint.getValue(EndpointImpl.ENDPOINT_NAME);
            List ldapOper = new ArrayList();
            ldapOper = getLDAPOperations(defwsdl, serviceName, endpointName);

            for (final Iterator it = ldapOper.iterator(); it.hasNext();) {
                final QName opname = (QName) it.next();
                final String key = endpointName + serviceName + opname.toString();

                if (mActivatedInboundMsgProcs.containsKey(key)) {
                    final InboundMessageProcessor proc = (InboundMessageProcessor) mActivatedInboundMsgProcs.get(key);
                    // Stop the inbound message processor thread
                    proc.stopReceiving();
                    //	proc.deleteTableTigger(endpoint);
                    // Remove the thread from the map
                    mActivatedInboundMsgProcs.remove(key);
                } // if
            } // for

            InboundReceiver.mLogger.log(Level.INFO, InboundReceiver.mMessages.getString("LDAPBC-R00421.IR_Shutdown"));
        }
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(final int threadCount) {
        mInboundCorePoolSize = threadCount;

        if (mInboundPooledExecutor != null) {
            mInboundPooledExecutor.setCorePoolSize(threadCount);
        }
    }

    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        InboundReceiver.mLogger.log(Level.INFO, InboundReceiver.mMessages.getString("LDAPBC-R00423.IR_Stop"));
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public List getLDAPOperations(final Definition def, final String serviceName,
        final String endpointName) {
        final List ldapOperations = new ArrayList();
        LDAPOperationInput ldapOperationInput = null;
        final Binding binding = getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List bindingOperations = binding.getBindingOperations();
            final Iterator extIter = (bindingOperations == null) ? null
                                                           : bindingOperations.iterator();

            while ((extIter != null) && extIter.hasNext()) {
                final BindingOperation oper = (BindingOperation) extIter.next();
                if (LDAPOperation.class.isInstance(oper)) {
                    LDAPOperation ldapOperation = (LDAPOperation)oper;
                    if (ldapOperation.getOperationType()
                            .equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
                        ldapOperations.add(QName.valueOf(oper.getName()));
                    }
                }
                
                
                
            }
        }

        return ldapOperations;
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public Binding getBinding(final Definition def, final String serviceName,
        final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }

    /**
     *
     * @param bindingInput
     * @return
     */
    public LDAPOperationInput getLDAPOperationInput(final BindingInput bindingInput) {
        LDAPOperationInput operationInput = null;

        final List extElems = bindingInput.getExtensibilityElements();

        // Look for ldap:input entries
        final Iterator extIter = (extElems == null) ? null : extElems.iterator();

        while ((extIter != null) && extIter.hasNext()) {
            final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

            if (LDAPOperationInput.class.isInstance(ee)) {
                operationInput = (LDAPOperationInput) ee;

                break;
            }
        }

        return operationInput;
    }
}
