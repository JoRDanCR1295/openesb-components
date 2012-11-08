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

package com.sun.jbi.hl7bc;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extservice.server.HL7Listener;
import com.sun.jbi.hl7bc.extservice.server.HL7Server;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.util.UniqueMsgIdGenerator;
import com.sun.jbi.hl7bc.I18n;
import static com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.*;
import static com.sun.jbi.hl7bc.extservice.server.HL7ServerFactory.*;

/**
 * This class handles and starts the inbound message processors
 * 
 * @author Sriram Vedula, S. Nageswara Rao
 * @version
 */
public class InboundReceiver implements HL7Constants {

    private static final Logger mLog = Logger.getLogger(InboundReceiver.class.getName());

    private ComponentContext mContext;

    private MessagingChannel mChannel;

    // private MessageStore mMessageStore;
    private Map<String, HL7Server> mActiveHL7Servers;

    private Map<String, HL7Server> mPausedHL7Servers;

    private RuntimeConfiguration mRuntimeConfig;

    private UniqueMsgIdGenerator mMsgGenerator;

    /**
     * Constructor.
     * 
     * @param context the component context associated with the hl7 binding component.
     * @param dc the delivery channel
     * @param runtimeconfig the runtime configuration bean
     */
    public InboundReceiver(ComponentContext context, MessagingChannel channel, RuntimeConfiguration runtimeconfig) {
        mContext = context;
        mChannel = channel;
        mRuntimeConfig = runtimeconfig;
        mActiveHL7Servers = new HashMap<String, HL7Server>();
        mPausedHL7Servers = new HashMap<String, HL7Server>();
        mMsgGenerator = new UniqueMsgIdGenerator();
    }

    public void stopReceiving() {
        try {
            HL7Server hl7Server = null;
            for (String key : mActiveHL7Servers.keySet()) {
                hl7Server = mActiveHL7Servers.get(key);
                hl7Server.stopAllServices();
            }
            mActiveHL7Servers.clear();
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0177: an exception occured during stopping the servers"), ex.getCause());
        }
    }

    /**
     * Starts a new inbound message processor for each hl7 address specified in the wsdl to listen
     * to hl7 messages
     * 
     * @param endpoint A service end point.
     */
    public void addInboundMessageProcessor(Endpoint endpoint) throws Exception {
        synchronized (mActiveHL7Servers) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                String key = makeDelegatorsMapKey(endpoint);
                if (!mActiveHL7Servers.containsKey(key)) {
                    Map operations = endpoint.getHL7Operations();
                    Set entrySet = operations.entrySet();
                    Map<String, HL7Listener> hl7Listeners = new HashMap<String, HL7Listener>();
                    for (Iterator it = entrySet.iterator(); it.hasNext();) {
                        Entry entry = (Entry) it.next();
                        QName opname = (QName) entry.getKey();
                        HL7Operation hl7Operation = (HL7Operation) entry.getValue();
                        String messageType = hl7Operation.getMessageType();
                        InboundMessageDelegator inMsgDelegator = new InboundMessageDelegator();
                        inMsgDelegator.setComponentContext(mContext);
                        inMsgDelegator.setMessagingChannel(mChannel);
                        inMsgDelegator.setEndPoint(endpoint);
                        inMsgDelegator.setOperationName(opname);
                        inMsgDelegator.setRuntimeConfiguration(mRuntimeConfig);
                        inMsgDelegator.setUniqueMsgIdGenerator(mMsgGenerator);
                        inMsgDelegator.initialize();
                        hl7Listeners.put(messageType, inMsgDelegator);
                    }
                    ProtocolInfo protocolInfo = null;
                    String transportProtocolName = endpoint.getHL7Address().getTransportProtocolName();
                    HL7ProtocolProperties hl7ProtocolProperties = endpoint.getHL7ProtocolProperties();
                    // right now only TCPIP transport protocol is supported
                    if (transportProtocolName.equalsIgnoreCase(TCPIP)) {
                        HL7Server hl7Server = createHL7Server(TransportProtocolType.TCPIP);
                        hl7Server.setComponentContext(this.mContext);
                        hl7Server.setEndpoint(endpoint);
                        hl7Server.setRuntimeConfiguration(this.mRuntimeConfig);
                        protocolInfo = new ProtocolInfo();
                        String svrPort = endpoint.getHL7Address().getHL7ServerPort().toString();
                        // Server port
                        protocolInfo.put(HL7Address.ATTR_HL7_SVR_PORT, svrPort);
                        protocolInfo.put(SERVICE_NAME, key);
						if(endpoint.getHL7ProtocolProperties().getMLLPv1PersistanceEnabled()){
							String persistenceEnabled = endpoint.getHL7ProtocolProperties().getMLLPv1PersistanceEnabled().toString();
							protocolInfo.put(PERSISTENCE_ENABLED, persistenceEnabled);
						}
                        ProtocolInfo llpInfo = Util.populateLLPInfo(hl7ProtocolProperties);
                        protocolInfo.putAll(llpInfo);
                        StringBuilder sb = new StringBuilder("HL7Service-");
                        sb.append(endpoint.getHL7Address().getHL7ServerLocation()).append("-");
                        sb.append(svrPort);
                        //MBean Name against which an MBean is registered and that holds
                        // an attribute to maintain client connection status
                        hl7Server.setMonitorExtSysConnMBeanName(sb.toString());
                        hl7Server.createHL7Service(hl7Listeners, protocolInfo);
						if (mLog.isLoggable(Level.FINE)) {
							mLog.log(Level.FINE, I18n.msg("I0114: Created HL7Server {2}://{0}:{1}",
                                endpoint.getHL7Address().getHL7ServerLocation(), svrPort, transportProtocolName));
						}
                        mActiveHL7Servers.put(key, hl7Server);
                    }
                }
            }
        }
    }

    private QName lookupOperation(Endpoint endpoint) {
        // Just select the first operation.
        QName[] operationNames = (QName[]) endpoint.getHL7Operations().keySet().toArray(new QName[0]);
        return operationNames[0];

    }

    /**
     * Stops the hl7server and removes the inbound message processor
     * 
     * @param endpoint A service end point.
     */
    public void removeInboundMessageProcessor(Endpoint endpoint) throws Exception {
        synchronized (mActiveHL7Servers) {
            if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
                String key = makeDelegatorsMapKey(endpoint);
                String transportProtocolName = endpoint.getHL7Address().getTransportProtocolName();
                // right now only TCPIP transport protocol is supported
                if (transportProtocolName.equalsIgnoreCase(TCPIP)) {
                    HL7Server hl7Server = mActiveHL7Servers.get(key);
                    ProtocolInfo protocolInfo = new ProtocolInfo();
                    String svrPort = endpoint.getHL7Address().getHL7ServerPort().toString();
                    // Server port
                    protocolInfo.put(HL7Address.ATTR_HL7_SVR_PORT, svrPort);
                    protocolInfo.put(SERVICE_NAME, key);
                    hl7Server.destoryHL7Service(protocolInfo);
					if (mLog.isLoggable(Level.FINE)) {
						 mLog.log(Level.FINE, I18n.msg("I0115: Destroyed HL7Server {2}://{0}:{1}",
                            endpoint.getHL7Address().getHL7ServerLocation(), svrPort, transportProtocolName));
					}
                    // remove the server from Map
                    mActiveHL7Servers.remove(key);
                }
            }
        }
    }

    /**
     * Suspends the HL7 server from processing the messages
     * 
     * @param endpoint A service end point.
     */
    public void suspendInboundMessageProcessor(Endpoint endpoint) throws ApplicationException {
        assert endpoint != null;
        if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
            synchronized (mActiveHL7Servers) {
                synchronized (mPausedHL7Servers) {
                    String key = makeDelegatorsMapKey(endpoint);
                    if (!mActiveHL7Servers.containsKey(key)) {
                        if (mLog.isLoggable(Level.FINE)) {
                            mLog.fine(I18n.msg("Endpoint {0} skipped for suspend - not active", key));
                        }
                    } else {
                        if (mPausedHL7Servers.containsKey(key)) {
                            if (mLog.isLoggable(Level.FINE)) {
                                mLog.fine(I18n.msg("Endpoint {0} skipped for suspend - already suspended", key));
                            }
                        } else {
                            if (mLog.isLoggable(Level.FINE)) {
                                mLog.fine(I18n.msg("Pausing MEx-processor for endpoint {0}", key));
                            }
                            HL7Server hl7Server = mActiveHL7Servers.get(key);
                            ProtocolInfo protocolInfo = new ProtocolInfo();
                            String svrPort = endpoint.getHL7Address().getHL7ServerPort().toString();
                            // Server port
                            protocolInfo.put(HL7Address.ATTR_HL7_SVR_PORT, svrPort);
                            hl7Server.suspendHL7Service(protocolInfo);
                            mPausedHL7Servers.put(key, hl7Server);
                        }
                    }

                }
            }
        }

    }

    /**
     * Resume the HL7 server to process the messages
     * 
     * @param endpoint A service end point.
     */
    public void resumeInboundMessageProcessor(Endpoint endpoint) throws ApplicationException {
        assert endpoint != null;
        if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) {
            synchronized (mActiveHL7Servers) {
                synchronized (mPausedHL7Servers) {
                    String key = makeDelegatorsMapKey(endpoint);
                    if (!mActiveHL7Servers.containsKey(key)) {
                        if (mLog.isLoggable(Level.FINE)) {
                            mLog.fine(I18n.msg("Endpoint {0} skipped for resume - not active", key));
                        }
                    } else {
                        if (!mPausedHL7Servers.containsKey(key)) {
                            if (mLog.isLoggable(Level.FINE)) {
                                mLog.fine(I18n.msg("Endpoint {0} skipped for resume - not suspended", key));
                            }
                        } else {
                            if (mLog.isLoggable(Level.FINE)) {
                                mLog.fine(I18n.msg("Resuming MEx-processor for endpoint {0}", key));
                            }
                            HL7Server hl7Server = mActiveHL7Servers.get(key);
                            ProtocolInfo protocolInfo = new ProtocolInfo();
                            String svrPort = endpoint.getHL7Address().getHL7ServerPort().toString();
                            // Server port
                            protocolInfo.put(HL7Address.ATTR_HL7_SVR_PORT, svrPort);
                            hl7Server.resumeHL7Service(protocolInfo);
                            mPausedHL7Servers.remove(key);
                        }
                    }

                }
            }
        }

    }

    private String makeDelegatorsMapKey(Endpoint endpoint) {
        assert endpoint != null;
        return endpoint.getServiceName().toString() + endpoint.getEndpointName();
    }

    /**
     * Package protected method. Used solely for JUnit test purposes
     */
    Map getActivateInboundMsgProcs() {
        return mActiveHL7Servers;
    }
}// end of class
