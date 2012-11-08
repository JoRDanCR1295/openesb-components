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
 * @(#)ChannelManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import com.sun.jbi.jmsbc.extensions.JMSOperation;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.internationalization.Messages;


import com.sun.jbi.jmsbc.EndpointImpl;
import com.sun.jbi.jmsbc.InboundMessageProcessorFactory;
import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.jmsbc.Endpoint;

import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSOutput;

import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.jmsbc.util.JMSBCContext;

/**
 * Channel management utility class.  Generates different types of
 * channels depending on the binding operation information.  Retains
 * the generated channels for management: lookup, remove, etc.
 * 
 */
public class ChannelManagerImpl implements ChannelManager {
    
    private static final Messages mMessages =
        Messages.getMessages(ChannelManagerImpl.class);
    private static final Logger mLogger =
        Messages.getLogger(ChannelManagerImpl.class);

    private ComponentContext context = null;
    
    private Map channelMap;  // <serviceName+endpointName+jmsBindingOpQNameStr, Channel>
    
    private Map inboundMessageExchanges;  // used by receive channels for saving message exchanges for request/reply inbound exchanges
    
    /** Creates a new instance of ChannelManager */
    public ChannelManagerImpl(ComponentContext context,
                              Map inboundMessageExchanges) {
        this.context = context;
        this.inboundMessageExchanges = inboundMessageExchanges;
        channelMap = Collections.synchronizedMap(new HashMap());        
    }


    /**
     * Creates a Channel and returns it.
     * Retains the created Channel for management.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, 
                              QName jmsBindingOpName) throws ChannelManagerException {
        
        String key = getUniqueKey(endpoint.getServiceName(),
                                  endpoint.getEndpointName(),
                                  endpoint.getEndpointType(),
                                  jmsBindingOpName);
        
        JMSOperation jmsOp = (JMSOperation)endpoint.getJMSOperations().get(jmsBindingOpName);

        synchronized (this) {
            Channel achannel = null;
            
            if (!channelMap.containsKey(key)) {
                
                // Create the channel
                if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
                    achannel = new SendChannelJCAImpl();                    
                } else { // inbound
                    String ndcContextName = new StringBuffer(endpoint
							.getServiceUnitID()).append("::").append(
							endpoint.getEndpointName()).append("::").append(
							jmsBindingOpName.getLocalPart()).toString();

					achannel = new ReceiveChannelJCAImpl(ndcContextName);
                    // Now apply QOS to the channel
                    ServiceQuality[] qos = JMSBCContext.getRef()
							.getQOSConfigConfigurationsValidForMEP(jmsOp,
									endpoint);
                    boolean redeliveryApplied = false;
                    if(qos!=null){
                    	for(int i=0; i<qos.length; ++i){
                    		if(qos[i] instanceof ThrottlingConfig){
                    			((ReceiveChannelJCA) achannel)
										.setMaximumConcurrentConsumers(((ThrottlingConfig) qos[i])
												.getMaxConcurrencyLimit());
                    		}if(qos[i] instanceof RedeliveryConfig){
                    			((ReceiveChannelJCA) achannel)
								.setAppendRedeliveryHandlingWaitTime(((RedeliveryConfig) qos[i])
										.getRetryInterval());
                    			redeliveryApplied = true;
                    		}
                    	}
                    }
                    InboundMessageProcessorFactory mef = 
                            new InboundMessageProcessorFactory(context,
                                                               endpoint,
                                                               jmsOp,
                                                               inboundMessageExchanges, redeliveryApplied);
                    
                    ((ReceiveChannelJCA)achannel).setMessageEndpointFactory(mef);
                    
                } 

                achannel.initialize(endpoint.getJMSAddress(),
                                    jmsOp);
                
                // Add it to the channel map
                channelMap.put (key, achannel);
                
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG,
                                "ChannelManagerImpl_CHANNEL_ADDED",
                                new Object[]{endpoint.getServiceName().toString(),
                                             endpoint.getEndpointName(),
                                             EndpointImpl.endpointTypeToString(endpoint.getEndpointType()),
                                             jmsBindingOpName.toString()});                    
                }
            } else {
                if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
                    achannel = (Channel)channelMap.get(key);
                    // Allow multiple consumers on the same JMS provider endpoint
                    ((SendChannelJCAImpl)achannel).incrementRefCount();
                } else {
                    String errMsg = mMessages.getString("JMSBC-E0201.ChannelAlreadyExist",
                                new Object[]{endpoint.getServiceName(),
                                             endpoint.getEndpointName(),
                                             EndpointImpl.endpointTypeToString(endpoint.getEndpointType()),
                                             jmsBindingOpName});
                    throw new ChannelManagerException(errMsg); 
                }
            }
            return achannel;            
        }
    }

    /**
     * Removes a previously added Channel.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel (Endpoint endpoint, 
                                  QName jmsBindingOpName) throws ChannelManagerException {
        
        String key = getUniqueKey(endpoint.getServiceName(),
                                  endpoint.getEndpointName(),
                                  endpoint.getEndpointType(),
                                  jmsBindingOpName);
        
        Channel achannel = null;
        synchronized (this) {
            if (channelMap.containsKey(key)) {
                achannel = (Channel)channelMap.get(key);
                boolean canRemove = false;
                if (achannel instanceof SendChannel) {
                    SendChannelJCAImpl sendCh = (SendChannelJCAImpl)achannel;
                    if (sendCh.decrementRefCount()==0) {
                        canRemove = true;
                    }                    
                } else {
                    canRemove = true;
                }
                
                if (canRemove) {
                    channelMap.remove(key);                    
                    if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                        mLogger.log(LogSupport.LEVEL_DEBUG,
                                    "ChannelManagerImpl_CHANNEL_REMOVED",
                                    new Object[]{endpoint.getServiceName().toString(),
                                                 endpoint.getEndpointName(),
                                                 EndpointImpl.endpointTypeToString(endpoint.getEndpointType()),
                                                 jmsBindingOpName.toString()});                    
                    }
                }                
            } else {
                String errMsg = mMessages.getString("JMSBC-E0202.ChannelNotFound",
                            new Object[]{endpoint.getServiceName(),
                                         endpoint.getEndpointName(),
                                         EndpointImpl.endpointTypeToString(endpoint.getEndpointType()),
                                         jmsBindingOpName});
                throw new ChannelManagerException(errMsg);                
            }
            
            return achannel;
        }
    }
     
    
    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, 
                         QName jmsBindingOpName) throws ChannelManagerException {

        return lookup (endpoint.getServiceName(),
                       endpoint.getEndpointName(),
                       endpoint.getEndpointType(),
                       jmsBindingOpName);        
    }

    /**
     * Lookup a previously created Channel.
     *
     * @param serviceName The Service QName.
     * @param endpointName The endpoint name.
     * @param endpointType The endpoint type; if provisioning endpoint then Endpoint.EndpointType.OUTBOUND
     *                     otherwise it is a consuming endpoint then Endpoint.EndpointType.INBOUND.
     * @param operation The JMS binding operation QName.
     *
     * @throws ChannelManagerException if a Channel can not be found.
     */
    public Channel lookup(QName serviceName, 
                          String endpointName, 
                          int endpointType,
                          QName jmsBindingOpName) throws ChannelManagerException {
        
        String key = getUniqueKey(serviceName,
                                  endpointName,
                                  endpointType,
                                  jmsBindingOpName);
        
        synchronized(this) {
            Channel achannel = (Channel)channelMap.get(key);
            if (achannel == null) {
                String errMsg = mMessages.getString("JMSBC-E0202.ChannelNotFound",
                            new Object[]{serviceName.toString(),
                                         endpointName,
                                         EndpointImpl.endpointTypeToString(endpointType),
                                         jmsBindingOpName.toString()});
                throw new ChannelManagerException(errMsg);
            }
            
            return achannel;
        }        
    }


    private String getUniqueKey (QName serviceName, 
                                 String endpointName, 
                                 int endpointType,
                                 QName operation) {
        return serviceName.toString() + 
               endpointName + 
               EndpointImpl.endpointTypeToString(endpointType) +
               operation.toString();
        
    }
        
}
