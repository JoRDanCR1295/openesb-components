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
 * @(#)MessagingChannel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.messaging;

import java.util.List;

import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;

/**
 * Defines a <code>DeliveryChannel</code> decorator capable of storing
 * service qualities and handling redelivery scenarios.
 * 
 * @author Kevan Simpson
 */
public interface MessagingChannel extends DeliveryChannel {
    /**
     * Adds a service quality configuration to the specified consumer endpoint.
     * @param endpoint A consumer endpoint (i.e. {@link EndpointInfo#isProvides()} returns <code>false</code>).
     * @param qos One or more service quality configurations.
     */
    public void addServiceQualityToEndpoint(EndpointInfo endpoint, ServiceQuality... qos);

    /**
     * Removes a service quality configuration from the specified consumer endpoint.
     * @param endpoint A consumer endpoint (i.e. {@link EndpointInfo#isProvides()} returns <code>false</code>).
     * @param qos One or more service quality configurations.
     */
    public void removeServiceQualityFromEndpoint(EndpointInfo endpoint, ServiceQuality... qos);
    
    /**
     * Fetches the service quality configurations for the specified consumer endpoint.
     * @param endpoint A consumer endpoint (i.e. {@link EndpointInfo#isProvides()} returns <code>false</code>).
     * @return A list of service quality configurations. 
     */
    public List<ServiceQuality> getServiceQualitiesForEndpoint(EndpointInfo endpoint);
    
    /**
     * Fetches a typed service quality configuration for the specified endpoint.
     * @param <T> The type of service quality to fetch.
     * @param endpoint A consumer endpoint (i.e. {@link EndpointInfo#isProvides()} returns <code>false</code>).
     * @param qosType The typed class of the service quality to fetch.
     * @return A service quality configuration of the specified type or <code>null</code>.
     */
    public <T extends ServiceQuality> T getServiceQuality(EndpointInfo endpoint, Class<T> qosType);
    
    /**
     * Utility method for redelivery to create exchanges on the fly, one for each 
     * retry attempt (if necessary).
     * @param et The message exchange template factory.
     * @throws MessagingException if an error occurs creating or sending exchange.
     */
    public void send(ExchangeTemplates et)throws MessagingException;
    
    /**
     * Fetches the message exchange factory for this component.
     * @return the message exchange factory for this component.
     */
    public MessageExchangeFactory getExchangeFactory();
    
    /**
     * Looks up and installs all service quality configurations for the specified service unit.
     * @param suName The service unit name.
     * @param suPath The service unit installation path.
     * @throws DeploymentException if an error occurs looking up configuration.
     */
    public void installServiceQualities(String suName, String suPath)throws DeploymentException;
    
    /**
     * Uninstalls service quality configurations for the specified service unit.
     * @param suName The service unit name.
     */
    public void uninstallServiceQualities(String suName);
    
    /**
     * Adds a listener for {@link MessagingException}s that occur during <code>send()</code>.
     * @param sfl A listener.
     * @see SendFailureListener
     */
    public void addSendFailureListener(SendFailureListener sfl);
    
    /**
     * Removes a listener for {@link MessagingException}s that occur during <code>send()</code>.
     * @param sfl A listener.
     * @see SendFailureListener
     */
    public void removeSendFailureListener(SendFailureListener sfl);
    
    /**
     * Fetches the {@link DeploymentLookup} utility used by this channel.
     * @return the {@link DeploymentLookup} utility used by this channel.
     */
    public DeploymentLookup getLookup();
}