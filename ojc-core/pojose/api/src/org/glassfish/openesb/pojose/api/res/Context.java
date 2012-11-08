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
 * @(#)Context.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api.res;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * Context provides methods to Provider instance to discover ServiceEndpoints 
 * and create MessageExchangeFactory objects.<br>
 * 
 * Instance is injected by Service Engine into member variable annotated using 
 * {@link Resource}.<br>
 *
 * @author Girish Patil
 * @author Sreeni Genipudi
 * 
 * @see Resource
 */
public interface Context {
    /**
     * Get the service endpoint for the named activated endpoint, if any.<br>
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.<br>
     * 
     * @return ServiceEndpoint
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public ServiceEndpoint getEndpoint(QName serviceName, String endpoint);
    
    /**
     * Retrieve the service description metadata for the specified endpoint.<br>
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.<br>
     * @return Document
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    
    public Document getEndpointDescriptor(ServiceEndpoint endpoint) throws JBIException;

    /**
     * Queries the NMR for active endpoints that implement the given interface.<br>
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.<br>
     * @return ServiceEndpoint
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public  ServiceEndpoint[] getEndpoints(javax.xml.namespace.QName interfaceName);
          
    /**
     * Queries the NMR for active endpoints belonging to the given service.<br>
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.<br>

     * @return ServiceEndpoint
     * 
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    
    public  ServiceEndpoint[] getEndpointsForService(javax.xml.namespace.QName serviceName);
          
    /**
     * Queries the NMR for external endpoints that implement the given interface name.<br>
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.<br>
     * @return ServiceEndpoint
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public  ServiceEndpoint[] getExternalEndpoints(javax.xml.namespace.QName interfaceName);
    
    /**
     * Queries the NMR for external endpoints that are part of the given service.
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.
     * @return ServiceEndpoint
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public ServiceEndpoint[] getExternalEndpointsForService(javax.xml.namespace.QName serviceName);          
    
    /**
     * Resolve the given endpoint reference into a service endpoint.
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.
     * @return ServiceEndpoint
     * @see ComponentContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public ServiceEndpoint resolveEndpointReference(org.w3c.dom.DocumentFragment epr);         
    
    //ME factories
    
    /**
     * Returns MessageExchangeFactory
     * Delegates call to {@link  javax.jbi.messaging.DeliveryChannel  DeliveryChannel}.
     * @return MessageExchangeFactory
     * @see DeliveryChannel
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/messaging/DeliveryChannel.html}">JBI 1.0 API</a>
     */
    public MessageExchangeFactory createExchangeFactory();

    /**
     * Returns MessageExchangeFactory for a given interface.
     * Delegates call to {@link javax.jbi.messaging.DeliveryChannel  DeliveryChannel}.
     * @return MessageExchangeFactory
     * @see DeliveryChannel
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/messaging/DeliveryChannel.html}">JBI 1.0 API</a>
     */
    public MessageExchangeFactory createExchangeFactory(javax.xml.namespace.QName interfaceName);

    /**
     * Returns MessageExchangeFactory for given ServiceEndpoint
     * Delegates call to {@link  javax.jbi.messaging.DeliveryChannel  DeliveryChannel}.
     * @return MessageExchangeFactory
     * @see DeliveryChannel
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/messaging/DeliveryChannel.html}">JBI 1.0 API</a>
     */
    public MessageExchangeFactory createExchangeFactory(ServiceEndpoint endpoint);

    /**
     * Returns MessageExchangeFactory for given service name.
     * Delegates call to {@link  javax.jbi.messaging.DeliveryChannel  DeliveryChannel}.
     * @return MessageExchangeFactory
     * @see DeliveryChannel
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/messaging/DeliveryChannel.html}">JBI 1.0 API</a>
     */
    public MessageExchangeFactory createExchangeFactoryForService(javax.xml.namespace.QName serviceName);

    /**
     * Returns MessageExchange associated with this POJO's Provisioning Service.
     * Delegates call to {@link  javax.jbi.messaging.DeliveryChannel  DeliveryChannel}.
     * @return MessageExchangeFactory
     * @see DeliveryChannel
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/messaging/DeliveryChannel.html}">JBI 1.0 API</a>
     */
    public MessageExchange getMessageExchange();
    
    /**
     * Returns intsnace of InitialContext or null.
     * Delegates call to {@link  javax.jbi.component.ComponentContext  ComponentContext}.
     *
     * @return InitialContext
     * @see <a href="https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html">JBI 1.0 API</a>
     */
    public InitialContext getNamingContext();


    /**
     * Gets the new Consumer instance.
     * 
     * @param se ServiceEndpoint
     * @param oper QName of the operation
     * @param inpt QName of the input
     * @return Consumer
     */
    public Consumer getConsumer(ServiceEndpoint se, QName oper, QName inpt);

    /**
     * Gets the new Consumer instance.
     * 
     * @return Consumer
     */
    public Consumer getConsumer();

    /**
     * Utility method to construct FaultMessage. This utility method always
     * tries to wrap passed payload with JBI WSDL 1.1 wrapper elements.
     * 
     * @param payload
     * @param faultMsgType
     * @return FaultMessage
     */
    public FaultMessage createFaultMessage(String payload, QName faultMsgType);

    /**
     * Utility method to construct FaultMessage. This utility method always
     * tries to wrap passed payload with JBI WSDL 1.1 wrapper elements.
     *
     * @param payload
     * @param faultMsgType
     * @return FaultMessage
     */
    public FaultMessage createFaultMessage(Node payload, QName faultMsgType);
    /**
     * Utility method to construct FaultMessage. This utility method always
     * tries to wrap passed payload with JBI WSDL 1.1 wrapper elements.
     *
     * @param payload
     * @param faultMsgType
     * @return FaultMessage
     */
    public FaultMessage createFaultMessage(Source payload, QName faultMsgType);
}
