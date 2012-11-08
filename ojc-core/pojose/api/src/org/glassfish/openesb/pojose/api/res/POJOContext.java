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
 * @(#)POJOContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api.res;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.MessageException;
import javax.jbi.messaging.MessageExchange;

import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.w3c.dom.Document;

/**
 *  POJO Context provides easy methods to POJO Service to invoke another service.
 *  Instance is injected by POJO Service Engine into member variable annotated with
 *  POJOResource
 *  @deprecated  since 02/05/2009. Use {@link Context}.
 * 
 *  @author Girish Patil
 *  @author Sreeni Genipudi
 *  @see Context
 *  @see Resource
 */
public interface POJOContext {
    public enum MessageObjectType {String, Node, Source, NormalizedMessage};

    /**
     * Get the service endpoint for the named activated endpoint, if any.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public ServiceEndpoint getEndpoint(QName serviceName, String endpoint);
    
    /**
     * Retrieve the service description metadata for the specified endpoint.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return Document
     */
    public Document getEndpointDescriptor(ServiceEndpoint endpoint) throws JBIException;

    /**
     * Queries the NMR for active endpoints that implement the given interface.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public  ServiceEndpoint[] getEndpoints(javax.xml.namespace.QName interfaceName);
          
    /**
     * Queries the NMR for active endpoints belonging to the given service.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public  ServiceEndpoint[] getEndpointsForService(javax.xml.namespace.QName serviceName);
          
    /**
     * Queries the NMR for external endpoints that implement the given interface name.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public  ServiceEndpoint[] getExternalEndpoints(javax.xml.namespace.QName interfaceName);
    
    /**
     * Queries the NMR for external endpoints that are part of the given service.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public ServiceEndpoint[] getExternalEndpointsForService(javax.xml.namespace.QName serviceName);          
    
    /**
     * Resolve the given endpoint reference into a service endpoint.
     * 
     * Delegates call to jbi.javax.jbi.component.ComponentContext.
     * Refer https://open-esb.dev.java.net/nonav/public/javadoc/jbi/javax/jbi/component/ComponentContext.html
     * @return ServiceEndpoint
     */
    public ServiceEndpoint resolveEndpointReference(org.w3c.dom.DocumentFragment epr);         
    
    //ME factories
    /**
     * Returns MessageExchangeFactory
     * @return MessageExchangeFactory
     */
    public MessageExchangeFactory createExchangeFactory();

    /**
     * Returns MessageExchangeFactory for a given interface.
     * @return MessageExchangeFactory
     */
    public MessageExchangeFactory createExchangeFactory(javax.xml.namespace.QName interfaceName);

    /**
     * Returns MessageExchangeFactory for given ServiceEndpoint
     * @return MessageExchangeFactory
     */
    public MessageExchangeFactory createExchangeFactory(ServiceEndpoint endpoint);

    /**
     * Returns MessageExchangeFactory for given service name.
     * @return MessageExchangeFactory
     */
    public MessageExchangeFactory createExchangeFactoryForService(javax.xml.namespace.QName serviceName);

    /**
     * Returns MessageExchange associated with this POJO's Provisioning Service.
     * @return MessageExchange
     */
    public MessageExchange getMessageExchange();

    /**
     * Sends the MessageExchange in synch mode. User should take care to use 
     * appropriate MessageExchange, i.e either InOnly or InOut.
     * Any MessagingException from delivery channel is wrapped in POJOError.
     * 
     * @param me MessageExchange 
     * @return true if the exchange has been processed and returned by the servicing component, false otherwise.
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage any MessagingException is also wrapped in this object.
     */
    public boolean sendSynch(MessageExchange me) throws ErrorMessage;

    /**
     * Sends the status in asynch mode as needed by runtime.
     * Sending done, error status in synch mode will result in exception.
     * <br>
     * JBIMR0024: SendSynch operation not legal in current state.
     * <br>
     * @param me MessageExchange
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage any MessagingException is also wrapped in this object.
     */
    public void send(MessageExchange me) throws ErrorMessage;
    
    /**
     * Sends the message to given ServiceEndpoint.
     * Any MessagingException will be wrapped in POJOError to the caller 
     * including when Delivery channel fails to process the message by returning 
     * false.
     * 
     * Implicit object conversion is done as below.
     * Instance of NormalizedMessage is sent as is. 
     * Instance of Source, Node, String is wrapped with WSDL 1.1 JBI wrapper 
     * message elements.
     * @param se
     * @param msg
     * @throws org.glassfish.openesb.pojose.api.POJOError
     */
    public void sendSynchInOnly(ServiceEndpoint se, Object msg) throws ErrorMessage;
    
    /**
     * Sends the message to given ServiceEndpoint.
     * Any MessagingException will be wrapped in POJOError to the caller 
     * including when Delivery channel fails to process the message by returning 
     * false.
     * 
     * Implicit input object conversion is done as below.
     * Instance of NormalizedMessage is sent as is. 
     * Instance of Source, Node and String is wrapped with WSDL 1.1 JBI wrapper 
     * message elements before sending it as source in a NormalizedMessage.
     * 
     * Output or returned object conversion is done based on the given 
     * MessageObjectType. Implicit WSDL 1.1 JBI message unwrapping is done when 
     * returned message is of type String, Node and Source.
     * 
     * @param se
     * @param inMsg
     * @throws org.glassfish.openesb.pojose.api.POJOError
     */    
    public Object sendSynchInOut(ServiceEndpoint se, Object inMsg, MessageObjectType outputType) throws MessageException;
}
