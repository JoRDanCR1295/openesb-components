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
 * @(#)ConsumerImpl.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.res.Context;

/**
 * Instance of Consumer used to consume Service. <p>
 * Instance of this class will be injected into JBI Provider field when annotaed 
 * with ConsumerEndpoint. Instance can also be obtained from the Context object.
 * Note this class instance is not thread safe and should not be used concurrently.
 * <p>
 * @author gmpatil
 * @see Context
 * @see ConsumerEndpoint
 */
public interface Consumer {
    //public enum MessageObjectType {String, Document, Element, TextNode, NormalizedMessage};    
    public enum MessageObjectType {String, Document, Node, Source, NormalizedMessage};

    /**
     * Sets ServiceEndpoint.
     *<p>
     * @param se ServiceEndpoint
     */
    public void setServiceEndpoint(ServiceEndpoint se);

    /**
     * Returns ServiceEndpoint instance associated with this Consumer.
     *
     * @return ServiceEndpoint
     */
    public ServiceEndpoint getServiceEndpoint();    

    /**
     * Sets the service QName instance. ServiceEndpoint instance if set will take
     * precedence over service QName.
     * <p>
     * @param svc Service QName.
     */    
    public void setService(QName svc);

    /**
     * Return QName of the Service associated with this consumer.
     *<p>
     * @return QName of the service
     */
    public QName getService();

    /**
     * Sets the default interface QName.
     * <p>
     * @param intf Interface QName
     */    
    public void setInterface(QName intf);

    /**
     * Get the default interface QName associated with this consumer.
     * <p>
     * @return QName of the default interface.
     */
    public QName getInterface();
    
    /**
     * Input message type to be used inside WSDL 1.1 wrapper JBI message.
     * This is used only during implicit WSDL 1.1 message wrapping.
     * <p>
     * @param inpt QName of input message type.
     */
    public void setDefaultInputMessageType(QName inpt);

    /**
     * Operation name to be set on MessageExchange created implicitly by Consumer.
     * <p>
     * @param  opr QName of operation.
     */    
    public void setDefaultOperationName(QName opr);

    /**
     * Creates and returns InOnly MessageExchange. Sets and uses ServiceEndpoint
     * if available.  Also sets operation, if default is present.
     * <p>
     * @return
     * @throws MessagingException
     */
    public MessageExchange createInOnlyMessageExchange() throws MessagingException;

    /**
     * Creates and returns InOut MessageExchange. Sets and uses ServiceEndpoint
     * if available.  Also sets operation, if default is present.
     * <p>
     * @return
     * @throws MessagingException
     */
    public MessageExchange createInOutMessageExchange() throws MessagingException;

    /**
     * Sends the MessageExchange in synch mode. User should take care to use 
     * appropriate MessageExchange, i.e either InOnly or InOut.
     * <p>
     * Any MessagingException from delivery channel is wrapped in POJOError.
     * <p>
     * Uses the available default ServiceEndpoint, Service or Interface in order 
     * if all of the above is null in the passed MessageExchange.
     * <p>
     * Default Operation name is set if it is null on the passed MessageExchange.
     * <p>
     * @param me MessageExchange 
     * @return true if the exchange has been processed and returned by the servicing component, false otherwise.
     * @throws org.glassfish.openesb.pojose.api.POJOError any MessagingException is also wrapped in this object.
     */
    public boolean sendSynch(MessageExchange me) throws MessageException;

    /**
     * Sends the MessageExchange in synch mode. User should take care to use
     * appropriate MessageExchange, i.e either InOnly or InOut.
     * <p>
     * Any MessagingException from delivery channel is wrapped in POJOError.
     * <p>
     * Uses the available default ServiceEndpoint, Service or Interface in order
     * if all of the above is null in the passed MessageExchange.
     * <p>
     * Default Operation name is set if it is null on the passed MessageExchange.
     * <p>
     * @param me MessageExchange
     * @param timeout
     * @return true if the exchange has been processed and returned by the servicing component, false in the case of timeout.
     * @throws org.glassfish.openesb.pojose.api.POJOError any MessagingException is also wrapped in this object.
     */
    public boolean sendSynch(MessageExchange me, long timeout) throws MessageException;
    
    /**
     * Sends the ME is asynch mode. Transaction object is not propagated by default,
     * in asynch mode.  Use overloaded method to explicitly suspend and propagate
     * transaction object if one is available in provisioning message exchange.
     *
     * @see Consumer#send(javax.jbi.messaging.MessageExchange, boolean)
     *
     * <p>
     * @param me MessageExchange
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage any MessagingException is also wrapped in this object.
     */
    public void send(MessageExchange me) throws ErrorMessage;

    /**
     * Sends the ME is asynch mode. Paramter propagateTxn is used to suspend and
     * propgating the Transaction object or continue in current thread if one available.
     * 
     * <p>
     * @param me MessageExchange
     * @param propagateTxn boolean, true - propagate transaction, false other wise.
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage any MessagingException is also wrapped in this object.
     */
    public void send(MessageExchange me, boolean propagateTxn) throws ErrorMessage;

    /**
     * Sends the ME is asynch mode. Converts/wraps objects into XML Source.
     * Creates InOnly message exchange and uses ServiceEndpoint, Input message
     * type and Operation name specified on the annotation.
     * <p>
     * Transaction object is not propagated by default, in asynch mode.
     * Use overloaded method to explicitly suspend and propagate
     * transaction object if one is available in provisioning message exchange.
     *
     * @see Consumer#sendInOnly(java.lang.Object, boolean)
     * 
     * @param msg
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void sendInOnly(Object msg) throws ErrorMessage;

    /**
     * Sends the ME is asynch mode. Converts/wraps objects into XML Source.
     * Creates InOnly message exchange and uses ServiceEndpoint, Input message
     * type and Operation name specified on the annotation.
     *
     * Paramter propTxn is used to suspend and propgating the Transaction
     * object or continue in current thread if one available.
     *
     * @param msg
     * @param propTxn true to propagate the Transaction if available.
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void sendInOnly(Object msg, boolean propTxn) throws ErrorMessage;

    /**
     * Sends the ME is asynch mode. Converts/wraps objects into XML Source.
     * Creates InOut message exchange and uses ServiceEndpoint, Input message
     * type and Operation name specified on the annotation.
     * <p>
     * Transaction object is not propagated by default, in asynch mode.
     * Use overloaded method to explicitly suspend and propagate
     * transaction object if one is available in provisioning message exchange.
     *
     * @see Consumer#sendInOut(java.lang.Object, boolean) 
     *
     * @param msg
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void sendInOut(Object msg) throws ErrorMessage;

    /**
     * Sends the ME is asynch mode. Converts/wraps objects into XML Source.
     * Creates InOut message exchange and uses ServiceEndpoint, Input message
     * type and Operation name specified on the annotation.
     *
     *
     * Paramter propTxn is used to suspend and propgating the Transaction
     * object or continue in current thread if one available.
     *
     * @param msg
     * @param propTxn true to propagate the Transaction if available.
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void sendInOut(Object msg, boolean propTxn) throws ErrorMessage;

    /**
     * Sends the message to given ServiceEndpoint.<p>
     * Any MessagingException will be wrapped in Error to the caller, 
     * including when Delivery channel fails to process the message by returning 
     * false.
     * <p>
     * Implicit object conversion is done as below.<br>
     * Instance of NormalizedMessage is sent as is. <br>
     * Instance of Source, Node, String is wrapped with WSDL 1.1 JBI wrapper 
     * message elements.
     * <p>
     * @param msg instance of type String, Node, Source or NormalizedMessage.
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void sendSynchInOnly(Object msg) throws ErrorMessage;

    /**
     * Sends the message to given ServiceEndpoint.<p>
     * Any MessagingException will be wrapped in Error to the caller,
     * including when Delivery channel fails to process the message by returning
     * false.
     * <p>
     * Implicit object conversion is done as below.<br>
     * Instance of NormalizedMessage is sent as is. <br>
     * Instance of Source, Node, String is wrapped with WSDL 1.1 JBI wrapper
     * message elements.<p>
     * 
     * @param msg instance of type String, Node, Source or NormalizedMessage.
     * @param timeout 
     * @throws ErrorMessage
     */
    public void sendSynchInOnly(Object msg,  long timeout) throws ErrorMessage;
    
    /**
     * Sends the message to given ServiceEndpoint.<p>
     * Any MessagingException will be wrapped in POJOError to the caller 
     * including when Delivery channel fails to process the message by returning 
     * false.
     * <p>
     * Implicit input object conversion is done as below.<br>
     * Instance of NormalizedMessage is sent as is. <br>
     * Instance of Source, Node and String is wrapped with WSDL 1.1 JBI wrapper 
     * message elements before sending it as source in a NormalizedMessage.
     * <p>
     * Output or returned object conversion is done based on the given 
     * MessageObjectType. Implicit WSDL 1.1 JBI message unwrapping is done when 
     * returned message is of type String, Node and Source.
     * <p>
     * @param inMsg instance of type String, Node, Source or NormalizedMessage.
     * @param outputType - value to be returned of type.
     * @return object of type outputType
     * @throws MessageException
     */    
    public Object sendSynchInOut(Object inMsg, MessageObjectType outputType) throws MessageException;

    /**
     * Sends the message to given ServiceEndpoint.<p>
     * Any MessagingException will be wrapped in POJOError to the caller
     * including when Delivery channel fails to process the message by returning
     * false.
     * <p>
     * Implicit input object conversion is done as below.<br>
     * Instance of NormalizedMessage is sent as is. <br>
     * Instance of Source, Node and String is wrapped with WSDL 1.1 JBI wrapper
     * message elements before sending it as source in a NormalizedMessage.
     * <p>
     * Output or returned object conversion is done based on the given
     * MessageObjectType. Implicit WSDL 1.1 JBI message unwrapping is done when
     * returned message is of type String, Node and Source.
     * <p>
     * 
     * @param inMsg - instance of type String, Node, Source or NormalizedMessage.
     * @param outputType - value to be returned of type.
     * @param timeout
     * @return object of type outputType
     * @throws MessageException
     */
    public Object sendSynchInOut(Object inMsg, MessageObjectType outputType, long timeout) throws MessageException;
}
