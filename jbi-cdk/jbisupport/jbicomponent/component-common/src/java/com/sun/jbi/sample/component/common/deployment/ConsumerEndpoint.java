/*
 * ConsumerEndpoint.java
 */

package com.sun.jbi.sample.component.common.deployment;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * This class extends from Endpoint class and implements lifecycle methods functionality required for the
 * endpoint for a service consumer. {@link ServiceUnit#createConsumerEndpoint} creates the object of this
 * type to implement the service consumer functionality.
 *
 * @see ServiceUnit#createConsumerEndpoint
 * @author chikkala
 */

public class ConsumerEndpoint extends Endpoint {
    /**
     * This constructor initializes the endpoint with CONSUMER role and makes sure that the service
     * description passed to it is of consumer description.
     */
    protected ConsumerEndpoint(SUDescriptor.Consumes consumes, Definition wsdlDef, ServiceUnit su) {
        super(Role.CONSUMER, consumes, wsdlDef, su);
    }
    /**
     * constructor that does not need service unit information. useful for creating the endpoint for
     * static services provided by the component.
     */
    protected ConsumerEndpoint(SUDescriptor.Consumes consumes, Definition wsdlDef) {
        this(consumes, wsdlDef, null);
    }
    public final void init() throws JBIException {
        getLogger().fine("ConsumerEndpoint: init called");
        doInit();                       //1. initialize the endpiont resources
        addMessageExchangeListener();   //2. register message exchange linster.
    }
    public final void activate() throws JBIException {
        getLogger().fine("ConsumerEndpoint: activate called");
        //1. do common ativation tasks.
        doActivate(); //2. do any other activation related tasks.
    }
    
    public final void deactivate() throws JBIException {
        getLogger().fine("ConsumerEndpoint: deactivate called");
        //1. do common deactivation tasks.
        doDeactivate(); //2. do any other deactivation related tasks.
    }
    
    public final void clean() throws JBIException {
        getLogger().fine("ConsumerEndpoint: clean called");
        removeMessageExchangeListener();    //1. remove message exchange listener
        doClean();                          //2. clean up any other resources.
    }
    
    protected void doInit() throws JBIException {
        //NOOP
    }
    protected void doActivate() throws JBIException {
        //NOOP
    }
    protected void doDeactivate() throws JBIException {
        //NOOP
    }
    protected void doClean() throws JBIException {
        //NOOP
    }
    
    /**
     * helper method to find the active ServiceEndpiont for the service described with the
     * serviceDescriptor. This method looks for the Active ServiceEndpoint using interface or
     * service name or service name and the endpoint name.
     */
    public ServiceEndpoint findServiceEndpoint() {
        
        QName serviceType = this.getService().getInterface();
        QName serviceName = this.getService().getServiceName();
        String endpointName = this.getService().getEndpointName();
        
        ServiceEndpoint [] refs = null;
        ServiceEndpoint serviceEndpoint = null;
        ComponentContext compContext = this.getComponentContext();
        
        if ( compContext == null ) {
            this.getLogger().info("Null Component context. Can not find ServiceEndpoint");
            return null;
        }
        // lookup ServiceEndpiont with concrete service(serice qname + endpoint name).
        if ( serviceName != null && endpointName != null ) {
            this.getLogger().info("Looking for ServiceEndpoint with:" +
                " ServiceName: " + serviceName + " EndpointName: " + endpointName);
            serviceEndpoint =  compContext.getEndpoint(serviceName, endpointName);
        }
        // else lookup ServiceEndpiont with Service Name
        if ( serviceEndpoint == null && serviceName != null && endpointName == null) {
            this.getLogger().info("Looking for ServiceEndpoint with Service name: " + serviceName);
            refs = compContext.getEndpointsForService(serviceName);
            if ( refs != null && refs.length > 0 ) {
                serviceEndpoint = refs[0];
            }
        }
        // else lookup ServiceEndpont with serviceType
        if ( serviceEndpoint == null && serviceType != null &&
            serviceName == null && endpointName == null) {
            this.getLogger().info("Looking for ServiceEndpoint with Service type: " + serviceType);
            refs = compContext.getEndpoints(serviceType);
            if ( refs != null && refs.length > 0 ) {
                serviceEndpoint = refs[0];
            }
        }
        
        return serviceEndpoint;
    }
    
    /**
     * this method creates a InOutMessageExchange Object and sets the required
     * data on the MessageExchange object including the create and set the Normalized
     * message object to hold the input message on the MessageExchange object.
     */
    public InOut createInOutMessageExchange(QName operation)
    throws MessagingException, JBIException {
        
        ServiceEndpoint serviceEndpoint = null;
        // find a ServiceEndpoint activated by the Service Providers for this service
        serviceEndpoint = findServiceEndpoint();
        
        if ( serviceEndpoint == null ) {
            // if not found a activated ServiceEndpoint for this service, throw exception.
            throw new JBIException("Can not find an Active ServiceEndpoint for \n" + this.getService());
        } else {
            this.setServiceEndpoint(serviceEndpoint);
        }
        
        InOut inOutME = null;
        DeliveryChannel channel = this.getDeliveryChannel();
        // create message exchange factory for the endpiont
        MessageExchangeFactory factory =  channel.createExchangeFactory(serviceEndpoint);
        // create INOUT Message Exchange
        inOutME = factory.createInOutExchange();
        // set operation
        inOutME.setOperation(operation);
        // set interface if that is not set
        if ( inOutME.getInterfaceName() == null ) {
            inOutME.setInterfaceName(this.getService().getInterface());
        }
        // create IN Nomralized Message
        NormalizedMessage inMsg = inOutME.createMessage();
        // set IN Normalized message on message exchange
        inOutME.setInMessage(inMsg);
        
        return inOutME;
    }
    
    /**
     * this method creates a InOnlyMessageExchange Object and sets the required
     * data on the MessageExchange object including the create and set the Normalized
     * message object to hold the input message on the MessageExchange object.
     */
    public InOnly createInOnlyMessageExchange(QName operation)
    throws MessagingException, JBIException {
        
        ServiceEndpoint serviceEndpoint = null;
        // find a ServiceEndpoint activated by the Service Providers for this service
        serviceEndpoint = findServiceEndpoint();
        
        if ( serviceEndpoint == null ) {
            // if not found a activated ServiceEndpoint for this service, throw exception.
            throw new JBIException("Can not find an Active ServiceEndpoint for \n" + this.getService());
        } else {
            this.setServiceEndpoint(serviceEndpoint);
        }
        
        InOnly inOnlyME = null;
        DeliveryChannel channel = this.getDeliveryChannel();
        // create message exchange factory for the endpiont
        MessageExchangeFactory factory =  channel.createExchangeFactory(serviceEndpoint);
        // create INOUT Message Exchange
        inOnlyME = factory.createInOnlyExchange();
        // set operation
        inOnlyME.setOperation(operation);
        // set interface if that is not set
        if ( inOnlyME.getInterfaceName() == null ) {
            inOnlyME.setInterfaceName(this.getService().getInterface());
        }
        // create IN Nomralized Message
        NormalizedMessage inMsg = inOnlyME.createMessage();
        // set IN Normalized message on message exchange
        inOnlyME.setInMessage(inMsg);
        
        return inOnlyME;
    }
    
    
}