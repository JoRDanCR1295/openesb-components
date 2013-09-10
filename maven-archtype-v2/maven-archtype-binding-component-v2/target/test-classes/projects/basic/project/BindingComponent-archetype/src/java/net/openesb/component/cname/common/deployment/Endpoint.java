/*
 * Endpoint.java
 *
 */

package net.openesb.component.BindingComponent-archetype.common.deployment;

import net.openesb.component.BindingComponent-archetype.common.MessageExchangeHandler;
import net.openesb.component.BindingComponent-archetype.common.MessageExchangeListener;
import net.openesb.component.BindingComponent-archetype.common.MessageExchangeSupport;
import net.openesb.component.BindingComponent-archetype.common.RuntimeHelper;
import net.openesb.component.BindingComponent-archetype.common.wsdl.WSDLProcessor;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;

/**
 * This is an abstract class that represents the Endpoint defined for service provisioning or
 * consumption in a jbi component. It stores the required service description ( wsdl definition,
 * role, service unit to which it belongs etc) and defines the lifecycle operations {@link #init},
 * {@link #activate}, {@link #deactivate}, {@link #clean} to control a service endpoint that
 * provides or consumes the service.
 * <p>
 * Extended classes implement the abstract methods defined in this class to implement the endpoint
 * functionality by providing the functionality to process the deployment artifacts specific to this
 * endpoint, configure the endpoint to send/receive messages to/from delivery channel and process them
 * according to the specific service implementation.
 *
 * @author chikkala
 */
public abstract class Endpoint {
    /** Role of this endpoint. CONSUMER or PROVIDER */
    private Role mRole;
    /** Service description from the SU descriptor for which this endpoint is configured */
    private SUDescriptor.Service mService;
    /** WSDL definition corresponding to this endpoint */
    private Definition mWsdlDef;
    /** service endpoint corresponding to this endpoint */
    private ServiceEndpoint mServiceEndpoint;
    /** service unit from which this endpoint is created */
    private ServiceUnit mSU; // can be null
    /** private constructor to force extended classes to use the parameterized constructor */
    private Endpoint() {
    }
    /** Creates a new instance of Endpoint
     * @param role CONSUMER or PRVODER role.
     * @param service service description from the su descriptor
     * @param wsdl wsdl definition corresponding to this endpoint
     * @param su  service unit object which created this endpoint.
     */
    protected Endpoint(Role role, SUDescriptor.Service service,  Definition wsdl, ServiceUnit su) {
        this.mService = service;
        this.mRole = role;
        this.mWsdlDef = wsdl;
        this.mSU = su;
    }
    /** Creates a new instance of Endpoint without service unit. may be useful for static endpoints that
     * are not part of the deployment
     * @param role CONSUMER or PRVODER role.
     * @param service service info from the su descriptor
     * @param wsdl wsdl definition corresponding to this endpoint
     */
    protected Endpoint(Role role, SUDescriptor.Service service, Definition wsdl) {
        this(role, service, wsdl, null);
    }
    /**
     * should be called to initialize any resources related to this endpoint object
     * throws JBIException
     */
    public abstract void init() throws JBIException;
    /**
     * activates the endpoint to send/receive messages
     * throws JBIException
     */
    public abstract void activate() throws JBIException;
    /**
     * deactivates the endpoint
     * throws JBIException
     */
    public abstract void deactivate() throws JBIException;
    /**
     * clean endpoint
     * throws JBIException
     */
    public abstract void clean() throws JBIException;
    
    public final Role getRole() {
        return this.mRole;
    }
    public final boolean isProvider() {
        return (Role.PROVIDER.equals(this.getRole()));
    }
    public final boolean isConsumer() {
        return (Role.CONSUMER.equals(this.getRole()));
    }
    public final Definition getWSDL() {
        return this.mWsdlDef;
    }
    public final SUDescriptor.Service getService() {
        return this.mService;
    }
    public final ServiceEndpoint getServiceEndpoint() {
        return this.mServiceEndpoint;
    }
    protected final void setServiceEndpoint(ServiceEndpoint svcEP) {
        this.mServiceEndpoint = svcEP;
    }
    public final ServiceUnit getServiceUnit() {
        return this.mSU;
    }
    /**
     * generates an ID that would uniquely identify this endpoint implementation. Use as a key to map
     * any information to store that is related to this endpoint.
     */
    public final String getID() {
        StringBuffer strBuff = new StringBuffer();
        strBuff.append(this.mService.getInterface()).append("+");
        strBuff.append(this.mService.getServiceName()).append("+");
        strBuff.append(this.mService.getEndpointName()).append("+");        
        String roleType = null;
        if ( Role.CONSUMER.equals(this.mRole) ) {
            roleType = "CONSUMER";
        } else if ( Role.PROVIDER.equals(this.mRole) ) {
            roleType = "PROVIDER";
        }        
        strBuff.append(roleType);
        return strBuff.toString();
    }
    
    @Override
    public String toString() {
        return "Endpoint : " + "\n" + this.mService;
    }
    /**
     * checks if this endpoint is configured for the binding component or service engine.
     */
    public boolean isForBindingComponent-archetype() {
        boolean isForBindingComponent-archetype = false;
        if ( this.mSU != null ) {
            try {
                isForBindingComponent-archetype = this.mSU.getSUDescriptor().isForBindingComponent-archetype();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        return isForBindingComponent-archetype;
    }
    
    public QName getOperationQName(String opName) {
        return new QName(this.getService().getInterface().getNamespaceURI(), opName);
    }
    
    public Operation getWSDLOperation(QName opQName) {
        String opName = null;
        if ( opQName != null ) {
            opName = opQName.getLocalPart();
        }
        Operation operation = WSDLProcessor.findOperation(this.getWSDL(),
            this.getService().getInterface(), opName);
        return operation;
    }
    
    public Binding getWSDLBinding() {
        QName interfaceName = this.getService().getInterface();
        QName serviceName = this.getService().getServiceName();
        String endpointName = this.getService().getEndpointName();
        if ( serviceName != null ) {
            return WSDLProcessor.findServiceBinding(this.getWSDL(), serviceName, endpointName);
        } else {
            return WSDLProcessor.findInterfaceBinding(this.getWSDL(), interfaceName, null);
        }
    }
    /** @return logger */
    public Logger getLogger()  {
        return RuntimeHelper.getLogger();
    }
    /** @return ComponentContext */
    public ComponentContext getComponentContext() {
        return RuntimeHelper.getComponentContext();
    }
    /** @return DeliveryChannel */
    public DeliveryChannel getDeliveryChannel() {
        return RuntimeHelper.getDeliveryChannel();
    }
    /**
     * helper function to get the MessageExchangeSupport object
     */
    public MessageExchangeSupport getMessageExchangeSupport() {
        return RuntimeHelper.getMessageExchangeSupport();
    }
    
    public MessageExchangeHandler createMessageExchangeHandler() {
        return null;
    }
    /**
     * creates the message exchange listener. Extended classes should return
     * MessageExchangeListener implementation.
     * @return MessageExchangeListener or null.
     *
     */
    protected MessageExchangeListener createMessageExchangeListener() {
        return null;
    }
    /**
     * Creates and adds message exchange listener to receive message exchange received notification.
     */
    protected void addMessageExchangeListener() {
        QName interfaceName = this.getService().getInterface();
        QName serviceName = this.getService().getServiceName();
        String endpointName = this.getService().getEndpointName();
        MessageExchangeListener meListener = createMessageExchangeListener();
        MessageExchangeSupport meListenerSupport = getMessageExchangeSupport();
        if ( meListenerSupport != null && meListener != null ) {
            meListenerSupport.addMessageExchangeListener(
                this.getRole(), interfaceName, serviceName, endpointName, meListener);
            if ( serviceName != null ) {
                meListenerSupport.addMessageExchangeListener(
                    this.getRole(), null, serviceName, endpointName, meListener);
            }
        }
    }
    /**
     * Removes message exchange listener from the MessageExchangeSupport.
     */
    protected void removeMessageExchangeListener() {
        QName interfaceName = this.getService().getInterface();
        QName serviceName = this.getService().getServiceName();
        String endpointName = this.getService().getEndpointName();
        MessageExchangeSupport meListenerSupport = getMessageExchangeSupport();
        if ( meListenerSupport != null ) {
            meListenerSupport.removeMessageExchangeListener(
                this.getRole(), interfaceName, serviceName, endpointName);
            if ( serviceName != null ) {
                meListenerSupport.removeMessageExchangeListener(
                    this.getRole(), null, serviceName, endpointName);
            }
        }
    }
    
    public final boolean processMessageExchangeWithHandler(ExchangeStatus status, MessageExchange me) {
        
        //1. lookup handler
        //2. if not there create one and register
        //3. call process message exchange on it
        //4. check status. if that is the end, remove the handler
        
        MessageExchangeSupport support = this.getMessageExchangeSupport();
        if ( support == null ) {
            getLogger().fine("No MessageExchangeSupport present");
            return false;
        }
        MessageExchangeHandler handler = support.findMessageExchangeHandler(me);
        if ( handler == null ) {
            handler = this.createMessageExchangeHandler();
            if ( handler == null ) {
                getLogger().fine("MessageExchangeHandler not supported");
                return false;
            }
            support.addMessageExchangeHandler(me, handler);
        }
        
        handler.processMessageExchange(status, me);
        
        getLogger().fine("XXX MX Handler processed ME with STATUS: " + status);
        
        if (!ExchangeStatus.ACTIVE.equals(status) ) {
            // DONE or ERROR means done with the me.
            getLogger().fine("End of ME processing. STATUS: " + status +
                ". Removing the MX Handler ...");
            support.removeMessageExchangeHandler(me);
        }
        
        return true;
    }
        
}

