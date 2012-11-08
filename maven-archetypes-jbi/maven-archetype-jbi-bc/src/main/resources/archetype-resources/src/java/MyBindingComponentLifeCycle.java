/*
 * MyBindingComponentLifeCycle.java
 */
package ${package};

import com.sun.jbi.sample.component.common.ComponentLifeCycleImpl;
import com.sun.jbi.sample.component.common.EchoServiceDescriptor;
import com.sun.jbi.sample.component.common.Helper;
import java.io.StringReader;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

/**
 * This class extends the default ComponentLifeCycleImpl to provide MyBinding BC
 * specific implementation of the ComponentLifeCycle.
 *
 * This implementation makes MyBinding BC as a proxy service consumer for external
 * service consumers that want to invoke service providers inside the jbi environment
 * using jmx protocol.
 *
 * This implementation shows various steps involved in setting up the MyBinding BC
 * to allow external service consumers to invoking a services provided inside the
 * jbi environment through a Binding Coponent.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.ComponentLifeCycleImpl
 * @author chikkala
 */
public class MyBindingComponentLifeCycle extends ComponentLifeCycleImpl {
    /** Synchronous send timeout  */
    public static final long SEND_SYNC_TIMEOUT = 60000;
    /** NMR delivery channel */
    private DeliveryChannel mDeliveryChannel;
    /** echo service description */
    private EchoServiceDescriptor mEchoServiceDescriptor;
    /** Echo ServiceEndpoint  */
    private ServiceEndpoint mEchoServiceEndpoint;
    /** jmx endpoint implementation */
    private JMXEndpointMBean mEchoJMXEndpoint;
    /** jmx domain name for JMXEndpointMBean to register with mbean server. */
    public static final String JMX_DOMAIN = "com.sun.jbi.sample.component.jmx.binding";
    /** key in the mbean object name of the JMXEndpointMBean */
    public static final String JMX_ENDPOINT_ADDRESS_KEY = "jmx-endpoint-address";
    /** value of the ENDPOINT_ADDRESS_MBEAN_KEY */
    private final static String  JMX_ENDPOINT_ADDRESS_VALUE = "${artifactId}/MyEngine/echo";
    
    /** constructor for the ComponentLifecycle implementation. */
    public MyBindingComponentLifeCycle() {
    }
    /**
     * This method will be called from ComponentLifeCycleImpl.init(...)
     * implementation after initializing required component context. So, do any
     * other required initialization here.
     * @see com.sun.jbi.sample.component.common.ComponentLifeCycleImpl\#init(
     * javax.jbi.component.ComponentContext)
     * @throws javax.jbi.JBIException on error
     */
    protected void doInit() throws JBIException {
        // called from ComponentLifeCycle.init
        openDeliveryChannel();  // 1. Open a delivery channel
        initServiceConsumers(); // 2. initialize Service consumers
        this.getLogger().info(this.getComponentContext().getComponentName() + " Initialized");
    }
    /**
     * Start the component.
     * @see javax.jbi.component.ComponentLifeCycle\#start()
     * @throws javax.jbi.JBIException on error.
     */
    public final void start() throws JBIException {
        
        activateServiceConsumers(); // 1. Activate the Service Provider endpoints
        
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Started.");
    }
    /**
     * Stop the component.
     * @see javax.jbi.component.ComponentLifeCycle\#stop()
     * @throws javax.jbi.JBIException on error.
     */
    public final void stop() throws JBIException {
        
        deactivateServiceConsumers(); // 1. Deactivate Service Consumers
        
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Stopped.");
    }
    /**
     * Shut down the component.
     * @see javax.jbi.component.ComponentLifeCycle\#shutDown()
     * @throws javax.jbi.JBIException on error.
     */
    public final void shutDown() throws JBIException {
        // release all resources
        closeDeliveryChannel(); // 1. Close delivery channel
        
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Shutdown.");
        
    }
    /**
     * this is a default implementation which does not have any extension mbeans. extended
     * classes can return the mbean name for the extension mbeans if the component supports.
     * @return null to indicate no extension mbean.
     * @see javax.jbi.component.ComponentLifeCycle\#getExtensionMBeanName()
     */
    public final ObjectName getExtensionMBeanName() {
        return null;
    }
    
    /** @return DeliveryChannel opened for this component */
    public final DeliveryChannel getDeliveryChannel() {
        return this.mDeliveryChannel;
    }
    /** opens the delivery channel to accept or send message exchange objects
     */
    private void openDeliveryChannel() {
        try {
            this.mDeliveryChannel = this.getComponentContext().getDeliveryChannel();
        } catch (MessagingException ex) {
            ex.printStackTrace();
        }
    }
    /** closes the delivery channel as part of the component shutdown process.
     */
    private void closeDeliveryChannel() {
        // closes delivery channel and remove the reference.
        try {
            if ( this.mDeliveryChannel != null ) {
                this.mDeliveryChannel.close();
            }
        } catch (MessagingException ex) {
            ex.printStackTrace();
        } finally {
            this.mDeliveryChannel = null;
        }
    }
    /**
     * service consumer initialization such as creating the service descriptions,
     * the resources required, etc.
     * This method is invoked in the implementation of ComponentLifeCycle init
     * method
     * @throws javax.jbi.JBIException
     */
    private void initServiceConsumers()  throws JBIException {
        // create EchoService Descriptor with concrete service with
        // service provider activated endpoint name.        
        String echoServiceEndpointName = null;
        // TODO: uncomment this code and change the providerComponentName value if 
        // you want to lookup the service with service endpoint address
        // String providerComponentName = "MyEngine";
        // echoServiceEndpointName = 
        //        EchoServiceDescriptor.generateJBIInternalEndpointName(providerComponentName);
        
        this.mEchoServiceDescriptor =
            new EchoServiceDescriptor(MessageExchange.Role.CONSUMER,
            echoServiceEndpointName);
        // create jmx endpoint mbean implementation which acts as external endpoint.
        this.createJMXEndpointMBean();
    }
    /**
     * This method is invoked in the implementation of ComponentLifeCycle start
     * method
     * @throws javax.jbi.JBIException
     */
    private void activateServiceConsumers() throws JBIException {
        try {
            // activate consumer to receive messages from external jmx consumer
            this.registerJMXEndpointMBean();
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new JBIException(ex.getMessage());
        }
        this.getLogger().info("---- Echo Service Consumer JMX Endpoint Activated");
    }
    /**
     * This method is invoked in the implementation of ComponentLifeCycle stop
     * method
     * @throws javax.jbi.JBIException
     */
    private void deactivateServiceConsumers()  throws JBIException {
        try {
            // deactivate Echo Service JMX Endpoint
            this.unregisterJMXEndpointMBean();
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
            throw new JBIException(ex.getMessage());
        }
        this.getLogger().info("---- Echo Service Consumer JMX Endpoint Deactivated");
    }
    /**
     * invokes a echoService with in-out message exchange sent to delivery channel
     * by the consumer
     * @param operation operation name on a service
     * @param inputDoc input xml document for the InOut operation
     * @throws java.lang.Exception if any error occurs in invoking the operation on the service.
     * @return output xml document in a StringBuffer received from InOut operation of the
     * service invoked.
     */
    private StringBuffer doInOutMessageExchange(String operation, StringBuffer inputDoc)
    throws Exception {
        // get the component context and the delivery channel for preparing to send message
        ComponentContext compContext = this.getComponentContext();
        DeliveryChannel channel = this.getDeliveryChannel();
        
        ServiceEndpoint serviceEndpoint = null;
        // find a ServiceEndpoint activated by the Service Providers for this service
        serviceEndpoint = findEchoServiceEndpoint();
        
        if ( serviceEndpoint == null ) {
            // if not found a activated ServiceEndpoint for this service, throw exception.
            throw new Exception("MyBinding can not find an Active ServiceEndpoint for \n" +
                this.mEchoServiceDescriptor);
        }
        // create INOUT Message Exchange
        InOut inOutME = createInOutMessageExchange(
            this.mEchoServiceDescriptor.getOperationQName(), serviceEndpoint);
        // set the content of the IN normalized message ( Normalize the message )
        Source inMsgSource = Helper.createDOMSource(new StringReader(inputDoc.toString()));
        NormalizedMessage inMsg = inOutME.getInMessage();
        inMsg.setContent(inMsgSource);
        // send the message exchange and wait for response
        boolean isSent = channel.sendSync(inOutME, SEND_SYNC_TIMEOUT);
        if ( !isSent ) {
            throw new Exception("MyBinding:Timeout occured in sending the message to provider");
        }
        // check if you got a out message or fault or error
        // process the Message Exchange to get the output message and
        // complete InOut message exchange with provider
        NormalizedMessage outMsg = processInOutMessageExchangeOnConsumer(inOutME);
        // read the out message content (Denormalize the message) to string buffer.
        StringBuffer outputDoc = Helper.readFromSource(outMsg.getContent());
        // return the out message content to the external service consumer.
        return outputDoc;
    }
    /**
     * takes the InOut message exchange received from sendSync call and processes it further
     * to complete InOut message exchange with provider and returns the out message or throws
     * exception in case of error or faults.
     */
    private NormalizedMessage processInOutMessageExchangeOnConsumer(InOut inoutExchange)
    throws Exception {
        // InOut MessageExchange processing on consumer side
        // 1. ACTIVE status receives a fault or out message,
        //    send the done status to complete message exchange
        //    return the fault/out message to external consumer
        // 2. can not receive DONE status
        // 3. when received ERROR status, return the error to consumer.
        
        // process the message exchange based on its state.
        ExchangeStatus status = inoutExchange.getStatus();
        this.getLogger().info("Consumer:Processing Message Exchange with status " + status);
        
        if (ExchangeStatus.ACTIVE.equals(status) ) {
            
            Fault fault = inoutExchange.getFault();
            NormalizedMessage outMsg = inoutExchange.getOutMessage();
            
            // send done to complete message exchange.
            DeliveryChannel channel = this.getDeliveryChannel();
            inoutExchange.setStatus(ExchangeStatus.DONE);
            channel.send(inoutExchange);
            this.getLogger().info("Consumer: Completed the INOUT MessageExchange");
            
            // process fault or out message
            if ( fault != null ) {
                // throw an exception if there is a fault message.
                Source faultContent = fault.getContent();
                StringBuffer faultContentBuff = null;
                if ( faultContent != null ) {
                    faultContentBuff = Helper.readFromSource(faultContent);
                }
                throw new Exception("Consumer:INOUT message exchange Fault \n" +
                    faultContentBuff);
            }
            
            // return the outMessage for processing
            if ( outMsg != null ) {
                return outMsg;
            } else {
                throw new Exception("Consumer: Null Out message in INOUT message exchange. ");
            }
        } else if (ExchangeStatus.DONE.equals(status) ) {
            // can not get DONE on Consumer side in INOUT message exchange.
            throw new Exception("Consumer: Illegal status DONE on INOUT message exchange");
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            // error can occur any time. so just return the error back to client.
            Exception serverSideEx = inoutExchange.getError();
            StringBuffer exMsgBuff = Helper.getExceptionStackTrace(serverSideEx);
            throw new Exception("Consumer:INOUT Message Exchange status ERROR.\n" + exMsgBuff);
        } else {
            throw new Exception("Consumer:INOUT Message Exchange error. status: " + status);
        }
    }
    /**
     * helper method to find the active ServiceEndpiont for the service described with the
     * serviceDescriptor. This method looks for the Active ServiceEndpoint using interface or
     * servicename or service name and the endpointname.
     */
    private ServiceEndpoint findEchoServiceEndpoint() {
        
        QName serviceType = this.mEchoServiceDescriptor.getInterfaceQName();
        QName serviceName = this.mEchoServiceDescriptor.getServiceQName();
        String endpointName = this.mEchoServiceDescriptor.getEndpointName();
        
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
    private InOut createInOutMessageExchange(QName operation, ServiceEndpoint serviceEndpoint)
    throws MessagingException {
        
        InOut inOutME = null;
        DeliveryChannel channel = this.getDeliveryChannel();
        // create message exchange factory for the endpiont
        MessageExchangeFactory factory =  channel.createExchangeFactory(serviceEndpoint);
        // create INOUT Message Exchange
        inOutME = factory.createInOutExchange();
        // set operation
        inOutME.setOperation(operation);
        // create IN Nomralized Message
        NormalizedMessage inMsg = inOutME.createMessage();
        // set IN Normalized message on message exchange
        inOutME.setInMessage(inMsg);
        
        return inOutME;
    }
    /** creates the JMXEndpointMBean implementation.
     */
    private void createJMXEndpointMBean() {
        // JMX EndpointMBean implementation that initiates in-out message exchange.
        this.mEchoJMXEndpoint = new JMXEndpointMBean() {
            // external service consumer calls this method using jmx interface.
            public StringBuffer sendMessage(
                String operation, StringBuffer inputDoc) throws Exception {
                // when receive input from external service consumer,
                // start in-out message exchange.
                return doInOutMessageExchange(operation, inputDoc);
            }
        };
    }
    /** creates jmx objectname for the JMXEndpointMBean
     * @throws javax.management.MalformedObjectNameException
     * @return ObjectName
     */
    private ObjectName createJMXEndpointMBeanObjectName()
    throws MalformedObjectNameException {
        String objectName = JMX_DOMAIN + ":" +
            JMX_ENDPOINT_ADDRESS_KEY + "=" + JMX_ENDPOINT_ADDRESS_VALUE;
        return new ObjectName(objectName);
    }
    /** registers the JMXEndpointMBean implemenataion with MBean server.
     * @throws javax.management.NotCompliantMBeanException
     * @throws javax.management.MalformedObjectNameException
     * @throws javax.management.InstanceAlreadyExistsException
     * @throws javax.management.MBeanRegistrationException
     */
    public void registerJMXEndpointMBean()
    throws NotCompliantMBeanException, MalformedObjectNameException,
        InstanceAlreadyExistsException, MBeanRegistrationException {
        // open jmx connectivity to external consumers by registering the
        // external endpoint mbean implementation.
        StandardMBean mbean = new StandardMBean(this.mEchoJMXEndpoint, JMXEndpointMBean.class);
        ObjectName objectName = createJMXEndpointMBeanObjectName();
        this.getComponentContext().getMBeanServer().registerMBean(mbean, objectName);
    }
    /**
     *
     * @param endpointAddressMBean
     * @throws javax.management.MalformedObjectNameException
     */
    public void unregisterJMXEndpointMBean()
    throws MalformedObjectNameException {
        // close the jmx  connectivity to external consumers by unregistering
        // the external endpoint mbean.
        ObjectName objectName = createJMXEndpointMBeanObjectName();
        try {
            this.getComponentContext().getMBeanServer().unregisterMBean(objectName);
        } catch (MBeanRegistrationException ex) {
            ex.printStackTrace();
        } catch (InstanceNotFoundException ex) {
            ex.printStackTrace();
        }
    }
}
