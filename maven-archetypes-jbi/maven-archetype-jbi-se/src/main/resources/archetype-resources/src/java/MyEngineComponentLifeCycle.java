/*
 * MyEngineComponentLifeCycle.java
 */
package ${package};

import com.sun.jbi.sample.component.common.ComponentLifeCycleImpl;
import com.sun.jbi.sample.component.common.EchoServiceDescriptor;
import com.sun.jbi.sample.component.common.Helper;
import java.io.StringReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

/**
 * This class extends the default ComponentLifeCycleImpl to provide MyEngine SE
 * specific implementation of the ComponentLifeCycle.
 *
 * This implementation makes MyEngine SE as a Service Provider for the EchoService
 * that can receive and process a InOut message exchange on a activated service 
 * endpoint corresponding to the EchoService that this component provides.
 *
 * This implementation shows various steps involved in providing a service from
 * a Service Engine during its lifecycle.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.ComponentLifeCycleImpl
 * @author chikkala
 */
public class MyEngineComponentLifeCycle extends ComponentLifeCycleImpl {
    
    /** delivery channel accept time out */
    private final static long DC_ACCEPT_TIME_OUT = 3000; // milliseconds
    /** receiver thread wait time before polling for messages after woke up **/
    private final static long RECEIVER_WAIT_TIME = 2000; // milliseconds
    /** receiver thread wait time before force shutdown */
    private final static long RECEIVER_SHUTDOWN_WAIT_TIME = 10; // seconds
    /** Synchronous send timeout  */
    private static final long SEND_SYNC_TIMEOUT = 60000;
    /** NMR delivery channel */
    private DeliveryChannel mDeliveryChannel;
    /** receiver thread accept message exchange condition */
    private Boolean mCanAccept = false;
    /** receiver thread termination condition */
    private Boolean mContinue = true;
    /** receiver thread executor service */
    private ExecutorService mReceiverThreadMgr;
    /** Echo Service implementation.   */
    private EchoService mEchoService;
    /** echo service description */
    private EchoServiceDescriptor mEchoServiceDescriptor;
    /** Echo ServiceEndpoint  */
    private ServiceEndpoint mEchoServiceEndpoint;
    
    /** constructor */
    public MyEngineComponentLifeCycle() {
        super();
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
        openDeliveryChannel();         // 1. Open a delivery channel
        initMessageExchangeReceiver(); // 2. create message receiver thread
        initServiceProviders();        // 3. initialize service providers
        this.getLogger().info(this.getComponentContext().getComponentName() + " Initialized");
    }
    /**
     * Start the component.
     * @see javax.jbi.component.ComponentLifeCycle\#start()
     * @throws javax.jbi.JBIException on error.
     */
    public final void start() throws JBIException {
        
        activateServiceProviders();     // 1. Activate the Service Providers
        startMessageExchangeReceiver(); // 2. Start message exchange processing
        this.getLogger().info(this.getComponentContext().getComponentName() + " Started.");
    }
    /**
     * Stop the component.
     * @see javax.jbi.component.ComponentLifeCycle\#stop()
     * @throws javax.jbi.JBIException on error.
     */
    public final void stop() throws JBIException {
        
        stopMessageExchangeReceiver(); // 1. Stop message exchange processing.
        deactivateServiceProviders();  // 2. Deactivate Service Providers.
        this.getLogger().info(this.getComponentContext().getComponentName() + "  Stopped.");
    }
    /**
     * Shut down the component.
     * @see javax.jbi.component.ComponentLifeCycle\#shutDown()
     * @throws javax.jbi.JBIException on error.
     */
    public final void shutDown() throws JBIException {
        // release all resources
        removeMessageExchangeReceiver(); // 1. end message exchange receiver thread
        closeDeliveryChannel();          // 2. Close delivery channel.
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
     * service providers initialization such as creating the service descriptions,
     * the resources required, etc.
     * This method is invoked in the implementation of ComponentLifeCycle init
     * method
     * @throws javax.jbi.JBIException
     */
    private void initServiceProviders()  throws JBIException {
        // create Service implementation
        this.mEchoService = EchoService.EchoServiceImpl.getInstance();
        // generate jbi internal endpoint name for the service endpoint
        String echoServiceEndpointName =
            EchoServiceDescriptor.generateJBIInternalEndpointName(
            this.getComponentContext().getComponentName());
        // create EchoServiceDescriptor that describes the service.
        this.mEchoServiceDescriptor =
            new EchoServiceDescriptor(MessageExchange.Role.PROVIDER,
            echoServiceEndpointName);
    }
    /**
     * service providers initialization such as creating the service descriptions and
     * activating the service endpoints should be done. If the component supports
     * deployment, deployment manager should be notified to active service providers
     * from the deployed service units.
     * This method is invoked in the implementation of ComponentLifeCycle start
     * method
     * @throws javax.jbi.JBIException
     */
    private void activateServiceProviders() throws JBIException {
        // activate Echo Service Endpoint
        QName serviceQName = this.mEchoServiceDescriptor.getServiceQName();
        String endpointName = this.mEchoServiceDescriptor.getEndpointName();
        this.mEchoServiceEndpoint =
            this.getComponentContext().activateEndpoint(serviceQName, endpointName);
        this.getLogger().info("Activated endpoint " + this.mEchoServiceEndpoint);
    }
    /**
     * disable the service providers so that the service consumers can not
     * find the service endpoints for this service provider. This method is
     * invoked in the implementation of ComponentLifeCycle stop method.
     * @throws javax.jbi.JBIException
     */
    private void deactivateServiceProviders()  throws JBIException {
        // deactivate Echo Service Endpoint
        this.getComponentContext().deactivateEndpoint(this.mEchoServiceEndpoint);
        this.getLogger().info("Deactivated endpoint " + this.mEchoServiceEndpoint);
    }
    /**
     * creates a message receiver thread as part of the component initialization.
     * This component wait to receive message exchange from its delivery channel
     * in this thread and then processes the received message exchange object.
     */
    private void initMessageExchangeReceiver() {
        this.mReceiverThreadMgr = Executors.newSingleThreadExecutor();
        //
        this.mReceiverThreadMgr.execute(new Runnable() {
            public void run() {
                Thread t = Thread.currentThread();
                while ( mContinue ) {  // continue running the thread
                    if (mCanAccept) { // check for message exchange and process it
                        receiveAndProcessMessageExchange();
                    } else { // wait before polling for message exchange again.
                        try {
                            t.sleep(RECEIVER_WAIT_TIME);
                        } catch (InterruptedException interruptException) {
                            // someone must have interrupted this thread. do nothing.
                        }
                    }
                }
            }
        });
    }
    /**
     * allows the component to accept the message exchange objects from the
     * delivery channel and process it as part of the component startup process.
     */
    private void startMessageExchangeReceiver() {
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = true;  // start accepting the messages
        }
    }
    /**
     * stops the component from accepting the message exchange objects from the
     * delivery channel as part of the component stop process
     */
    private void stopMessageExchangeReceiver() {
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = false; // stop accepting the messages
        }
    }
    /**
     * removes the message receiver as part of the component shutdown process
     */
    private void removeMessageExchangeReceiver() {
        synchronized ( mContinue ) {
            mContinue = false;
        }
        boolean terminated = false;
        try {
            this.mReceiverThreadMgr.shutdown();
            terminated = this.mReceiverThreadMgr.awaitTermination(
                RECEIVER_SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            // do nothing
        } finally {
            if ( !terminated ) {
                this.mReceiverThreadMgr.shutdownNow(); // Force shutdown the receiver thread.
            }
        }
    }
    /**
     * this method pulls message exchange object from the delivery channel when
     * it is available and calls the appropriate message processing. In this case
     * it calls the InOut message exchange processing method processInOutMessageExchange
     * to process InOut message exchange as a Service provider.
     */
    private void receiveAndProcessMessageExchange() {
        try {
            if ( this.mDeliveryChannel != null ) {
                MessageExchange msgExchange = null;
                msgExchange = this.mDeliveryChannel.accept(DC_ACCEPT_TIME_OUT);
                if ( msgExchange != null ) {
                    processInOutMessageExchangeOnProvider(msgExchange);
                } else {
                    return; // delivery channel timeout occurred. do nothing.
                }
            } else {
                this.getLogger().info("Delivery Channel Not Opened for receiving messages");
            }
        } catch (MessagingException ex) {
            ex.printStackTrace();
        }
    }
    /**
     * This method processes the message exchange as a InOut message exchange
     * ServiceProvider. It retrieves In message and invokes the echo service to
     * process the In message and then constrcuts the Out message or a fault message
     * based on the echo service invocation results and then sends the message exchange
     * with the Out message or fault using the delivery channel's synchronous send.
     */
    private void processInOutMessageExchangeOnProvider(MessageExchange msgExchange)
    throws MessagingException {
        // Process inout message exchange.
        InOut inOutExchange = (InOut)msgExchange;
        ExchangeStatus status = inOutExchange.getStatus();
        
        if (ExchangeStatus.ACTIVE.equals(status) ) {
            // receive IN message.
            NormalizedMessage inMsg = inOutExchange.getInMessage();
            NormalizedMessage outMsg = null;
            Fault fault = null;
            Source inContent = null;
            Source outContent = null;
            Source faultContent = null;
            
            // process in message
            inContent = inMsg.getContent();
            // invoke the service operation
            try {
                outContent = this.mEchoService.echo(inContent);
            } catch (Exception ex) {
                // exception invoking the operation. so, set exception text as fault content.
                String faultText = Helper.getExceptionAsXmlText(ex);
                faultContent = Helper.createDOMSource(new StringReader(faultText));
            }
            // set out or fault message
            if ( outContent != null ) {
                // set the out message content.
                outMsg = inOutExchange.createMessage();
                outMsg.setContent(outContent);
                inOutExchange.setOutMessage(outMsg);
            } else if ( faultContent != null ) {
                fault = inOutExchange.createFault();
                fault.setContent(faultContent);
                inOutExchange.setFault(fault);
            }
            // send out or fault message.
            this.mDeliveryChannel.sendSync(inOutExchange, SEND_SYNC_TIMEOUT);
            
        } else if (ExchangeStatus.DONE.equals(status) ) {
            this.getLogger().info("InOut Message Exchange Provider received DONE :" +
                " END of service invocation");
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            Exception errEx = msgExchange.getError(); // get the error and print
            this.getLogger().info(
                "InOut Message Exchange Provider received Error: " + errEx.getMessage());
            msgExchange.getError().printStackTrace();
        }
    }
    
}
