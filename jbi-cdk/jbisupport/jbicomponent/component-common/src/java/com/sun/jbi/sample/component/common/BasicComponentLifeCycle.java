/*
 * AbstractComponentLifeCycle.java
 */

package com.sun.jbi.sample.component.common;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * This class implements the javax.jbi.component.ComponentLifeCycle. This is an
 * abstract class that keeps the reference to the component context and the lifecycle
 * extension mbeans. It implements the lifecycle method and provide the extended methods
 * for each lifecycle methods to implement component specific functionality.
 *<p>
 * It makes sure that the mininum initialization required for the component
 * lifecycle is implemented by this class or the classes extending this class.
 *<p>
 * Another important functionality implemented in this class is that it initializes the global
 * RuntimeContext for the component with the ComponentContext passed by the JBI runtime and the
 * opened delivery channel to make them available globally to any class in the component implementation
 * that may need it during component lifecycle
 *<p>
 * Service engine or a binding component lifecycle implementation can directly extend
 * this class for their required functionality and override some of the methods to provide
 * their own implementation.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @author chikkala
 */
public class BasicComponentLifeCycle implements ComponentLifeCycle {
    /** reference to the component runtime */
    private Component mCompRuntime;
    /** component context */
    private ComponentContext mComponentContext;
    /** Extension Mbean Name*/
    private ObjectName mExtensionMBeanName;
    /** Extension Mbean Implementation*/
    private StandardMBean mExtensionMBeanImpl;
    /** a message receiver that accepts messages from delivery channel */
    private MessageExchangeReceiver mMsgExchReceiver;
    
    /**
     * constructor for the ComponentLifecycle implementation.
     * @param compRuntime
     */
    protected BasicComponentLifeCycle(Component compRuntime) {
        this.mCompRuntime = compRuntime;
    }
    /**
     * @return Component
     */
    public final Component getComponentRuntime() {
        return this.mCompRuntime;
    }
    /**
     * @return ComponentContext component context.
     */
    public final ComponentContext getComponentContext() {
        return mComponentContext;
    }
    /**
     * @return component name
     */
    protected String getComponentName() {
        return (this.mComponentContext != null) ?
            this.mComponentContext.getComponentName() : null;
    }
    ///////////////////////////////////////////////////////////////////////////
    // ComponentLifeCycle interface implemenation
    ///////////////////////////////////////////////////////////////////////////
    
    /**
     * Initializes the component lifecycle. This method makes sure that the
     * component context passed by the jbi runtime is saved in the lifecycle
     * object before calling the other component initialization methods to
     * completely initialize the component.
     * @param context Component context
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     * @throws javax.jbi.JBIException on error
     */
    public final void init(ComponentContext context) throws JBIException {
        
        if ( context == null  ) {
            throw new JBIException("Null Component Context received in " +
                "Component Lifecycle init ");
        }
        this.mComponentContext = context; // 0. save the component context passed by jbi framework.
        // initialize the content
        initGlobalContext();           // 1. Initialize component runtime context.
        registerExtensionMBean();      // 2. Create and register extension mbean.
        openDeliveryChannel();         // 3. Open delivery channel.
        initMessageExchangeReceiver(); // 4. Create message exchange receiver.
        doInit();                      // 5. Do any component specific initialization such
        //    as static service providers and consumers initialization.
        
        this.getLogger().fine("Component " + this.getComponentName() + " initialized");
    }
    
    /**
     * Starts the component. Extended classes can override/implement the methods called
     * in this method to provide component specify start functionality.
     *
     * @see javax.jbi.component.ComponentLifeCycle#start()
     * @throws javax.jbi.JBIException on error
     */
    public final void start() throws JBIException {
        
        doStart();                         // 1. do any other component specific start tasks such as
                                           // activating any static service providers and consumers.
        startMessageExchangeProcessing();  // 2. begin accepting and processing message exchanges.
        
        this.getLogger().fine("Component " + this.getComponentName() + " started");
    }
    
    /**
     * Stop the component.Extended classes can override/implement the methods called
     * in this method to provide component specify stop functionality.
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#stop()
     */
    public final void stop() throws JBIException {
        
        stopMessageExchangeProcessing();  // 1. stop accepting and processing message exchanges.
        doStop();                         // 2. do any other component specific stop tasks such as
                                          //    deactivating any static service providers and consumers.
        this.getLogger().fine("Component " + this.getComponentName() + " stopped.");
    }
    
    /**
     * Shut down the component. Extended classes can override/implement the methods called
     * in this method to provide component specify shut down functionality.
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#shutDown()
     */
    public final void shutDown() throws JBIException {
        // clean up all resources.
        shutdownMessageExchangeReceiver();  // 1. remove the message receiver.
        closeDeliveryChannel();             // 2. close delivery channel
        unregisterExtensionMBean();         // 3. unregister and remove extension mbean if exists.
        doShutDown();                       // 4. do any other component specific clean up tasks such
        //    as clean any static service providers and consumer resource.
        this.getLogger().fine("Component " + this.getComponentName() + " shut down");
    }
    /**
     * create jmx object name for the extension mbean. default implementation does
     * not required to have the extension mbean.
     * @return ObjectName of the extension mbean
     */
    protected ObjectName createExtensionMBeanName() {
        return null;
    }
    /**
     * gets the reference to the extension mbean name.
     * @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName()
     * @return ObjectName of the extension mbean objectname or null if component
     * does not support extension mbean
     */
    public final ObjectName getExtensionMBeanName() {
        if ( this.mExtensionMBeanName == null ) {
            this.mExtensionMBeanName = this.createExtensionMBeanName();
        }
        return this.mExtensionMBeanName;
    }
    /**
     * create mbean implementation for the extension mbean as a StandardMBean. default
     * implementation does not require the extension mbean
     * @return the ExtensionMBean implementation as StandardMBean
     */
    protected StandardMBean createExtensionMBean() {
        return null;
    }
    /**
     * gets the reference to the extension mbean.
     * @return extension mbean implementation or null if component does not support.
     */
    protected final StandardMBean getExtensionMBean() {
        if ( this.mExtensionMBeanImpl == null ) {
            this.mExtensionMBeanImpl = this.createExtensionMBean();
        }
        return this.mExtensionMBeanImpl;
    }
    /**
     * if there is an extension mbean supported by the component, then register
     * it with the mbean server.
     * @throws JBIException on error registering mbean
     */
    protected final void registerExtensionMBean() throws JBIException {
        // creatre the extension mbean's object name
        ObjectName mbeanName = this.getExtensionMBeanName();
        // create the extension mbean implemenation.
        StandardMBean mbeanImpl = this.getExtensionMBean();
        
        if ( mbeanName == null || mbeanImpl == null ) {
            this.getLogger().fine(this.getComponentName() +
                " Does not have Extension MBean implemenation ");
            return;
        }
        // register with mbean only if object name and implementation are non null
        try {
            MBeanServer mbServer = RuntimeHelper.getComponentContext().getMBeanServer();
            mbServer.registerMBean(mbeanImpl, mbeanName);
        } catch (Exception e) {
            throw new JBIException("Failed to register Extension MBean for " +
                this.getComponentName(), e);
        }
    }
    /**
     * remove the registered extension mbean from the mbean server if any.
     */
    protected final void unregisterExtensionMBean() {
        ObjectName mbeanName = this.getExtensionMBeanName();
        if ( mbeanName == null ) {
            return;
        }
        try {
            MBeanServer mbServer = RuntimeHelper.getComponentContext().getMBeanServer();
            mbServer.unregisterMBean(mbeanName);
        } catch (Exception e) {
            this.getLogger().log(Level.WARNING,
                "Failed to unregister Extension MBean from MBeanServer for " + this.getComponentName(), e);
        }
    }
    /**
     * creates MessageExchangeReceiver implementation that handles receiving and processing
     * the message exchanges from the delivery channel. Component should implement
     * this method to provide the MessageReceiver.
     *
     * Component may return null indicating that they don't need the message receiver
     * that can receive and process message exchanges from delivery channel. For example,
     * components that have only service consumers which send a synchronous messages to
     * providers don't need this.
     *
     * @throws java.lang.Exception
     * @return MessageExchangeReceiver implementation. null if the component does not support it.
     * @see com.sun.jbi.sample.component.common.DefaultComponentLifecycle#createMessageExchangeReceiver
     */
    protected MessageExchangeReceiver createMessageExchangeReceiver() {
        return null;
    }
    /**
     * returns the message exchange receiver implementation reference
     * @return MessageExchangeReceiver interface
     */
    protected final MessageExchangeReceiver getMessageExchangeReceiver() {
        if ( this.mMsgExchReceiver == null ) {
            this.mMsgExchReceiver = createMessageExchangeReceiver();
        }
        return this.mMsgExchReceiver;
    }
    /**
     * creates a message receiver object as part of the component initialization.
     * @throws javax.jbi.JBIException
     */
    protected void initMessageExchangeReceiver() throws JBIException {
        // create and initialize the MessageExchangeReceiver
        MessageExchangeReceiver mxReceiver = getMessageExchangeReceiver();
        if ( mxReceiver != null ) {
            mxReceiver.initReceiver();
        }
    }
    /**
     * allows the component to accept the message exchange objects from the
     * delivery channel and process it as part of the component startup process.
     * @throws javax.jbi.JBIException
     */
    protected void startMessageExchangeProcessing() throws JBIException {
        // start message processing
        MessageExchangeReceiver mxReceiver = getMessageExchangeReceiver();
        if ( mxReceiver != null ) {
            mxReceiver.startProcessing();
        }
    }
    /**
     * stops the component from accepting the message exchange objects from the
     * delivery channel as part of the component stop process
     * @throws javax.jbi.JBIException
     */
    protected void stopMessageExchangeProcessing() throws JBIException {
        // stop message processing
        MessageExchangeReceiver mxReceiver = getMessageExchangeReceiver();
        if ( mxReceiver != null ) {
            mxReceiver.stopProcessing();
        }
    }
    /**
     * removes the message receiver as part of the component shutdown process
     * @throws javax.jbi.JBIException
     */
    protected void shutdownMessageExchangeReceiver() throws JBIException {
        // clean up message processing.
        MessageExchangeReceiver mxReceiver = getMessageExchangeReceiver();
        if ( mxReceiver != null ) {
            mxReceiver.shutdownReceiver();
        }
    }
    /**
     * Returns logger initialized from the component context or a default logger.
     * @return Logger
     */
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    /**
     * initializes the RuntimeContext during component init lifecycle phase. This
     * includes setting the ComponentContext and loggers etc. extended classes must
     * save the component context in the RuntimeContext in this methods implementation.
     * @see com.sun.jbi.sample.component.common.DefaultComponentLifecycle#initGlobalContext
     */
    protected void initGlobalContext() {
        RuntimeContext.getInstance().setComponentContext(this.getComponentContext());
        RuntimeContext.getInstance().setLogger(this.getClass().getName(), null);
    }
    /** opens the delivery channel to accept or send message exchange objects
     * @see com.sun.jbi.sample.component.common.DefaultComponentLifecycle#openDeliveryChannel
     */
    protected void openDeliveryChannel() {
        try {
            DeliveryChannel channel = this.getComponentContext().getDeliveryChannel();
            RuntimeContext.getInstance().setDeliveryChannel(channel);
        } catch (MessagingException ex) {
            ex.printStackTrace();
        }
    }
    /** closes the delivery channel as part of the component shutdown process.
     * @see com.sun.jbi.sample.component.common.DefaultComponentLifecycle#closeDeliveryChannel
     */
    protected void closeDeliveryChannel() {
        // closes delivery channel and remove the reference.
        try {
            DeliveryChannel channel = RuntimeHelper.getDeliveryChannel();
            if ( channel != null ) {
                channel.close();
            }
        } catch (MessagingException ex) {
            ex.printStackTrace();
        } finally {
            RuntimeContext.getInstance().setDeliveryChannel(null);
        }
    }
    /**
     * chance to extended classes to do any extra component specific init tasks. If the component
     * supports static service providers or consumers without deployment support, they can be
     * initialized here in the class extended from this class in implementing such a component.
     * This method is invoked in the implementation of ComponentLifeCycle#init method by this class.
     * @throws javax.jbi.JBIException
     * @see com.sun.jbi.sample.component.common.AbstractComponentLifecycle#init
     */
    protected void doInit() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do any extra component specific start tasks. If the component
     * supports static service providers or consumers without deployment support, they can be
     * activated here in the class extended from this class in implementing such a component.
     * This method is invoked in the implementation of ComponentLifeCycle#start method by this class.
     * @throws javax.jbi.JBIException
     * @see com.sun.jbi.sample.component.common.AbstractComponentLifecycle#start
     */
    protected void doStart() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do any extra component specific stop tasks. If the component
     * supports static service providers or consumers without deployment support, they can be
     * deactivated here in the class extended from this class in implementing such a component.
     * This method is invoked in the implementation of ComponentLifeCycle#stop method by this class.
     * @throws javax.jbi.JBIException
     * @see com.sun.jbi.sample.component.common.AbstractComponentLifecycle#stop
     */
    protected void doStop() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do any extra component specific shutdown tasks.If the component
     * supports static service providers or consumers without deployment support, they can be
     * cleaned up here in the class extended from this class in implementing such a component.
     * This method is invoked in the implementation of ComponentLifeCycle#shutdown method by this class.
     * @throws javax.jbi.JBIException
     * @see com.sun.jbi.sample.component.common.AbstractComponentLifecycle#shutdown
     */
    protected void doShutDown() throws JBIException {
        //NOOP
    }
    
}
