/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * AbstractComponentLifeCycle.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.Jbi4EjbLifeCycle;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * This class is an abstract implemenation of the ComponentLifeCycle. It implements
 * the component lifecycle methods and provides necessary hooks to the extended classes
 * to supply the component specific implemenation for the lifecycle functionality.
 *
 * @see javax.jbi.ComponentLifeCycle
 *
 * @author Sun Microsystems, Inc.
 */
public abstract class AbstractComponentLifeCycle implements ComponentLifeCycle {
	/** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger( AbstractComponentLifeCycle.class);    
    private static final Messages MESSAGES = Messages.getMessages( AbstractComponentLifeCycle.class);
	
    /** reference to the component runtime */
    private ComponentRuntime mCompRuntime;
    /** Extension Mbean Name*/
    private ObjectName mExtensionMBeanName;
    /** Extension Mbean Implemenation*/
    private StandardMBean mExtensionMBeanImpl;
    /** a message receiver that accepts messages from delivery channel */
    private MessageExchangeReceiver mMsgExchReceiver;
    
    /** private constructor  */
    private AbstractComponentLifeCycle() {}
    /**
     * constructor for the ComponentLifecycle implementation.
     * @param compRuntime
     */
    protected AbstractComponentLifeCycle(ComponentRuntime compRuntime) {
        this.mCompRuntime = compRuntime;
    }
    
    ///////////////////////////////////////////////////////////////////////////
    // ComponentLifeCycle interface implemenation
    ///////////////////////////////////////////////////////////////////////////
    
    /**
     * Initialize the component.
     * @param context
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     */
    public final void init(ComponentContext context) throws JBIException {
        
        if ( context == null  ) {
            throw new JBIException("Null Component Context received in " +
                    "Component Lifecycle init ");
        }
        // initialize the content
        initContext(context);
        // initialize the message exchange handler factory
        initMessageExchangeHandlerFactory();
        // initialize the message receiver
        initMessageExchangeReceiver();
        // create and register extension mbean
        registerExtensionMBean();
        // call the onInit to allow other initialization that is specific to the component
        onInit();
        
                LOG.info("EJB000201_Component_initialized", new Object[]{RuntimeHelper.getComponentName()});
    }
    
    /**
     * Start the component.
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#start()
     */
    public final void start() throws JBIException {
        
        // open the delivery channel from the component context
        openDeliveryChannel();
        
        activateServiceProviders();
        activateServiceConsumers();
        
        startMessageExchangeProcessing();
        // call onStart to perform component specific tasks on start
        onStart();
        //RuntimeHelper.logInfo("Component " + RuntimeHelper.getComponentName() + " started");
        LOG.info("EJB000202_Component_started", new Object[]{RuntimeHelper.getComponentName()});
    }
    
    /**
     * Stop the component.
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#stop()
     */
    public final void stop() throws JBIException {
        
        stopMessageExchangeProcessing();
        // deactivate endpoints
        deactivateServiceConsumers();
        deactivateServiceProviders();
        // close channel. No further messages to be processed.
        closeDeliveryChannel();
        // call onStop to perform component specific tasks on stop
        onStop();
        LOG.info("EJB000203_Component_stopped", new Object[]{RuntimeHelper.getComponentName()});
            }
    
    /**
     * Shut down the component.
     * @throws javax.jbi.JBIException
     * @see javax.jbi.component.ComponentLifeCycle#shutDown()
     */
    public final void shutDown() throws JBIException {
        // remove the message receiver.
        shutdownMessageExchangeReceiver();
        // unregister extension mbean and remove it
        unregisterExtensionMBean();
        // call onShutdown to perform component specific tasks on shutdown
        onShutDown();
        LOG.info("EJB000204_Component_shut_down", new Object[]{RuntimeHelper.getComponentName()});
    }
    
    /**
     * this is a default implementation which does not have any extension mbeans. extended
     * classes can return the mbean name for the extension mbeans if the component supports.
     *
     *
     * @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName()
     * @return
     */
    public final ObjectName getExtensionMBeanName() {
        return this.mExtensionMBeanName;
    }
    /**
     * if there is an extension mbean supported by the component, then register
     * it with the mbean server.
     */
    private void registerExtensionMBean()  throws JBIException {
        
        this.mExtensionMBeanName = this.createExtensionMBeanName();
        
        if ( this.mExtensionMBeanName == null ) {
            RuntimeHelper.logDebug("No Extension MBean is registerd with MBeanServer for " +
                    RuntimeHelper.getComponentName());
            return;
        }
        // create the extension mbean implemenation if the object name is created.
        this.mExtensionMBeanImpl = this.createExtensionMBean();
        
        if ( this.mExtensionMBeanImpl == null ) {
            this.mExtensionMBeanName = null;
            RuntimeHelper.logDebug("No Extension MBean is registerd with MBeanServer for " +
                    RuntimeHelper.getComponentName());
            return;
        }
        // register with mbean only if object name and implementation are non null
        try {
            MBeanServer mbServer = RuntimeHelper.getComponentContext().getMBeanServer();
            mbServer.registerMBean(this.mExtensionMBeanImpl, this.mExtensionMBeanName);
        } catch (Exception e) {
            this.mExtensionMBeanName = null;
            this.mExtensionMBeanImpl = null;
            String msg=MESSAGES.getString("EJB000205_Can_not_register_extension_MBean_with_MBeanServer", new Object[] {RuntimeHelper.getComponentName()});
            LOG.error(msg,e);
            throw new JBIException(msg,e);

        }
    }
    /**
     * remove the registered extension mbean from the mbean server.
     */
    private void unregisterExtensionMBean()  throws JBIException {
        if ( this.mExtensionMBeanName != null ) {
            try {
                MBeanServer mbServer = RuntimeHelper.getComponentContext().getMBeanServer();
                mbServer.unregisterMBean(this.mExtensionMBeanName);
            } catch (Exception e) {
            	String msg=MESSAGES.getString("EJB000206_Can_not_register_extension_MBean_from_MBeanServer", new Object[] {RuntimeHelper.getComponentName()});
                LOG.error(msg,e);
                throw new JBIException(msg,e);
            } finally {
                this.mExtensionMBeanName = null;
                this.mExtensionMBeanImpl = null;
            }
        }
    }
    
    /**
     *
     * @return
     */
    public final ComponentRuntime getComponentRuntime() {
        return this.mCompRuntime;
    }
    /**
     * initializes the RuntimeContext using ComponentContext passed by init method.
     * @param context
     */
    private void initContext(ComponentContext context) {
        RuntimeContext.getInstance().setComponentContext(context);
        RuntimeContext.getInstance().setLogger(this.getClass().getPackage().getName(), null);
    }
    /**
     * creates a message receiver object as part of the component initialization.
     * @throws javax.jbi.JBIException
     */
    private void initMessageExchangeReceiver() throws JBIException {
        try {
            
            // create message receiver
            this.mMsgExchReceiver = createMessageExchangeReceiver();
            if ( this.mMsgExchReceiver != null ) {
                this.mMsgExchReceiver.initReceiver();
            }
        } catch (Exception ex) {
        	String msg=MESSAGES.getString("EJB000207_Failure_in__init_Message_Exchange_Receiver");
            LOG.error(msg,ex);
            throw new JBIException(msg,ex);
        }
    }
    /**
     * removes the message receiver as part of the component shutdown process
     * @throws javax.jbi.JBIException
     */
    private void shutdownMessageExchangeReceiver() throws JBIException {
        try {
            if ( this.mMsgExchReceiver != null ) {
                this.mMsgExchReceiver.shutdownReceiver();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            this.mMsgExchReceiver = null;
        }
    }
    /**
     * allows the component to accept the message exchange objects from the
     * delivery channel and process it as part of the component startup process.
     * @throws javax.jbi.JBIException
     */
    private void startMessageExchangeProcessing() throws JBIException {
        try {
            // start message processing
            if ( this.mMsgExchReceiver != null ) {
                this.mMsgExchReceiver.startProcessing();
            }
        } catch (Exception ex) {
        	String msg=MESSAGES.getString("EJB000208_Failure_in__start_Message_Exchange_Processing");
            LOG.error(msg,ex);
            throw new JBIException(msg,ex);
        }
    }
    /**
     * stops the component from accepting the message exchange objects from the
     * delivery channel as part of the component stop process
     * @throws javax.jbi.JBIException
     */
    private void stopMessageExchangeProcessing() throws JBIException {
        try {
            // stop message processing
            if ( this.mMsgExchReceiver != null ) {
                this.mMsgExchReceiver.stopProcessing();
            }
        } catch (Exception ex) {
        	String msg=MESSAGES.getString("EJB000209_Failure_in__stop_Message_Exchange_Processing");
            LOG.error(msg,ex);
            throw new JBIException(msg,ex);
        }
        
    }
    /**
     * opens the delivery channel to accept the message exchange objects or
     * send the message exchange objects
     */
    private void openDeliveryChannel() {
        RuntimeContext.getInstance().openDeliveryChannel();
    }
    /**
     * closes the delivery channel as part of the component shutdown process.
     */
    private void closeDeliveryChannel() {
        RuntimeContext.getInstance().closeDeliveryChannel();
    }
    /**
     * create jmx object name for the extension mbean
     */
    protected abstract ObjectName createExtensionMBeanName();
    /**
     * create mbean implemenation for the extension mbean as a StandardMBean
     */
    protected abstract StandardMBean createExtensionMBean();
    /**
     * initializes the message exchange handler factory. components implement this
     * method to provide their own handler factory
     * @throws javax.jbi.JBIException
     */
    protected abstract void initMessageExchangeHandlerFactory() throws JBIException; 
    /**
     * creates a message receiver object that handles receiving and processing
     * the message exchanges from the delivery channel. Component should implement
     * this method to provide the MessageReceiver.
     *
     * Component may return null indicating that they don't need the message receiver
     * that can receive and process message exchanges from delivery channel. For example,
     * components that have only service consumers which send a synchronous messages to
     * providers don't need this.
     * @throws java.lang.Exception
     * @return
     */
    protected abstract MessageExchangeReceiver createMessageExchangeReceiver() throws Exception;
    /**
     * service providers initialization such as creating the service descriptions and
     * activating the service endpoints should be done. If the component supports
     * deployment, deployment manager should be notified to active service providers
     * from the deployed service units.
     * This method is invoked in the implementation of ComponentLifeCycle start
     * method
     * @throws javax.jbi.JBIException
     */
    protected abstract void activateServiceProviders() throws JBIException;
    /**
     * disable the service providers so that the service consumers can not
     * find the service endpoints for this service provider.
     * This method is invoked in the implementation of ComponentLifeCycle stop
     * method
     * @throws javax.jbi.JBIException
     */
    protected abstract void deactivateServiceProviders() throws JBIException;
    /**
     * service consumer initialization such as creating the service descriptions and
     * activating the protocol specific external endpoints should be done. If the component
     * supports  deployment, deployment manager should be notified to initialize service
     * consumers from the deployed service units.
     * This method is invoked in the implementation of ComponentLifeCycle start
     * method
     * @throws javax.jbi.JBIException
     */
    protected abstract void activateServiceConsumers() throws JBIException;
    /**
     * disables the service consumers so that the service consumer can not
     * send message exchange to the providers. This is called in the implemenation
     * of component lifecycle stop method
     * This method is invoked in the implementation of ComponentLifeCycle stop
     * method
     * @throws javax.jbi.JBIException
     */
    protected abstract void deactivateServiceConsumers() throws JBIException;
    /**
     * chance to extended classes to do the component specific init
     * @throws javax.jbi.JBIException
     */
    protected void onInit() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do the component specific start
     * @throws javax.jbi.JBIException
     */
    protected void onStart() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do the component specific stop
     * @throws javax.jbi.JBIException
     */
    protected void onStop() throws JBIException {
        //NOOP
    }
    /**
     * chance to extended classes to do the component specific shutdown
     * @throws javax.jbi.JBIException
     */
    protected void onShutDown() throws JBIException {
        //NOOP
    }
    
}
