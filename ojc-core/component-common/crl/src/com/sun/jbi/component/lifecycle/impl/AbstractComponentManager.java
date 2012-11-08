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
 * @(#)AbstractComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.lifecycle.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.qos.config.RuntimeConfigurationMBean;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.util.MBeanHelper;
import com.sun.jbi.component.alerter.Alerter;
import com.sun.jbi.component.alerter.AlerterFactory;
import com.sun.jbi.component.alerter.Event.ComponentType;
import com.sun.jbi.component.alerter.NotificationEvent.OperationalState;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.endpoint.impl.DefaultEndpointManager;
import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.CallbackRegistry;
import com.sun.jbi.crl.mep.CorrelationMap;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.MessageListenerFactory;
import com.sun.jbi.crl.mep.ThreadManager;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.mep.impl.DefaultAcceptManager;
import com.sun.jbi.crl.mep.impl.DefaultCallbackRegistry;
import com.sun.jbi.crl.mep.impl.DefaultCorrelationMap;
import com.sun.jbi.crl.mep.impl.DefaultManagerContext;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Abstract implementation of {@link ComponentManager}.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractComponentManager implements ComponentManager {
    private ComponentContext mComponentCtx = null;
    private ServiceUnitManager mServiceUnitMgr = null;
    private ObjectName mExtensionMBeanName = null;

    private AcceptManager mAcceptManager = null;
    private CRLMessageExchangeFactory mExchangeFactory = null;
    private CallbackRegistry mCallbackRegistry = null;
    private ManagerContext mManagerCtx;
    private CorrelationMap mCorrelationMap = null;
    private EndpointManager mEndpointManager = null;

    private ComponentConfig mConfig;
    private Object mRuntimeMBean;
    private MBeanHelper mMBeanHelper;
    private Logger mLogger = Logger.getLogger(this.getClass().getName());
    private Alerter mAlerter;
    
    // the thread context class-loader utility is initialized with the 
    // the component thread context class loader. If initialized this 
    // has to initialized when the component is shutdown. 
    private CustomClassLoaderUtil mCustomCLUtil;
    
    /* **********                 Template Methods                 ********** */
    
    /**
     * Creates, initializes, and returns the {@link AcceptManager} 
     * for this component.
     * <p>
     * The following steps should be considered when 
     * initializing the <code>AcceptManager</code>:
     * <ul>
     *      <li><b>REQUIRED:</b> Register {@link Processor} implementations
     *          with the <code>AcceptManager</code>'s {@link ExchangeRouter}.</li>
     *      <li>Specify an <code>ExchangeRouter</code> (OPTIONAL).</li>
     *      <li>Specify a {@link ListenerContext} (OPTIONAL).</li>
     *      <li>Specify a {@link MessageListenerFactory} (OPTIONAL).</li>
     *      <li>Specify a {@link ThreadManager} (OPTIONAL).</li>
     *      <li>Specify the poller count (OPTIONAL).</li>
     * </ul>
     * 
     * @see AcceptManager
     * @see DefaultAcceptManager
     */
    protected abstract AcceptManager initAcceptManager();

    /**
     * Creates, initializes, and returns the {@link EndpointManager} 
     * for this component.
     * 
     * @see EndpointManager
     * @see DefaultEndpointManager
     */
    protected abstract EndpointManager initEndpointManager();
    
    /**
     * Initializes the component's runtime configuration MBean during
     * {@link ComponentLifeCycle#init(ComponentContext)} but prior
     * to {@link #initAcceptManager()}.
     * The default implementation returns <code>null</code>.
     * 
     * @return The component's runtime configuration MBean or <code>null</code>.
     * @throws JBIException if an error occurs initializing the MBean.
     */
    protected Object initRuntimeMBean() throws JBIException {
        return null;    // default impl is a no-op
    }
    
    /**
     * Creates, initializes, and returns the {@link ServiceUnitManager} 
     * for this component.
     * <p>
     * <b>NOTE:</b> This template method does not require explicit 
     * implementation by subclasses. The current implementation
     * instantiates an instance of  
     * {@link DefaultServiceUnitManager#DefaultServiceUnitManager(ComponentContext, EndpointManager)}
     * using this component's <code>ComponentContext</code> and 
     * <code>EndpointManager</code>.
     * 
     * @see ServiceUnitManager
     * @see DefaultServiceUnitManager
     * @see #getComponentContext()
     * @see #getEndpointManager()
     */
    protected ServiceUnitManager initServiceUnitManager() {
        return new DefaultServiceUnitManager(getManagerContext(),
                                             getEndpointManager());
    }
    
    /* **********                 ComponentManager                 ********** */
    
    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getAcceptManager() */
    public AcceptManager getAcceptManager() {
        return mAcceptManager;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getCRLExchangeFactory() */
    public CRLMessageExchangeFactory getExchangeFactory() {
        return mExchangeFactory;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getCallbackRegistry() */
    public CallbackRegistry getCallbackRegistry() {
        return mCallbackRegistry;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getComponentContext() */
    public ComponentContext getComponentContext() {
        return mComponentCtx;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getCorrelationMap() */
    public CorrelationMap getCorrelationMap() {
        return mCorrelationMap;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getEndpointManager() */
    public EndpointManager getEndpointManager() {
        return mEndpointManager;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#getManagerContext() */
	public ManagerContext getManagerContext() {
		return mManagerCtx;
	}
	
	public CustomClassLoaderUtil getCustomClassLoaderUtil() {
		return mCustomCLUtil;
	}

	/** @see com.sun.jbi.component.lifecycle.ComponentManager#setAcceptManager(com.sun.jbi.crl.mep.AcceptManager) */
    protected void setAcceptManager(AcceptManager amgr) {
        mAcceptManager = amgr;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#setCallbackRegistry(com.sun.jbi.crl.mep.CallbackRegistry) */
    protected void setCallbackRegistry(CallbackRegistry registry) {
        mCallbackRegistry = 
                (registry == null) 
                        ? DefaultCallbackRegistry.createCallbackRegistry(
                        		getComponentContext(), getExchangeFactory())
                        : registry;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#setComponentContext(javax.jbi.component.ComponentContext) */
    protected void setComponentContext(ComponentContext ctx) {
        mComponentCtx = ctx;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#setCorrelationMap(com.sun.jbi.crl.mep.CorrelationMap) */
    protected void setCorrelationMap(CorrelationMap cmap) {
        mCorrelationMap = (cmap == null) ? new DefaultCorrelationMap() : cmap;
    }

    /** @see com.sun.jbi.component.lifecycle.ComponentManager#setEndpointManager(com.sun.jbi.component.endpoint.EndpointManager) */
    protected void setEndpointManager(EndpointManager emgr) {
        mEndpointManager = emgr;
    }

    protected void setManagerContext(ManagerContext ctx) {
    	mManagerCtx = ctx;
    }
    
    /* **********                     Component                    ********** */
    
    /** @see javax.jbi.component.Component#getLifeCycle() */
    public ComponentLifeCycle getLifeCycle() {
        /*
         * JBI implementation must call:
         *  - this method prior to any other methods of Component interface.
         *  - ComponentLifeCycle.init prior to any methods on 
         *          Component interface OR ComponentLifeCycle interface
         */
        return this;
    }

    /** @see javax.jbi.component.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint) */
    public Document getServiceDescription(ServiceEndpoint endpoint) {
        return null;
    }

    /** @see javax.jbi.component.Component#getServiceUnitManager() */
    public ServiceUnitManager getServiceUnitManager() {
        return mServiceUnitMgr;
    }

    /** @see javax.jbi.component.Component#isExchangeWithConsumerOkay(javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange) */
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint,
                                              MessageExchange exchange) {
        return true;
    }

    /** @see javax.jbi.component.Component#isExchangeWithProviderOkay(javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange) */
    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint,
                                              MessageExchange exchange) {
        return true;
    }

    /** @see javax.jbi.component.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment) */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        return null;
    }

    /* **********                ComponentLifeCycle                ********** */
    
    /** @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    /**
     * This implementations performs the following actions:
     * <ol>
     *      <li>Sets the {@link ComponentContext} via 
     *          {@link #setComponentContext(ComponentContext)}.</li>
     *      <li>Creates this component's {@link CRLMessageExchangeFactory}.</li>
     *      <li>Initializes and sets this component's {@link EndpointManager}.</li>
     *      <li>Initializes and sets this component's {@link ServiceUnitManager}.</li>
     *      <li>Initializes and sets this component's {@link AcceptManager}.</li>
     * </ol>
     * 
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     * @see #initEndpointManager()
     * @see #initServiceUnitManager(EndpointManager)
     * @see #initAcceptManager() 
     */
    public void init(ComponentContext ctx) throws JBIException {
    	
    	// set the CustomClassLoaderUtil object
    	mCustomCLUtil = new CustomClassLoaderUtil(getClass().getClassLoader());
    	
    	// minimum basic initialization for CRL-based componentss
        setComponentContext(ctx);
        mLogger = LogUtil.getLogger(ctx, this.getClass().getName());
        mAlerter = AlerterFactory.newAlerter();
        mManagerCtx = new DefaultManagerContext(this, new BaseMessagingChannel(ctx));
        // TODO do we need a protected setter for exchange factory? probably not
        mExchangeFactory = CRLMessageExchangeFactory.getInstance(getManagerContext());
        setEndpointManager(initEndpointManager());
        
        // parse component descriptor to make configuration available
        mConfig = ComponentConfig.parse(ctx.getInstallRoot());
        // load persisted configuration, won't fail if no config.properties
        ConfigPersistence.loadConfig(mConfig, ctx.getWorkspaceRoot());
        // init MBeanHelper to make available to SUMgrs who want it...
        mMBeanHelper = new MBeanHelper(ctx);
        setServiceUnitManager(initServiceUnitManager());
        
        // register configuration MBean prior to AcceptManager initialization
        mRuntimeMBean = initRuntimeMBean();
        if (getRuntimeMBean() != null) {
            getMBeanHelper().registerMBean(
                    RuntimeConfigurationMBean.CONFIGURATION_EXTENSION, 
                    getRuntimeMBean());
        }
        setAcceptManager(initAcceptManager());
        
        setCallbackRegistry(DefaultCallbackRegistry
        		.createCallbackRegistry(ctx, getExchangeFactory()));
        setCorrelationMap(new DefaultCorrelationMap());
        getAlerter().info(
        		I18n.loc("CRL-5014: Initializing component \"{0}\"", 
        				 ctx.getComponentName()), 
        		ctx.getComponentName(), null, null, ComponentType.valueOf(ctx), 
        		OperationalState.STARTING, "CRL-5014");
    }

    /** @see javax.jbi.component.ComponentLifeCycle#shutDown() */
    public void shutDown() throws JBIException {
        try {
            // Must close DeliveryChannel before stopping accept threads
            // because they may be pending on accept().
            // Closing channel will wake it up from accept()
            DeliveryChannel channel = getComponentContext().getDeliveryChannel();
            if(channel != null) {
                channel.close();
            }
        } 
        catch (Exception e) {
            String compName = getComponentContext().getComponentName();
            String msg = I18n.loc("CRL-6053: {0} failed to close DeliveryChannel: {0}", 
                                  compName, e.getMessage()); 
            log().log(Level.WARNING, msg, e);
            getAlerter().critical(msg, getComponentContext().getComponentName(), 
                                  null, null, ComponentType.ServiceEngine, 
                                  OperationalState.RUNNING, "CRL-6053");
            throw new JBIException(msg, e);
        }

        // shut down polling threads
        getAcceptManager().shutDown();
        // call cleanup on the CustomClassLoaderUtil.
        mCustomCLUtil.cleanup();
        mCustomCLUtil = null;

        getAlerter().info(
        		I18n.loc("CRL-5017: Shutting down component \"{0}\"", 
        				 getComponentContext().getComponentName()), 
        		getComponentContext().getComponentName(), null, null, 
        		ComponentType.valueOf(getComponentContext()), 
        		OperationalState.STOPPED, "CRL-5017");
    }

    /** @see javax.jbi.component.ComponentLifeCycle#start() */
    public void start() throws JBIException {
        getAcceptManager().startAccepting();
        getAlerter().info(
        		I18n.loc("CRL-5015: Starting component \"{0}\"", 
        				 getComponentContext().getComponentName()), 
        		getComponentContext().getComponentName(), null, null, 
        		ComponentType.valueOf(getComponentContext()), 
        		OperationalState.RUNNING, "CRL-5015");
    }

    /** @see javax.jbi.component.ComponentLifeCycle#stop() */
    public void stop() throws JBIException {
        getAcceptManager().stopAccepting();
        getAlerter().info(
        		I18n.loc("CRL-5016: Stopping component \"{0}\"", 
        				 getComponentContext().getComponentName()), 
        		getComponentContext().getComponentName(), null, null, 
        		ComponentType.valueOf(getComponentContext()), 
        		OperationalState.STOPPING, "CRL-5016");
    }

    /* **********           Protected Accessors/Mutators           ********** */
    
    /** Sets the extension object name. */
    protected void setExtensionMBeanName(ObjectName extensionMBeanName) {
        mExtensionMBeanName = extensionMBeanName;
    }
    /** Sets the component's {@link ServiceUnitManager}. */
    protected void setServiceUnitManager(ServiceUnitManager sumgr) {
        mServiceUnitMgr = sumgr;
    }
    
    protected Alerter getAlerter() {
    	return mAlerter;
    }
    protected ComponentConfig getConfig() {
        return mConfig;
    }
    protected MBeanHelper getMBeanHelper() {
        return mMBeanHelper;
    }
    protected Object getRuntimeMBean() {
        return mRuntimeMBean;
    }
    protected Logger log() {
    	return mLogger;
    }
}
