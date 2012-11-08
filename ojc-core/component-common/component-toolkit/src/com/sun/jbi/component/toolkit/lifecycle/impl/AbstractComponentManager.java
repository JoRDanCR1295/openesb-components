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

package com.sun.jbi.component.toolkit.lifecycle.impl;

import java.util.HashMap;
import java.util.Map;
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
import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.common.qos.config.RuntimeConfigurationMBean;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.util.MBeanHelper;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ComponentManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.I18n;
import com.sun.jbi.component.toolkit.util.alerter.Alerter;
import com.sun.jbi.component.toolkit.util.alerter.AlerterFactory;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;

/**
 * Abstract implementation of {@link ComponentManager}.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractComponentManager implements ComponentManager {
    // jbi/lifecycle
    private ComponentContext mComponentCtx = null;
    private ServiceUnitManager mServiceUnitMgr = null;
    private ObjectName mExtensionMBeanName = null;

    // toolkit lifecycle
    private MutableContext mManagerCtx;
    private ThreadedExchangeHandler mThreadedHandler;
    
    // utilities
    private ComponentConfig mConfig;
    private Object mRuntimeMBean;
    private Map<String, Object> mMBeanMap;
    private MBeanHelper mMBeanHelper;
    private Logger mLogger = Logger.getLogger(this.getClass().getName());
    private Alerter mAlerter;

    /* **********                 Template Methods                 ********** */
    
    /**
     * Creates, initializes, and returns the {@link ExchangeHandler} 
     * for this component.
     * <p>
     * NOTE: The component's {@link ManagerContext} is not guaranteed to be
     * available when this method is called.  However, both the 
     * <code>ComponentContext</code> and {@link ComponentConfig} will be.
     * 
     * @see ExchangeHandler
     * @see AbstractExchangeHandler
     */
    protected abstract ExchangeHandler initExchangeHandler();

    /**
     * Creates, initializes, and returns the {@link EndpointManager} 
     * for this component.
     * <p>
     * NOTE: The component's {@link ManagerContext} is not guaranteed to be
     * available when this method is called.  However, both the 
     * <code>ComponentContext</code> and {@link ComponentConfig} will be.
     * 
     * @throws JBIException if an error occurs initializing <code>EndpointManager</code>.
     * @see EndpointManager
     * @see AbstractEndpointManager
     */
    protected abstract EndpointManager initEndpointManager() throws JBIException;
    
    protected MutableContext initManagerContext(ComponentContext ctx) throws JBIException {
        return new DefaultManagerContext(
                ctx, new CustomClassLoaderUtil(getClass().getClassLoader()));
    }
    
    /**
     * Initializes the component's runtime configuration MBean during
     * {@link ComponentLifeCycle#init(ComponentContext)} but prior
     * to {@link #initExchangeHandler()}.
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
     * {@link DefaultServiceUnitManager#DefaultServiceUnitManager(ManagerContext)}
     * using this component's <code>ManagerContext</code>.
     * 
     * @see ServiceUnitManager
     * @see DefaultServiceUnitManager
     * @see #getManagerContext()
     */
    protected ServiceUnitManager initServiceUnitManager() {
        return new DefaultServiceUnitManager(getManagerContext());
    }
    
    /* **********                 ComponentManager                 ********** */
    
    protected ComponentContext getComponentContext() {
        return mComponentCtx;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ComponentManager#getManagerContext() */
    public ManagerContext getManagerContext() {
        return mManagerCtx;
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
     *      <li>Sets the {@link ComponentContext}.</li>
     *      <li>Creates a logger and alerter for this component.</li>
     *      <li>Loads and sets this component's {@link ComponentConfig}.</li>
     *      <li>Initializes and sets this component's {@link ServiceUnitManager}.</li>
     *      <li>Initializes and registers this component's runtime MBean.</li>
     *      <li>Initializes and sets this component's {@link EndpointManager}.</li>
     *      <li>Initializes and sets this component's {@link ExchangeHandler}.</li>
     *      <li>Initializes and sets this component's {@link ManagerContext}.</li>
     *      <li>Sends an alert of component's initialization.</li>
     * </ol>
     * 
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     * @see #initEndpointManager()
     * @see #initServiceUnitManager()
     * @see #initExchangeHandler()
     * @see #initRuntimeMBean() 
     */
    public void init(ComponentContext ctx) throws JBIException {
        try {
            // minimum basic initialization for Component Toolkit based components
            mComponentCtx = ctx;
            mLogger = Util.getLogger(ctx, this.getClass().getName());
            mAlerter = AlerterFactory.newAlerter();
            mManagerCtx = initManagerContext(ctx);

            // parse component descriptor to make configuration available
            mConfig = ComponentConfig.parse(ctx.getInstallRoot());
            // load persisted configuration, won't fail if no config.properties
            ConfigPersistence.loadConfig(mConfig, ctx.getWorkspaceRoot());
            // load Application configuration and variables, won't fail if no config-app-persistence.xml
            ConfigPersistence.loadApplicationConfig(mConfig, ctx.getWorkspaceRoot());
            mManagerCtx.setConfiguration(mConfig);
            
            // init MBeanHelper to make available to SUMgrs who want it...
            mMBeanHelper = new MBeanHelper(ctx);
            mMBeanMap = new HashMap<String, Object>();
            
            // register configuration MBean prior to AcceptManager initialization
            mRuntimeMBean = initRuntimeMBean();
            if (getRuntimeMBean() != null) {
                try {
                    getMBeanHelper().registerMBean(
                            RuntimeConfigurationMBean.CONFIGURATION_EXTENSION, 
                            getRuntimeMBean());
                }
                catch (JBIException jbi) {
                    mRuntimeMBean = null;   // aborts attempt to unregister
                    throw jbi;
                }
            }
            // register any additional MBeans created during initRuntimeMBean()
            if (!getCustomMBeans().isEmpty()) {
                for (String name : getCustomMBeans().keySet()) {
                    Object mbean = getCustomMBeans().get(name);
                    if (mbean != null) {
                        getMBeanHelper().registerMBean(name, mbean);
                    }
                }
            }
            // initialize manager context and service unit manager
            ExchangeHandler handler = initExchangeHandler();
            mManagerCtx.setExchangeHandler(handler);
            EndpointManager emgr = initEndpointManager();
            mManagerCtx.setEndpointManager(emgr);
            setServiceUnitManager(initServiceUnitManager());
            // determine threading, if any... calls init(Ctx) on EH
            installThreadedHandler();
            
            // send alert
            String info = I18n.loc("COMPTK-5001: Initialized {0} successfully!", 
                                   ctx.getComponentName());
            log().info(info);
            getAlerter().info(info, ctx.getComponentName(), 
                              null, null, ComponentType.ServiceEngine, 
                              OperationalState.STARTING, "COMPTK-5001");
        }
        catch (Exception ex) {
            String err = I18n.loc(
                    "COMPTK-7002: {0} initialization failed: {1}", 
                    ctx.getComponentName(), ex.getMessage());
            log().log(Level.SEVERE, err, ex);
            throw new JBIException(err, ex);
        }
    }

    protected ExchangeHandler getThreadedHandler() {
        return (mThreadedHandler == null) 
                ? getManagerContext().getExchangeHandler() : mThreadedHandler;
    }
    
    protected void installThreadedHandler() throws JBIException {
        Property prop = getConfig().getProperty(
                ThreadedExchangeHandler.EXCHANGE_THREADING_PROPERTY);
        ExchangeHandler handler = getManagerContext().getExchangeHandler();
        if (handler != null) {
            if (getConfig().propertySet().contains(prop)) {
                handler = mThreadedHandler = new ThreadedExchangeHandler(handler);
                // make available for AcceptPollers
                getManagerContext().getCorrelationMap()
                        .put(ThreadedExchangeHandler.class.getName(), handler);
            }
            // threadedEH will initialize the decorated EH
            handler.init(getComponentContext());
        }
    }
    
    protected void preShutDown() throws JBIException {
        // no op
    }
    protected void postShutDown() throws JBIException {
        // no op
    }
    
    /** @see javax.jbi.component.ComponentLifeCycle#shutDown() */
    public void shutDown() throws JBIException {
        // hook for cleanup prior to channel close
        preShutDown();

        JBIException jbi = null;
        // shut down polling threads to stop accepting before DC is closed
        try {
            getThreadedHandler().shutDown();
        }
        catch (Exception e) {
            String err = I18n.loc(
                    "COMPTK-7005: Failed to shutdown {0}: {1}", 
                    getComponentContext().getComponentName(), e.getMessage());
            log().log(Level.SEVERE, err, e);
            jbi = new JBIException(err, e);
        }

        try {
            
            // Must close DeliveryChannel before stopping accept threads
            // because they may be pending on accept().
            // Closing channel will wake it up from accept()
            DeliveryChannel channel = getComponentContext().getDeliveryChannel();
            if(channel != null) {
                // needs closing
                MessagingChannel mc = getManagerContext().getMessagingChannel();
                if (mc != null) {   // just in case...
                    mc.close(); // closes decorated DC after closing filters
                }
                else {
                    channel.close();
                }
            }
        } 
        catch (Exception e) {
            String compName = getComponentContext().getComponentName();
            String msg = I18n.loc("COMPTK-6012: {0} failed to close DeliveryChannel: {0}", 
                                  compName, e.getMessage()); 
            log().log(Level.WARNING, msg, e);
            getAlerter().critical(msg, compName, 
                                  null, null, ComponentType.ServiceEngine, 
                                  OperationalState.UNKNOWN, "COMPTK-6012");
            throw new JBIException(msg, e);
        }

        // unregister config MBean
        if (getRuntimeMBean() != null) {
            getMBeanHelper().unregisterMBean(
                    RuntimeConfigurationMBean.CONFIGURATION_EXTENSION);
        }
        // register any additional MBeans created during initRuntimeMBean()
        if (!getCustomMBeans().isEmpty()) {
            for (String name : getCustomMBeans().keySet()) {
                if (getCustomMBeans().get(name) != null) {
                    getMBeanHelper().unregisterMBean(name);
                }
            }
            getCustomMBeans().clear();  // clear cached mbeans
        }

        // hook for cleanup after channel close
        postShutDown();
        
        // unload cached service unit classloaders
        if (getManagerContext() != null) {
            getManagerContext().getCustomClassLoaderUtil().cleanup();
        }
        
        if (jbi != null) {
            throw jbi;
        }
        
        String info = I18n.loc("COMPTK-5004: Shut down {0} successfully!", 
                               getComponentContext().getComponentName());
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), null, null, 
                          ComponentType.valueOf(getComponentContext()), 
                          OperationalState.SHUTDOWN, "COMPTK-5004");
    }

    /** @see javax.jbi.component.ComponentLifeCycle#start() */
    public void start() throws JBIException {
        // load persisted configuration when component starts
        ConfigPersistence.loadConfig(getConfig(), getComponentContext().getWorkspaceRoot());
        ConfigPersistence.loadApplicationConfig(getConfig(), getComponentContext().getWorkspaceRoot());

        // start NMR polling threads
        try {
            getThreadedHandler().start();
        }
        catch (Exception ex) {
            String err = I18n.loc(
                    "COMPTK-7003: Failed to start {0}: {1}", 
                    getComponentContext().getComponentName(), ex.getMessage());
            log().log(Level.SEVERE, err, ex);
            throw new JBIException(err, ex);
        }
        
        String info = I18n.loc("COMPTK-5002: Started {0} successfully!", 
                               getComponentContext().getComponentName());
        log().info(info);
        getAlerter().info(info, 
                          getComponentContext().getComponentName(), null, null, 
                          ComponentType.valueOf(getComponentContext()), 
                          OperationalState.STARTED, "COMPTK-5002");
    }

    /** @see javax.jbi.component.ComponentLifeCycle#stop() */
    public void stop() throws JBIException {
        // stop NMR polling threads
        try {
            getThreadedHandler().stop();
        }
        catch (Exception ex) {
            String err = I18n.loc(
                    "COMPTK-7004: Failed to stop {0}: {1}", 
                    getComponentContext().getComponentName(), ex.getMessage());
            log().log(Level.SEVERE, err, ex);
            throw new JBIException(err, ex);
        }

        String info = I18n.loc("COMPTK-5003: Stopped {0} successfully!", 
                               getComponentContext().getComponentName());
        log().info(info);
        getAlerter().info(info, 
                          getComponentContext().getComponentName(), null, null, 
                          ComponentType.valueOf(getComponentContext()), 
                          OperationalState.STOPPED, "COMPTK-5003");
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
    protected Map<String, Object> getCustomMBeans() {
        return mMBeanMap;
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
