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
 * @(#)MQBindingComponent.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.mqbc.management.Management;
import com.sun.jbi.mqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.mqbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.monitoring.PerformanceMeasurement;
import com.sun.jbi.mqbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.mqbc.recovery.MQConnectionInfoFilePersister;
import com.sun.jbi.mqbc.recovery.MQXARecovery;
import com.sun.jbi.mqbc.recovery.MQXARecoveryHelper;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;


/**
 * MQBindingComponent defines a MQ JBI Component and ComponentLifeCycle.
 * This class implements the main interfaces with the JBI framework.  In
 * addition to implementing the Component and ComponentLifeCycle interfaces,
 * this class creates MBeans used for monitoring and management, and it creates
 * an initial thread to handle messages from the Normalized Message Router (NMR).
 *
 *
 */
public class MQBindingComponent implements ComponentLifeCycle, Component {

    public static final String COMPONENT_NAME = "sun-webspheremq-binding";
    public static final String COMPONENT_TYPE = "BindingComponent";
    
    private volatile EventLogger mLogger =
            new EventLogger(Logger.getLogger(getClass().getName()),
                    EventManagementFrameworkAlerter.alerter);

    private MQComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;
    private InboundReceiver mInboundReceiver;
    
    private ObjectName mExtensionMBeanName;
    private MessagingChannel mChannel;
    private MQBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    
    private MQXARecovery mRecovery;
    private MQXARecoveryHelper mRecoveryHelper;
    private ConnectionInfoPersister mConnPersister;
    private KeyStoreUtilClient mKeySupport;
    private Management mAdminMBean;

    public MQBindingComponent() {
    }

    ////////
    //
    //  Component Interface Methods
    //
    ////////
    
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        // TODO: The document returned should be a stand-alone document
        //       (no imports or includes)
        // TODO: consider whether it should only return the abstract wsdl
        //       concerning the endpoint
        // TODO: Beware for service engines that they HAVE TO include a specific
        //       binding type defined for Service Engines
        Document result = null;
        
        FIND_SERVICEDESC:
        for (ServiceUnit serviceUnit : mDeployer.getServiceUnits()) {
            for (Endpoint endpoint : serviceUnit.getEndpoints()) {
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName())
                        && serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())) {
                    result = endpoint.getServiceDescription();
                    break FIND_SERVICEDESC;
                }
            }
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            if (result != null) {
                mLogger.fine(I18n.msg(
                        "Service description for endpoint {0} requested and found.",
                        serviceEndpoint.getServiceName().toString().concat(
                                serviceEndpoint.getEndpointName())));
            }
        }

        return result;
    }
    
    public synchronized ServiceUnitManager getServiceUnitManager() {
        return mDeployer;
    }
    
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint,
            MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }
    
    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint,
            MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return false;
    }
    
    public ServiceEndpoint resolveEndpointReference(DocumentFragment fragment) {
        // Currently we do not support dynamic endpoints
        return null;
    }
    
    ////////
    //
    //  ComponentLifeCycle Interface Methods
    //
    ////////
    
    public synchronized ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }
    
    private boolean isInitialized = false;
    public synchronized void init(ComponentContext jbiContext) throws JBIException {
        if (jbiContext ==  null) {
            throw new JBIException(new NullPointerException("ComponentContext"));
        }
        
        mLogger = new EventLogger(Util.getLogger(jbiContext,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);

        mKeySupport = new KeyStoreUtilClient(jbiContext);

        try {
            mChannel = new BaseMessagingChannel(jbiContext);
        } catch(MessagingException me) {
            throw new MessagingException(
                    I18n.msg("1002: Messaging Channel initialization failed"),
                    me);
        }
        
        mContext = new MQComponentContext(jbiContext, mChannel);
        
        // MBeans
        initMBeans(mContext);
        
        // Recovery/persistence setup
        if (MQXARecovery.xaRecoverySupported(jbiContext)) {
            // Needed by registerXAResource
            jbiContext.getDeliveryChannel();
            try {
                Properties persistProps = new Properties();
                persistProps.setProperty(MQConnectionInfoFilePersister.FILE_PERSISTER_PROPS_DIR,
                        jbiContext.getWorkspaceRoot());
                mConnPersister = new MQConnectionInfoFilePersister();
                mConnPersister.initialize(persistProps);
                mRecoveryHelper = new MQXARecoveryHelper(mRuntimeConfig);
                mRecovery = new MQXARecovery(jbiContext, mConnPersister, mRecoveryHelper);
                mRecovery.recover();
            }catch (Throwable t) {
                mLogger.log(Level.WARNING,
                        NotificationEvent.SEVERITY_TYPE_FATAL,
                        NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                        I18n.msg(
                                "1000: Transaction recovery initialization failed."),
                        t);
            }
            
        }else {
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                    I18n.msg("1001: Context does not support XA recovery."
                            + " Initializing for non-transactional operation."));
            mConnPersister = null;
            mRecoveryHelper = null;
            mRecovery = null;
        }
        
        mDeployer = new MQBindingDeployer(mContext, mStatusProviderHelper,this,mConnPersister);
        mAdminMBean.setServiceUnitManager(mDeployer);
        
        isInitialized = true;

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                I18n.msg("1003: Component {0} initialized.",
                        mContext.getComponentName()));
    }
    
    public synchronized void shutDown() throws JBIException {
         if (mConnPersister != null) {
            try {
                mConnPersister.close();
            } catch (Throwable ex) {
                mLogger.log(Level.WARNING,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_SHUTTINGDOWN,
                        I18n.msg("1004: Settings persistence failed to" 
                                + " finalize. Shut down will continue."),
                        ex);
            }
        }
        
        if(mChannel != null) {
            try {
                mChannel.close();
            } catch (MessagingException e) {
                throw new JBIException(
                        I18n.msg("1006: Channel failed to shut down."), e);
            }
        }
        
        shutdownMBeans();
        isInitialized = false;

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_SHUTDOWN,
                I18n.msg("1007: Component {0} shut down.",
                        mContext.getComponentName()));
    }
    
    public synchronized void start() throws JBIException {
        if (!isInitialized) {
            throw new JBIException(new IllegalStateException(I18n.msg(
                    "1008: Cannot start component until it is initialized")));
        }
        
        if (mRecoveryHelper != null) {
            mRecoveryHelper.closeOpenedResources();
        }
        
        try {
            startOutbound();
            startInbound();
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1009: Failed to start message processors."), ex);
        }

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                I18n.msg("1010: Component {0} started.",
                        mContext.getComponentName()));

    }
    
    public synchronized void stop() throws JBIException {
        
        try {
            stopOutbound();
            stopInbound();
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1011: Failed to stop message processors."), ex);
        }

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STOPPED,
                I18n.msg("1012: Component {0} stopped.",
                        mContext.getComponentName()));
    }
    
    /**
     * Initializes the MBeans associated with this Binding Component.  Currently,
     * two MBeans are created for use with this Bindng Component:
     * <ul>
     * <li>StatusProviderMBean - Provides status about this Binding Component</li>
     * <li>RuntimeConfigurationMBean - Provides configurability for this Binding
     * component</li>
     * </ul>
     *
     * @param context ComponentContext
     * @exception    JBIException if any error occurs
     */
    private synchronized void initMBeans(MQComponentContext context) throws JBIException {
        
        MBeanServer mbServer = context.getMBeanServer();
        MBeanNames mbNames = context.getMBeanNames();
        
        // Statistics MBean
        try {
            ObjectName statsMbeanName = mbNames
                    .createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(context.getComponentName(),
                    statsMbeanName,
                    mbServer);
            mStatusProviderHelper.registerMBean();
            PerformanceMeasurement measurement = new PerformanceMeasurement();
            measurement.register(mStatusProviderHelper);
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                    I18n.msg("1013: Status MBean registration registered."));
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1014: Status MBean registration failed"), ex);
        }
        
        // Configuration MBean
        mKeySupport = new KeyStoreUtilClient(context);
                
        try {
            ObjectName configurationMbeanName = mbNames
                    .createCustomComponentMBeanName("Configuration");
            mRuntimeConfig =
                    new RuntimeConfiguration(context
                            .getWorkspaceRoot(),
                            mKeySupport);
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(
                    configurationMbeanName,
                    mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_UNKNOWN,
                    I18n.msg("1015: Configuration MBean registered."));
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1016: Configuration MBean registration failed"),
                    ex);
        }
        
        // Administration MBean
        try {
            ObjectName mbeanName = mbNames.createCustomComponentMBeanName("Administration");
            mAdminMBean = new Management(context);
            registerMBean(mbServer, mbeanName, mAdminMBean);
        } catch (Exception e) {
            throw new JBIException(
                    I18n.msg("1031: Administration MBean registration failed"),
                    e);
        }
    }
    
    private void registerMBean(MBeanServer server, ObjectName beanName, Object bean)
            throws MBeanRegistrationException, InstanceAlreadyExistsException,
                   NotCompliantMBeanException {
        assert server != null;
        assert bean != null;
        assert beanName != null;
        
        if (!server.isRegistered(beanName)) {
            server.registerMBean(bean, beanName);
        }
    }
    
    private void deregisterMBean(MBeanServer server, ObjectName beanName)
            throws InstanceNotFoundException, MBeanRegistrationException {
        assert server != null;
        assert beanName != null;
        
        if (server.isRegistered(beanName)) {
            server.unregisterMBean(beanName);
        }
    }
    
    /**
     * Shuts down the MBeans associated with this Binding Component.
     *
     * @exception    JBIException if any error occurs
     */
    private void shutdownMBeans() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_SHUTTINGDOWN,
                    I18n.msg("1017: Status MBean deregistered."));
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1018: Status MBean deregistration failed."), ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_SHUTTINGDOWN,
                    I18n.msg("1019: Config MBean deregistered."));
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1020: Configuration MBean deregistration failed."), ex);
        }
        
        MBeanServer mbServer = mContext.getMBeanServer();
        ObjectName mbeanName = mContext.getMBeanNames()
                .createCustomComponentMBeanName("Administration");
        try {
            deregisterMBean(mbServer, mbeanName);
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_SHUTTINGDOWN,
                    I18n.msg("1032: Administration MBean deregistered."));
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.msg("1033: Adminstration MBean deregistration failed."), ex);
        }
        
    }
    
    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR).
     * In the case of MQ, this thread will handle all messages from the NMR and
     * delegate them to worker threads.  These threads will be responsible for binding
     * the normalized messages to the MQ-specific protocol and handling the message
     * delivery.
     *
     * @exception    JBIException if any error occurs
     */
    private synchronized void startOutbound() throws JBIException {
        if (mOutboundReceiver == null) {
            mOutboundReceiver = new OutboundReceiver(mContext,
                    mDeployer,
                    mRuntimeConfig);
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTING,
                    I18n.msg("1021: Outbound processing started."));
        } else {
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTING,
                    I18n.msg("1022: Outbound processing already started."));
        }
    }
    
    /**
     * Shuts down the thread that handles messages from the NMR
     */
    private synchronized void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stop();
            // New instance needed if component is restarted;
            // a restart requires an init() call.
            mOutboundReceiver = null;
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STOPPING,
                    I18n.msg("1023: Outbound processing stopped."));
        } else {
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTING,
                    I18n.msg("1024: Outbound processing not started."));
        }
    }

    /**
     *
     *
     * @exception    JBIException if any error occurs
     */
    private synchronized void startInbound() throws JBIException {
        if (mInboundReceiver == null) {
            mInboundReceiver = new InboundReceiver(mContext, mRuntimeConfig);
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTING,
                    I18n.msg("1026: Inbound processing started."));
        } else {
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTING,
                    I18n.msg("1027: Inbound processing already started."));
        }
    }
    
    public synchronized InboundReceiver getInboundReceiver() {
        return mInboundReceiver;
    }
    
    /**
     *
     *
     * @exception    JBIException if any error occurs
     */
    private synchronized void stopInbound() throws JBIException {
        if (mInboundReceiver != null) {
            mInboundReceiver.stop();
            // New instance needed if component is restarted;
            // a restart requires an init() call.
            mInboundReceiver = null;
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STOPPING,
                    I18n.msg("1028: Inbound processing stopped."));
        } else {
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STOPPING,
                    I18n.msg("1029: Inbound processing not started."));
        }
    }
    
    public synchronized RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return mRuntimeConfig;
    }
}
