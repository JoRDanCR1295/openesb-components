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
 * @(#)SwiftBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.Component;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.naming.InitialContext;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.swiftbc.RuntimeConfiguration;
import com.sun.jbi.internationalization.Messages;

/**
 * This class implements the ComponentLifeCycle and Component interfaces. The implementation of the
 * Component interface allows the JBI framework to query the Swift BC component for various types of
 * information. The implementation of the ComponentLifeCycle interface provides initialization,
 * start, stop, and shutdown life cycle processing.
 * 
 * @author S. Nageswara Rao
 */

public class SwiftBindingComponent implements ComponentLifeCycle, Component {

    private static final Messages mMessages = Messages.getMessages(SwiftBindingComponent.class);

    /**
     * The logger used to log issues with this class
     */
    private static Logger mLogger = null;

    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = mMessages.getString("Swift_BC");

    private ComponentContext mContext;

    private OutboundReceiver mOutboundReceiver;

    private InboundReceiver mInboundReceiver;

    // private MessageStore mMessageStore;

    private ObjectName mExtensionMBeanName;

    private DeliveryChannel mChannel;

    private SwiftBindingDeployer mDeployer;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfiguration mRuntimeConfig;

    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    /**
     * Default constructor
     */
    public SwiftBindingComponent() {
    }

    /***********************************************************************************************
     * Component Interface Methods
     **********************************************************************************************/

    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        Document result = null;
        Iterator serviceUnits = mDeployer.getServiceUnits().iterator();
        while (serviceUnits.hasNext()) {
            Iterator endpoints = ((ServiceUnit) serviceUnits.next()).getEndpoints().iterator();
            while (endpoints.hasNext()) {
                Endpoint endpoint = (Endpoint) endpoints.next();
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName())
                        && serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())) {

                    result = endpoint.getServiceDescription();
                    return result;
                }
            }
        }
        return result;
    }

    public ServiceUnitManager getServiceUnitManager() {
        return mDeployer;
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return false;
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment fragment) {
        // Currently we do not support dynamic endpoints
        return null;
    }

    /***********************************************************************************************
     * ComponentLifeCycle Interface Methods
     **********************************************************************************************/

    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    public void init(ComponentContext jbiContext) throws JBIException {
        if (jbiContext == null) {
            throw new JBIException(mMessages.getString("swiftbc_Null_context"));
        }
        mContext = jbiContext;
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(SwiftBindingComponent.class);
        mDeployer = new SwiftBindingDeployer(mContext, this);
        // mMessageStore = new MessageStore();

        initMBeans();
        /*
        // create Database schema
        // get the properties
        Properties prop = mRuntimeConfig.getProperties();
        // get the Initial Context
        InitialContext context = mContext.getNamingContext();
        DBConnectionFactory fac = new DBConnectionFactory(prop, context);
        try {
            DBSchemaCreation.getInstance().create(prop, fac);
        } catch (SwiftRuntimeException Swiftre) {
            mLogger.log(Level.SEVERE, mMessages.getString("swiftbc_Failed_create_schema", Swiftre.getMessage()), Swiftre);
            throw new JBIException(mMessages.getString("swiftbc_Failed_create_schema", Swiftre.getMessage()), Swiftre);
        }
*/
        try {
            mChannel = mContext.getDeliveryChannel();
        } catch (MessagingException me) {
            mLogger.log(Level.SEVERE, "swiftbc_No_Dev_Channel", me.getMessage());
            throw me;
        }

        try {
            startOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("swiftbc_Failed_start_outbound", ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_start_outbound", ex.getMessage()), ex);
        }

        try {
            startInbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("swiftbc_Failed_start_inbound", ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_start_inbound", ex.getMessage()), ex);
        }
    }

    public void shutDown() throws JBIException {
        mLogger.info("Shutdown_swiftbc");

        try {
            stopOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("swiftbc_Failed_stop_outbound", ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_stop_outbound", ex.getMessage()), ex);
        }

        try {
            stopInbound();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("swiftbc_Failed_stop_inbound", ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_stop_inbound", ex.getMessage()), ex);
        }

        if (mChannel != null) {
            mChannel.close();
        }

        shutdownMBeans();

        mLogger.info("Shutdown_swiftbc_completed");
    }

    public void start() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.info("swiftbc_started");
        }
    }

    public void stop() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.info("swiftbc_stopped");
        }
    }

    /***********************************************************************************************
     * Swift Binding Component specific methods
     **********************************************************************************************/
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }

    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return (RuntimeConfigurationMBean)mRuntimeConfig;
    }

    /**
     * Initializes the MBeans associated with this Binding Component. Currently, two MBeans are
     * created for use with this Bindng Component:
     * <ul>
     * <li>StatusProviderMBean - Provides status about this Binding Component</li>
     * <li>RuntimeConfigurationMBean - Provides configurability for this Binding component</li>
     * </ul>
     * 
     * @exception JBIException if any error occurs
     */
    public void initMBeans() throws JBIException {

        MBeanServer mbServer = mContext.getMBeanServer();
        String componentName = mContext.getComponentName();

        try {
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_BINDING, componentName, mbServer);
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "swiftbc_Register_mbean", componentName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "swiftbc_Failed_register_mbean", ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_register_mbean"), ex);
        }

        try {
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot());
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    componentName, mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "swiftbc_Register_config_mbean", componentName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("swiftbc_Failed_register_config_mbean"), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_register_config_mbean"), ex);
        }
    }

    /**
     * Shuts down the MBeans associated with this Binding Component.
     * 
     * @exception JBIException if any error occurs
     */
    public void shutdownMBeans() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING,
                    mMessages.getString("swiftbc_Failed_unregister_mbean", mContext.getComponentName()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_unregister_mbean", mContext.getComponentName()),
                    ex);
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("swiftbc_Failed_unregister_config_mbean",
                    mContext.getComponentName()), ex);
            throw new JBIException(mMessages.getString("swiftbc_Failed_unregister_config_mbean",
                    mContext.getComponentName()), ex);
        }

    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR). In the case of
     * Swift, this thread will handle all messages from the NMR and delegate them to worker threads.
     * These threads will be responsible for binding the normalized messages to the Swift-specific
     * protocol and handling the message delivery.
     * 
     * @exception JBIException if any error occurs
     */
    public void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mContext, mChannel, mDeployer.getServiceUnits(), mRuntimeConfig);
        Thread mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();
        mLogger.info("swiftbc_started_outbound");
    }

    /**
     * Shuts down the thread that handles messages from the NMR
     * 
     * @exception JBIException if any error occurs
     */
    public void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        mLogger.info("swiftbc_stopped_outbound");
    }

    /**
     * @exception JBIException if any error occurs
     */
    public void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext, mChannel, mRuntimeConfig);
        mLogger.info("swiftbc_started_inbound");
    }

    /**
     * @exception JBIException if any error occurs
     */
    public void stopInbound() throws JBIException {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }
        mLogger.info("swiftbc_stopped_inbound");
    }

    public InboundReceiver getInboundReceiver() {
        return mInboundReceiver;
    }

    public OutboundReceiver getOutboundReceiver() {
        return mOutboundReceiver;
    }

    /**
     * Package protected method. Used solely for JUnit test purposes
     */
    void setServiceUnitManager(SwiftBindingDeployer deployer) {
        mDeployer = deployer;
    }
}
