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
 * @(#)SAPBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.util.ReadWriteTextFile;
import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;

/**
 * Control/Query implementation for the SAP Binding Component.
 * Executed when Binding Component is started
 */
public class SAPBindingLifeCycle implements ComponentLifeCycle, Component {
    public SAPBindingLifeCycle() {
    }
    
    /**************************************************************************
     * Component Interface
     **************************************************************************/
    
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        Document metadata = null;
        if (mDeployer != null) {
            for (Iterator serviceUnits = mDeployer.getServiceUnits().values().iterator()
                    ; serviceUnits.hasNext()
                    ; ) {
                for (Iterator endpoints = ((ServiceUnit)serviceUnits.next()).getEndpoints().iterator()
                        ; endpoints.hasNext()
                        ; ) {
                    Endpoint endpoint = (Endpoint)endpoints.next();
                    if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName()) &&
                        serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())) {
                        return endpoint.getServiceDescription();
                    }
                }
            }
        }
        return metadata;
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
        // No dynamic endpoint(s) is supported for now
        return null;
    }
    
    /**************************************************************************
     * ComponentLifeCycle Interface
     **************************************************************************/
    
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }
    
    public void init(ComponentContext jbiContext) throws JBIException {
        if (jbiContext ==  null) {
            throw new JBIException(mMessages.getString("SAPBindingLifeCycle.Null_context"));
        }
        mContext = jbiContext;
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(SAPBindingLifeCycle.class);
        mDeployer = new SAPBindingDeployer(mContext, this);
        
        /**
         * Initializes the MBeans associated with this Binding Component. 
         * Currently, two MBeans are created for use:
         * 1. StatusProviderMBean - Provides status about this Binding Component
         * 2. RuntimeConfigurationMBean - Provides configurability for this Binding
         *    component
         */      
        MBeanServer mbServer = jbiContext.getMBeanServer();
        String componentName = jbiContext.getComponentName();
        
        // Create and register StatusProviderMBean
        try {
            mStatusProviderHelper =
                    new StatusProviderHelper(
                            SHORT_DISPLAY_NAME, 
                            StatusProviderMBean.COMPONENT_TYPE_BINDING, 
                            componentName, 
                            mbServer);
            
            mStatusProviderHelper.registerMBean();
            
            Utils.checkLog(mLogger, Level.INFO, "SAPBindingLifeCycle.Registered_mbean", componentName);
        } catch (Exception ex) {
            mLogger.log(
                    Level.WARNING,
                    "SAPBindingLifeCycle.Failed_register_mbean",
                    ex);
            
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_register_mbean"),
                    ex);
        }
        
        // Create and register RuntimeConfigurationMBean
        try {
            String configData = "";
            String configSchema = "";
            String configDataFileLoc = mContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            if (configDataFile.exists()) {
                configData = ReadWriteTextFile.getContents(configDataFile);
            }
            String configSchemaFileLoc = mContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            if (configSchemaFile.exists()) {
                configSchema = ReadWriteTextFile.getContents(configSchemaFile);
            }
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), configSchema, configData);
            mRuntimeConfigHelper =
                    new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    componentName,
                    mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "SAPBindingLifeCycle.Register_config_mbean", componentName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("SAPBindingLifeCycle.Failed_register_config_mbean"), ex);
            throw new JBIException(mMessages.getString("SAPBindingLifeCycle.Failed_register_config_mbean"), ex);
        }
        
        try {
            mChannel = mContext.getDeliveryChannel();
        } catch(MessagingException me) {
            mLogger.log(
                    Level.SEVERE,
                    "SAPBindingLifeCycle.No_Delivery_Channel",
                    me.getMessage());
            throw me;
        }
        
        try {
            startOutbound();
        } catch (Exception ex) {
            mLogger.log(
                    Level.SEVERE,
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_start_outbound",
                            ex.getMessage()),
                    ex);
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_start_outbound",
                            ex.getMessage()),
                    ex);
        }
        
        try {
            startInbound();
        } catch (Exception ex) {
            mLogger.log(
                    Level.SEVERE,
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_start_inbound",
                            ex.getMessage()),
                    ex);
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_start_inbound",
                            ex.getMessage()),
                    ex);
        }
        
        isInitialized = true;
    }
    
    public void shutDown() throws JBIException {
        mLogger.info("SAPBindingLifeCycle.Calling_shutdown");
        
        if (!mDeployer.getServiceUnits().isEmpty()) {
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_shutdown_su_deployed"));
        }
        
        // Shut down the MBeans associated with this Binding Component.
        try {
            if (mStatusProviderHelper != null) {
                mStatusProviderHelper.unregisterMBean();
                mStatusProviderHelper = null;
            }
        } catch (Exception ex) {
            mLogger.log(
                    Level.WARNING,
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_unregister_mbean",
                            mContext.getComponentName()),
                    ex);
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_unregister_mbean",
                            mContext.getComponentName()),
                    ex);
        }
        
        try {
            if (mRuntimeConfigHelper != null) {
                mRuntimeConfigHelper.unregisterMBean();
                mRuntimeConfigHelper = null;
                mRuntimeConfig = null;
            }
        } catch (Exception ex) {
            mLogger.log(
                    Level.WARNING,
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_unregister_config_mbean",
                            mContext.getComponentName()),
                    ex);
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_unregister_config_mbean",
                            mContext.getComponentName()),
                    ex);
        }
        
        try {
            stopOutbound();
        } catch (Exception ex) {
            mLogger.log(
                    Level.WARNING,
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_stop_outbound", ex.getMessage()),
                    ex);
            throw new JBIException(
                    mMessages.getString(
                            "SAPBindingLifeCycle.Failed_stop_outbound", ex.getMessage()),
                    ex);
        }
        
        if (mChannel != null) {
            mChannel.close();
            mChannel = null;
        }
        
        mContext = null;
        mDeployer = null;
        
        isStarted = false;
        isInitialized = false;
        isShutdown = true;
        
        mLogger.info("SAPBindingLifeCycle.Shutdown");
        Messages.unregisterContext();
    }
    
    public void start() throws JBIException {
        if (!isInitialized) {
            if (isShutdown) {
                throw new JBIException(
                        mMessages.getString(
                                "SAPBindingLifeCycle.Failed_start_not_initialized"));
            }
        }
        isStarted = true;
        isShutdown = false;
        mLogger.info("SAPBindingLifeCycle.Started");
    }
    
    public void stop() throws JBIException {
        isStarted = false;
        mLogger.info("SAPBindingLifeCycle.Stopped");
    }
    
    
    /**************************************************************************
     * SAP Binding Component-specific methods
     **************************************************************************/
    
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }
    
    protected void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(
                mContext, mChannel, mRuntimeConfig);
        mLogger.info("SAPBindingLifeCycle.Started_inbound");
    }
    
    protected void startOutbound() throws JBIException {
        //Passes in whatever service units are currently deployed
        mOutboundReceiver = new OutboundReceiver(
                mChannel, mDeployer.getServiceUnits(), mRuntimeConfig); 
        mLogger.info("SAPBindingLifeCycle.Started_outbound");
    }
   
    protected void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
            mOutboundReceiver = null;
        }
        mLogger.info("SAPBindingLifeCycle.Stopped_outbound");
    }
    
    public InboundReceiver getInboundReceiver() {
        return mInboundReceiver;
    }
    
    public OutboundReceiver getOutboundReceiver() {
        return mOutboundReceiver;
    }
    
    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return mRuntimeConfig;
    }
    /** 
      * Package protected method.
      * Used solely for JUnit test purposes
      */
    void setServiceUnitManager(SAPBindingDeployer deployer) {
        mDeployer = deployer;
    }
    
    private static final Messages mMessages =
            Messages.getMessages(SAPBindingLifeCycle.class);
    
    private static Logger mLogger;
    
    public static final String SHORT_DISPLAY_NAME =
            mMessages.getString("SAPBindingLifeCycle.Short_name");
    
    private ComponentContext mContext;
    private Map mEndpointMapping = new HashMap();
    private static SAPBindingLifeCycle mInstance;
    private ObjectName mExtensionMBeanName;
    private DeliveryChannel mChannel;
    private ObjectName mDeployerMBeanName;
    private SAPBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private InboundReceiver mInboundReceiver;
    private OutboundReceiver mOutboundReceiver;
    private boolean isStarted;
    private boolean isShutdown;
    private boolean isInitialized;
}
