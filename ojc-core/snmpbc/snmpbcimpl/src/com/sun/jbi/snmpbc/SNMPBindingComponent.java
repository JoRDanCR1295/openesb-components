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
 * @(#)SNMPBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpbc.mbeans.RuntimeConfigurationImpl;
import com.sun.jbi.snmpbc.mbeans.RuntimeConfigurationIntf;
import javax.management.StandardMBean;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * SNMP BC ComponentLifeCycle implementation and Component implementation
 *
 * @author echou
 */
public class SNMPBindingComponent
        implements ComponentLifeCycle, Component {
    
    private static final Messages mMessages = 
        Messages.getMessages(SNMPBindingComponent.class);
    private static final Logger mLogger = 
            Logger.getLogger(SNMPBindingComponent.class.getName());
    
    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = "SNMP BC";
    
    private ComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;

    private SNMPServiceUnitManager mSUManager;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfigurationImpl mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private boolean mBCStarted = false;
    private Map<String, InboundMessageProcessorListenerEndpoint> inboundMsgExchanges;

    ////////
    //
    //  Component Interface Methods
    //
    ////////

    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        String pmKey = PM.getEndpointKey(serviceEndpoint.getServiceName(), 
                serviceEndpoint.getEndpointName());
        PM pm = mSUManager.getPMs().get(pmKey);
        if (pm == null) {
            return null;
        }
        return pm.getEndpoint().getServiceDescription();
    }

    public ServiceUnitManager getServiceUnitManager() {
        return mSUManager;
    }
    
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint,
                                              MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint,
                                              MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
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

    public ObjectName getExtensionMBeanName() {
        return null;
    }
    
    public void init(ComponentContext jbiContext) throws JBIException {
        mContext = jbiContext;
        Messages.registerContext(mContext);
        
        initMBeans();
        
        // Create message exchange maps
        inboundMsgExchanges = Collections.synchronizedMap(
                new HashMap<String, InboundMessageProcessorListenerEndpoint> ());
        
        mSUManager = new SNMPServiceUnitManager(mContext, 
                mStatusProviderHelper, inboundMsgExchanges);
        
    }
    
    public void shutDown() throws JBIException {
        mLogger.log(Level.INFO, "SNMPBC_R103.SHUTDOWN_CALLED");
        
        mContext.getDeliveryChannel().close();
        
        shutdownMBeans();
        
        mLogger.log(Level.INFO, "SNMPBC_R104.SHUTDOWN_DONE");
    }
    
    public void start() throws JBIException {
        
        try {
            startOutbound();
            mBCStarted = true;
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_OUTBOUND_RECVR_START_FAILED"),
//                         ex);
            
            throw new JBIException(mMessages.getString("SNMPBC_E105.OUTBOUND_RECVR_START_FAILED"),
                                   ex);
        }
        mLogger.log(Level.INFO, "SNMPBC_R106.START_DONE");
    }
    
    public void stop() throws JBIException {
        try {
            stopOutbound();
            mBCStarted = false;
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_OUTBOUND_RECVR_STOP_FAILED"),
//                         ex);

            throw new JBIException(mMessages.getString("SNMPBC_E107.OUTBOUND_RECVR_STOP_FAILED"),
                                   ex);
        }
        mLogger.log(Level.INFO, mMessages.getString("SNMPBC_R108.STOP_DONE"));
    }


    ////////
    //
    //  SNMPBindingComponent Public Methods
    //
    ////////

    public boolean isStarted() {
        return mBCStarted;
    }
    
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
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
     * @exception    JBIException if any error occurs
     */      
    private void initMBeans() throws JBIException {
        try {
            mStatusProviderHelper =
                new StatusProviderHelper(SHORT_DISPLAY_NAME,
                                         StatusProviderMBean.COMPONENT_TYPE_BINDING,
                                         mContext.getComponentName(),
                                         mContext.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO,
                            mMessages.getString("SNMPBC_R109.STATUS_MBEAN_REG_SUCCEEDED"),
                            new Object[]{mContext.getComponentName()});
            }
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_STATUS_MBEAN_REG_FAILED"),
//                         new Object[]{ex});

            throw new JBIException(mMessages.getString("SNMPBC_E110.STATUS_MBEAN_REG_FAILED", mContext.getComponentName()),
                                   ex);
        }
        
        try {
            String configData = "";
            String configSchema = "";
            String configDataFileLoc = mContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            RandomAccessFile configDataFileAccess = new RandomAccessFile(configDataFile, "r");
            if (configDataFile.exists()) {
                byte[] contents = new byte[(int)configDataFileAccess.length()];
                configDataFileAccess.read(contents);
                // Use default encoding!
                configData = new String(contents);
            }
            configDataFileAccess.close();

            String configSchemaFileLoc = mContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            RandomAccessFile configSchemaFileAccess = new RandomAccessFile(configSchemaFile, "r");
            if (configSchemaFile.exists()) {
                byte[] contents = new byte[(int)configSchemaFileAccess.length()];
                configSchemaFileAccess.read(contents);
                configSchema = new String(contents);
            }
            configSchemaFileAccess.close();

            mRuntimeConfig = new RuntimeConfigurationImpl(mContext.getWorkspaceRoot(), configSchema, configData);
            mRuntimeConfigHelper =
                new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    mContext.getComponentName(),
                    mContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(
                    new StandardMBean(mRuntimeConfig, RuntimeConfigurationIntf.class));
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO,
                            mMessages.getString("SNMPBC_R111.CONFIG_MBEAN_REG_SUCCEEDED"),
                            new Object[]{mContext.getComponentName()});
            }
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_CONFIG_MBEAN_REG_FAILED"),
//                         new Object[]{ex});

            throw new JBIException(mMessages.getString("SNMPBC.E112.CONFIG_MBEAN_REG_FAILED", mContext.getComponentName()),
                                   ex);
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
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_STATUS_MBEAN_UNREG_FAILED"),
//                         new Object[]{mContext.getComponentName(),
//                                      ex});

            throw new JBIException(mMessages.getString("SNMPBC_E113.STATUS_MBEAN_UNREG_FAILED", mContext.getComponentName()),
                                   ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
//             mLogger.log(Level.SEVERE,
//                         mMessages.getString("SNMPBindingComponent_CONFIG_MBEAN_UNREG_FAILED"),
//                         new Object[]{mContext.getComponentName(),
//                                      ex});

            throw new JBIException(mMessages.getString("SNMPBC_E114.STATUS_MBEAN_UNREG_FAILED", mContext.getComponentName()),
                                   ex);            
        }

    }

    private void startOutbound() throws JBIException {
        mOutboundReceiver =
            new OutboundReceiver(mContext,
                                 mSUManager,
                                 mRuntimeConfig, 
                                 inboundMsgExchanges);
        mOutboundReceiver.startReceiving();
        mLogger.log(Level.INFO, mMessages.getString("SNMPBC_R115.OUTBOUND_STARTED"));
    }
    
    private void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        mLogger.log(Level.INFO, mMessages.getString("SNMPBC_R116.OUTBOUND_STOPPED"));
    }
    
}
