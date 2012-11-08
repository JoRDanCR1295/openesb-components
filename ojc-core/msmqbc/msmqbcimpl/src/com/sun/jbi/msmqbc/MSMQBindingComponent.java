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
 * @(#)MSMQBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.jbi.management.MBeanNames;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.Component;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;

import com.sun.jbi.msmqbc.msmq.ChannelManager;
import com.sun.jbi.msmqbc.msmq.ChannelManagerImpl;
import com.sun.jbi.msmqbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;

/**
 * MSMQBindingComponent defines a MSMQ JBI Component and ComponentLifeCycle.  This class
 * implements the main interfaces with the JBI framework.  In addition to implementing
 * the Component and ComponentLifeCycle interfaces, this class creates MBeans used for
 * monitoring and management, and it creates an initial thread to handle messages from
 * the Normalized Message Router (NMR).
 *
 * @author Sun Microsystems
 */
public class MSMQBindingComponent implements ComponentLifeCycle, Component {

    private static final Messages mMessages = Messages.getMessages(MSMQBindingComponent.class);

    private static Logger mLogger = null;

    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = "MSMQ BC";

    private ComponentContext mContext;

    private OutboundReceiver mOutboundReceiver;

    private ObjectName mExtensionMBeanName;

    private DeliveryChannel mChannel;

    private MSMQBindingDeployer mDeployer;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfiguration mRuntimeConfig;

    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    private ChannelManager msmqChannelMgr;

    private boolean mBCStarted = false;

    private InboundReceiver mInboundReceiver;

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
        Document theDoc = null;
        for (Iterator serviceUnits = mDeployer.getServiceUnits().iterator(); serviceUnits.hasNext();) {
            for (Iterator endpoints = ((ServiceUnit) serviceUnits.next()).getEndpoints().iterator(); endpoints.hasNext();) {
                Endpoint endpoint = (Endpoint) endpoints.next();
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName())
                        && serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())) {
                    return endpoint.getServiceDescription();
                }
            }
        }
        return theDoc;
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
        return mExtensionMBeanName;
    }

    public void init(ComponentContext jbiContext) throws JBIException {
        mContext = jbiContext;
        Messages.registerContext(mContext);        
        mLogger = Messages.getLogger(MSMQBindingComponent.class);
        initMBeans();

        msmqChannelMgr = new ChannelManagerImpl(mContext);

        try {
            mChannel = mContext.getDeliveryChannel();
        } catch (MessagingException me) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_DELIVERY_CHANNEL_FAILED", new Object[] { me });
            throw me;
        }

        try {
            startInbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_INBOUND_RECVR_START_FAILED", new Object[] { ex });

            String errMsg = mMessages.getString("MSMQBindingComponent_INBOUND_RECVR_START_FAILED", new Object[] { ex });
            throw new JBIException(errMsg);
        }

        mDeployer = new MSMQBindingDeployer(mContext, mStatusProviderHelper, msmqChannelMgr, mInboundReceiver, mRuntimeConfig);
    }

    public void stop() throws JBIException {
        mLogger.log(Level.INFO, "MSMQBindingComponent_STOP_CALLED");

        try {
            stopOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_OUTBOUND_RECVR_STOP_FAILED", new Object[] { ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_OUTBOUND_RECVR_STOP_FAILED",
                    new Object[] { ex }));
        }

        try {
            stopInbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_INBOUND_STOP_FAILED", new Object[] { ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_INBOUND_STOP_FAILED", new Object[] { ex }));
        }

        try {
            removeChannels();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_CHANNELS_REMOVAL_FAILED", new Object[] { ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_CHANNELS_REMOVAL_FAILED",
                    new Object[] { ex }));

        }

        mBCStarted = true;
        mLogger.log(Level.INFO, "MSMQBindingComponent_STOP_DONE");
    }

    public void shutDown() throws JBIException {
        mLogger.log(Level.INFO, "MSMQBindingComponent_SHUTDOWN_CALLED");

        if (mChannel != null) {
            mChannel.close();
        }

        shutdownMBeans();

        mLogger.log(Level.INFO, "MSMQBindingComponent_SHUTDOWN_DONE");
    }

    public void start() throws JBIException {

        try {
            loadDLL();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_LOADING_NATIVE_DLL_FAILED", new Object[] { ex });

            String errMsg = mMessages.getString("MSMQBindingComponent_LOADING_NATIVE_DLL_FAILED", new Object[] { ex });
            throw new JBIException(errMsg);
        }

        try {
            startOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_OUTBOUND_RECVR_START_FAILED", new Object[] { ex });

            String errMsg = mMessages.getString("MSMQBindingComponent_OUTBOUND_RECVR_START_FAILED", new Object[] { ex });
            throw new JBIException(errMsg);
        }

        mBCStarted = true;
        mLogger.log(Level.INFO, "MSMQBindingComponent_START_DONE");
    }

    ////////
    //
    //  MSMQBindingComponent Public Methods
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

        MBeanServer mbServer = mContext.getMBeanServer();
        MBeanNames mbnHndl = mContext.getMBeanNames();

        try {
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_BINDING, mContext.getComponentName(), mContext.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            mLogger.log(Level.INFO, "MSMQBindingComponent_STATUS_MBEAN_REG_SUCCEEDED",
                    new Object[] { mContext.getComponentName() });

        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_STATUS_MBEAN_REG_FAILED", new Object[] { ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_STATUS_MBEAN_REG_FAILED",
                    new Object[] { ex }));
        }

        try {
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot());
            mRuntimeConfig.initialize();
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    mContext.getComponentName(), mContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            mLogger.log(Level.INFO, "MSMQBindingComponent_CONFIG_MBEAN_REG_SUCCEEDED",
                    new Object[] { mContext.getComponentName() });
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_CONFIG_MBEAN_REG_FAILED", new Object[] { ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_CONFIG_MBEAN_REG_FAILED",
                    new Object[] { ex }));
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
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_STATUS_MBEAN_UNREG_FAILED", new Object[] {
                    mContext.getComponentName(), ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_STATUS_MBEAN_UNREG_FAILED", new Object[] {
                    mContext.getComponentName(), ex }));
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingComponent_CONFIG_MBEAN_UNREG_FAILED", new Object[] {
                    mContext.getComponentName(), ex });

            throw new JBIException(mMessages.getString("MSMQBindingComponent_STATUS_MBEAN_UNREG_FAILED", new Object[] {
                    mContext.getComponentName(), ex }));
        }

    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR).
     * In the case of MSMQ, this thread will handle all messages from the NMR and
     * delegate them to worker threads.  These threads will be responsible for binding
     * the normalized messages to the MSMQ-specific protocol and handling the message
     * delivery.
     *
     * @exception    JBIException if any error occurs
     */
    private void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mContext, mChannel, mDeployer.getServiceUnits(), 
									mRuntimeConfig, msmqChannelMgr);
		new Thread(mOutboundReceiver).start();
        mLogger.log(Level.INFO, "MSMQBindingComponent_OUTBOUND_STARTED");
    }

    /**
     * Shuts down the thread that handles messages from the NMR
     *
     * @exception    JBIException if any error occurs
     */
    private void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        mLogger.log(Level.INFO, "MSMQBindingComponent_OUTBOUND_STOPPED");
    }

    /**
     * Starts the thread to handle messages for Binding Component (BC).
     * In the case of MSMQ, this thread will handle all messages from the BC and
     * delegate them to worker threads.
     *
     * @exception    JBIException if any error occurs
     */
    private void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext, mChannel, mRuntimeConfig, msmqChannelMgr);
        mLogger.log(Level.INFO, "MSMQBindingComponent_INBOUND_STARTED");
    }

    /**
     * Shuts down the thread(s) that handles messages from the MSMQ to NMR
     *
     * @exception    JBIException if any error occurs
     */
    private void stopInbound() {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }
        mLogger.log(Level.INFO, "MSMQBindingComponent_INBOUND_STOPPED");
    }

    private void removeChannels() {
        if (msmqChannelMgr != null) {
            msmqChannelMgr.removeChannels();
        }
        mLogger.log(Level.INFO, "MSMQBindingComponent_CHANNELS_REMOVED");
    }

    /**
     * Load the required native library files
     */
    private void loadDLL() throws Exception {
        try {
            System.loadLibrary("msmqbcjni");
            //System.loadLibrary("msmqruntimejni");
            //System.loadLibrary("MSMQXAResourceJni");
            mLogger.log(Level.INFO, "MSMQBindingComponent_NATIVE_DLL_LOADED");
        } catch (Exception ex) {
            throw ex;
        }
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setServiceUnitManager(MSMQBindingDeployer deployer) {
        mDeployer = deployer;
    }
}
