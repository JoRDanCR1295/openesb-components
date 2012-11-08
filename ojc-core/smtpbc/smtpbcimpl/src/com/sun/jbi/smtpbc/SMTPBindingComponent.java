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
 * @(#)SMTPBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.mbeans.RuntimeConfiguration;


/**
 * SMTPBindingComponent defines a SMTP JBI Component and ComponentLifeCycle.
 * This class implements the main interfaces with the JBI framework.  In
 * addition to implementing the Component and ComponentLifeCycle interfaces,
 * this class creates MBeans used for monitoring and management, and it creates
 * an initial thread to handle messages from the Normalized Message Router (NMR).
 * 
 * @author       Alexander Fung
 * @version      
 */
public class SMTPBindingComponent
        implements ComponentLifeCycle, Component {

    /**
     * Message manager for internationalized message bundles
     */
    private static final Messages mMessages =
            Messages.getMessages(SMTPBindingComponent.class);    
    /**
     * The logger used to log issues with this class
     */
    private static final Logger mLogger = Messages.getLogger(SMTPBindingComponent.class);
    
    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = SMTPBindingComponent.mMessages.getString("SMTP_BC");
    
    private ComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;
    private InboundReceiver mInboundReceiver;
    private MessageStore mMessageStore;

    private ObjectName mExtensionMBeanName;
    private DeliveryChannel mChannel;
    private SMTPBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;    

    ////////
    //
    //  Component Interface Methods
    //
    ////////

    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public Document getServiceDescription(final ServiceEndpoint serviceEndpoint) {

        Document result = null;
        final Iterator serviceUnits = mDeployer.getServiceUnits().iterator();
        while (serviceUnits.hasNext()) {
            final Iterator endpoints =
                ((ServiceUnit)serviceUnits.next()).getEndpoints().iterator();
            while(endpoints.hasNext()) {
                final Endpoint endpoint = (Endpoint)endpoints.next();
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
    
    public boolean isExchangeWithConsumerOkay(final ServiceEndpoint endpoint,
                                              final MessageExchange exchange) {
        return true;
    }

    public boolean isExchangeWithProviderOkay(final ServiceEndpoint endpoint,
                                              final MessageExchange exchange) {
        return false;
    }
        
    public ServiceEndpoint resolveEndpointReference(final DocumentFragment fragment) {
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
    
    public void init(final ComponentContext jbiContext) throws JBIException {
        if (jbiContext ==  null) {            
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Null_context"));
        }
        mContext = jbiContext;
        Messages.registerContext(mContext);
        mMessageStore = new MessageStore();
        initMBeans();

        mDeployer = new SMTPBindingDeployer(mContext, this);
        
        try {
            mChannel = mContext.getDeliveryChannel();
        } catch(final MessagingException me) {            
            SMTPBindingComponent.mLogger.log(Level.SEVERE, "SMTPBC_No_Dev_Channel", me.getMessage());
            throw me;
        }
        
        try {
            startOutbound();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.SEVERE, SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_start_outbound",  
                        ex.getMessage()), ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_start_outbound", 
                                   ex.getMessage()), ex);            
        }

        try {
            startInbound();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.SEVERE, SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_start_inbound",
                        ex.getMessage()), ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_start_inbound", 
                                   ex.getMessage()), ex);            
        }
    }
    
    public void shutDown() throws JBIException {
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.info("SMTPBC_Shutdown_BC");
        }
        try {
            stopInbound();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING,
                        SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_stop_inbound",
                                            ex.getMessage()),
                        ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_stop_inbound",
                                                       ex.getMessage()),
                                   ex);            
        }

        try {
            stopOutbound();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING,
                        SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_stop_outbound",
                                            ex.getMessage()),
                        ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_stop_outbound",
                                                       ex.getMessage()),
                                   ex);
        }
        
        if(mChannel != null) {
            mChannel.close();
        }
        
        shutdownMBeans();
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_Complete_BC_shutdown");
        }
    }
    
    public void start() throws JBIException {
    	if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_BC_started");
        }
    }
    
    public void stop() throws JBIException {
    	if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_BC_stopped");
        }
    }


    ////////
    //
    //  SMTPBindingComponent Public Methods
    //
    ////////

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
    public void initMBeans() throws JBIException {

        final MBeanServer mbServer = mContext.getMBeanServer();
        final MBeanNames mbnHndl = mContext.getMBeanNames();
        
        try {
            mStatusProviderHelper =
                new StatusProviderHelper(SMTPBindingComponent.SHORT_DISPLAY_NAME,
                                         StatusProviderMBean.COMPONENT_TYPE_BINDING,
                                         mContext.getComponentName(),
                                         mContext.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            
            if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
                SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_Register_mbean", 
                            mContext.getComponentName());
            }
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING, "SMTPBC_Failed_register_mbean", ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_register_mbean"), ex);
        }
        
        try {
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot());
            mRuntimeConfigHelper =
                new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    mContext.getComponentName(),
                    mContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
                SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_Register_config_mbean",
                            mContext.getComponentName());
            }            
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING, SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_register_config_mbean"), ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_register_config_mbean"), ex);
        }
    }

    /**
     * Shuts down the MBeans associated with this Binding Component.
     *
     * @exception    JBIException if any error occurs
     */
    public void shutdownMBeans() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING, SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_unregister_mbean",
                        mContext.getComponentName()), ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_unregister_mbean",
                                   mContext.getComponentName()), ex);            
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (final Exception ex) {
            SMTPBindingComponent.mLogger.log(Level.WARNING,
                        SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_unregister_config_mbean",
                                            mContext.getComponentName()),
                        ex);
            throw new JBIException(SMTPBindingComponent.mMessages.getString("SMTPBC_Failed_unregister_config_mbean",
                                                       mContext.getComponentName()),
                                   ex);
        }
    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR).
     * In the case of SMTP, this thread will handle all messages from the NMR and
     * delegate them to worker threads.  These threads will be responsible for binding
     * the normalized messages to the SMTP-specific protocol and handling the message
     * delivery.
     *
     * @exception    JBIException if any error occurs
     */
    public void startOutbound() throws JBIException {
        mOutboundReceiver =
            new OutboundReceiver(mChannel, mDeployer.getServiceUnits(),
                                 mRuntimeConfig,
                                 mMessageStore);        
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_started_outbound");
        }
    }
    
    /**
     * Shuts down the thread that handles messages from the NMR
     *
     * @exception    JBIException if any error occurs
     */
    public void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_stopped_outbound");
        }
    }

    /**
     * 
     *
     * @exception    JBIException if any error occurs
     */
    public void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext,
                                               mChannel,
                                               mMessageStore);
        mDeployer.addEndpointChangeListener(mInboundReceiver);
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_started_inbound");
        }
    }

    /**
     * 
     *
     * @exception    JBIException if any error occurs
     */
    public void stopInbound() throws JBIException {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }
        if (SMTPBindingComponent.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingComponent.mLogger.log(Level.INFO, "SMTPBC_stopped_inbound");
        }
    }

	public RuntimeConfiguration getRuntimeConfig() {
		return mRuntimeConfig;
	}

}
