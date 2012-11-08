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
 * @(#)LDAPBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;

import java.util.HashMap;
import java.util.Map;
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
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.mbeans.LDAPManagement;
import com.sun.jbi.ldapbc.util.AlertsUtil;

/**
 * BC life cycle implementation of the jbi framework
 *
 */
public class LDAPBindingLifeCycle implements ComponentLifeCycle, Component {

    private static final Messages mMessages = Messages.getMessages(LDAPBindingLifeCycle.class);
    private static Logger mLogger = Messages.getLogger(LDAPBindingLifeCycle.class);
    private static final Map initializedLifeCycles = new HashMap();
    public static final String SHORT_DISPLAY_NAME = "LDAP BC";
    private ComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;
    private InboundReceiver mInboundReceiver;
    private Thread mOutboundReceiverThread;
    Map mEndpoints = new HashMap();
    Map mEndpointMapping = new HashMap();
    private ObjectName mExtensionMBeanName;
    private DeliveryChannel mChannel;
    private ObjectName mDeployerMBeanName;
    private LDAPBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private ObjectName mManagementMBeanObjName;
    private LDAPManagement mManagementMBean;

    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION = "Normalization"; // NOI18N

    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N

    public static final String[] LDAP_PERF_CATEGORIES =
            new String[]{PERF_CAT_NORMALIZATION,
        PERF_CAT_DENORMALIZATION
    };

    public LDAPBindingLifeCycle() {
    }

    /**
     *
     * @return
     */
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    /**
     *
     * @return
     */
    public ServiceUnitManager getServiceUnitManager() {
        return mDeployer;
    }

    /**
     *
     * @return
     */
    public ObjectName getDeploymentMBeanName() {
        return mDeployerMBeanName;
    }

    /**
     *
     * @return
     */
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    /**
     *
     * @param jbiContext
     * @throws JBIException
     */
    public void init(final ComponentContext jbiContext) throws JBIException {
        if (jbiContext == null) {
            throw new JBIException(mMessages.getString("LDAPBC-R00212.LDAPBLC_Null_context"));
        }

        mContext = jbiContext;
        Messages.registerContext(mContext);
        LDAPBindingLifeCycle.mLogger = Messages.getLogger(LDAPBindingLifeCycle.class);
        LDAPBindingLifeCycle.initializedLifeCycles.put(mContext.getComponentName(), this);

        final MBeanServer mbServer = jbiContext.getMBeanServer();
        final MBeanNames mbnHndl = jbiContext.getMBeanNames();
        mDeployer = new LDAPBindingDeployer(mContext, this);

        final String componentName = jbiContext.getComponentName();

        try {
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    statusMBeanObjName,
                    mbServer);
            mStatusProviderHelper.registerMBean(LDAP_PERF_CATEGORIES, new LDAPPerformanceMeasurement());
            //mStatusProviderHelper.registerMBean();

            if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                LDAPBindingLifeCycle.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00250.LDAPBLC_Register_statistics_mbean", new Object[]{componentName}));
            }
        } catch (Exception ex) {
            String msg = mMessages.getString("LDAPBC-R00251.LDAPBLC_Failed_Register_statistics_mbean", new Object[]{ex.getLocalizedMessage()});
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                    LDAPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "LDAPBC-E00212.Mbean_register_failed_status_provider_exception");
            throw new JBIException(msg, ex);
        }
        try {
            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(mContext);
            ObjectName runtimeConfigMBeanObjName = mbnHndl.createCustomComponentMBeanName("Configuration");
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), keystoreUtil);
            //mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
            //        jbiContext.getComponentName(), jbiContext.getMBeanServer());
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(runtimeConfigMBeanObjName, mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);

            if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                LDAPBindingLifeCycle.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00214.LDAPBLC_Register_config_mbean",
                        new Object[]{componentName}));
            }
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.WARNING,
                    LDAPBindingLifeCycle.mMessages.getString("LDAPBC-W00202.LDAPBLC_Failed_register_config_mbean"), new Object[]{ex});
            throw new JBIException(mMessages.getString("LDAPBC-W00202.LDAPBLC_Failed_register_config_mbean", new Object[]{ex}));
        }

        try {
            mManagementMBeanObjName = mContext.getMBeanNames().createCustomComponentMBeanName("Administration");
            mManagementMBean = new LDAPManagement(mDeployer);
            MBeanServer server = mContext.getMBeanServer();
            if (server.isRegistered(mManagementMBeanObjName) == false) {
                server.registerMBean(mManagementMBean, mManagementMBeanObjName);
            }
            if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                LDAPBindingLifeCycle.mLogger.log(Level.INFO, mMessages.getString("LDAPLC_Administration_Mbean_Registered",
                        new Object[]{mContext.getComponentName(), mManagementMBeanObjName}));
            }
        } catch (Exception e) {
            String errMsg = mMessages.getString("LDAPBC-R00252.LDABLC_Failed_Administration_Mbean_Registered", new Object[]{e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            AlertsUtil.getAlerter().critical(errMsg,
                    SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "LDAPBC-E00115");
            throw new JBIException(errMsg);
        }



        try {
            mChannel = mContext.getDeliveryChannel();
        } catch (final MessagingException me) {
            LDAPBindingLifeCycle.mLogger.log(Level.SEVERE, mMessages.getString("LDAPBC-R00215.LDAPBLC_No_Dev_Channel", new Object[]{me.getMessage()}));
            throw me;
        }

        try {
            startOutbound();
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.SEVERE,
                    LDAPBindingLifeCycle.mMessages.getString("LDAPBC-E00202.LDAPBLC_Failed_start_outbound",
                    new Object[]{ex.getMessage(), ex}));
            throw new JBIException(LDAPBindingLifeCycle.mMessages.getString(
                    "LDAPBC-E00202.LDAPBLC_Failed_start_outbound", new Object[]{ex.getMessage(), ex}));
        }

        try {
            startInbound();
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.SEVERE,
                    LDAPBindingLifeCycle.mMessages.getString("LDAPBC-E00203.LDAPBLC_Failed_start_inbound",
                    new Object[]{ex.getMessage(), ex}));
            throw new JBIException(LDAPBindingLifeCycle.mMessages.getString(
                    "LDAPBC-E00203.LDAPBLC_Failed_start_inbound", new Object[]{ex.getMessage(), ex}));
        }
    }

    /**
     *
     * @throws JBIException
     */
    public void shutDown() throws JBIException {
        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00216.LDAPBLC_Shutdown_LDAPBC"));

        try {
            stopOutbound();
            stopInbound();
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.WARNING,
                    mMessages.getString("Failed to stop the outbound receiver: ", new Object[]{ex.getMessage(), ex}));
            throw new JBIException(mMessages.getString("Failed to stop the outbound receiver: ", new Object[]{ex.getMessage(), ex}));
        }

        if (mChannel != null) {
            mChannel.close();
        }

        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.WARNING,
                    LDAPBindingLifeCycle.mMessages.getString("LDAPBC-W00203.LDAPBLC_Failed_stop_outbound",
                    new Object[]{ex.getMessage(), ex}));
            throw new JBIException(LDAPBindingLifeCycle.mMessages.getString(
                    "LDAPBC-W00203.LDAPBLC_Failed_stop_outbound", new Object[]{ex.getMessage(), ex}));
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (final Exception ex) {
            LDAPBindingLifeCycle.mLogger.log(Level.WARNING,
                    LDAPBindingLifeCycle.mMessages.getString("LDAPBC-W00204.LDAPBLC_Failed_unregister_mbean", new Object[]{mContext.getComponentName(), ex}));
            throw new JBIException(LDAPBindingLifeCycle.mMessages.getString("LDAPBC-W00204.LDAPBLC_Failed_unregister_mbean", new Object[]{mContext.getComponentName(), ex}));
        }
        LDAPBindingLifeCycle.initializedLifeCycles.remove(mContext.getComponentName());
        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00217.LDAPBLC_Failed_Complete_LDAPBC_shutdown"));
    }

    /**
     *
     * @throws JBIException
     */
    public void start() throws JBIException {
        if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00218.LDAPBLC_LDAPBC_started"));
        }
    }

    /**
     *
     * @throws JBIException
     */
    public void stop() throws JBIException {
        if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00219.LDAPBLC_stopped"));
        }
    }

    /**
     *
     * @param endpoint
     * @param exchange
     * @return
     */
    public boolean isExchangeWithConsumerOkay(final ServiceEndpoint endpoint,
            final MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }

    /**
     *
     * @param endpoint
     * @param exchange
     * @return
     */
    public boolean isExchangeWithProviderOkay(final ServiceEndpoint endpoint,
            final MessageExchange exchange) {
        // In FileBC Currently no "inbound" is supported
        return false;
    }

    /**
     *
     * @param endpoint
     * @return
     */
    public Document getServiceDescription(final ServiceEndpoint endpoint) {
        Document result = null;

        // TODO: The document returned should be a stand-alone document (no imports or includes)
        // TODO: consider whether it should only return the abstract wsdl concerning the endpoint
        // TODO: Beware for service engines that they HAVE TO include a specific binding type defined for Service Engines
        final String uniqueName = EndpointImpl.getUniqueName(endpoint.getServiceName().toString(),
                endpoint.getEndpointName(), EndpointImpl.ENDPOINT_TYPE_OUTBOUND);
//        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
//        final DocumentBuilder documentBuilder = null;
        final EndpointImpl foundEndpoint = (EndpointImpl) mEndpoints.get(uniqueName);

        if (foundEndpoint == null) {
            LDAPBindingLifeCycle.mLogger.info("Endpoint " + uniqueName +
                    " could not be located in this binding component.");
        } else {
//            try {
//                final File matchedWSDL = (File) foundEndpoint.getValueObj(EndpointImpl.WSDL_FILE);
//                result = documentBuilder.parse(matchedWSDL);
//            } catch (final SAXException ex) {
//                LDAPBindingLifeCycle.mLogger.info("Endpoint " + result +
//                        "Cannot get the getServiceDescription Exception in SAXException");
//            } catch (final IOException exIO) {
//                LDAPBindingLifeCycle.mLogger.info("Endpoint " + result +
//                        "Cannot get the getServiceDescription Exception in IOException");
//
//            // throw exIO;
//            } catch (final Exception exception) {
//                LDAPBindingLifeCycle.mLogger.info("Endpoint " + result +
//                        "Cannot get the getServiceDescription Exception in Exception");
//
//            //throw exception;
//            }
            result = foundEndpoint.getServiceDescription();
        }

        return result;
    }

    /**
     *
     * @param fragment
     * @return
     */
    public ServiceEndpoint resolveEndpointReference(final DocumentFragment fragment) {
        // Currently we do not support dynamic endpoints
        return null;
    }

    /**
     *
     * @return
     */
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }

    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return (RuntimeConfigurationMBean) mRuntimeConfig;
    }

    /**
     *
     * @param componentName
     * @return
     */
    public static LDAPBindingLifeCycle getInstanceForId(final String componentName) {
        return (LDAPBindingLifeCycle) LDAPBindingLifeCycle.initializedLifeCycles.get(componentName);
    }

    /**
     *
     * @return
     */
    DeliveryChannel getDeliveryChannel() {
        return mChannel;
    }

    /**
     *
     * @param context
     * @return
     */
    public EndpointImpl getEndpointBeanForContext(final String context) {
        return (EndpointImpl) mEndpointMapping.get(context);
    }

    /**
     *
     * @param endpoints
     * @throws MessagingException
     */
    void activateEndpoints(final EndpointImpl[] endpoints) throws MessagingException, Exception {
        LDAPBindingLifeCycle.mLogger.info("Activating endpoints");

        EndpointImpl currEndpoint = null;

        for (EndpointImpl element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();

            if (!mEndpoints.containsKey(uniqueName)) {
                mEndpoints.put(uniqueName, currEndpoint);
            } else {
                throw new Exception(LDAPBindingLifeCycle.mMessages.getString("LDAPBC-R00224.LDAPBLC_Failed_deploy_EndPoint"));
            }

            // mEndpoints.put(uniqueName, currEndpoint);

            // For inbound endpoints, build an additional mapping from the URL
            // context to the endpoint
            if (currEndpoint.getValue(EndpointImpl.ENDPOINT_TYPE).equals(EndpointImpl.ENDPOINT_TYPE_INBOUND)) {
                try {
                    mInboundReceiver.addInboundMessageProcessor(currEndpoint);
                } catch (final FaultException ex) {
                    LDAPBindingLifeCycle.mLogger.log(Level.SEVERE, "SU_Failed_start_inbound_EP",
                            uniqueName + " Reason " + ex.getMessage());
                } catch (final JBIException me) {
                    LDAPBindingLifeCycle.mLogger.severe("Cannot activate endpoint " + uniqueName +
                            " Reason " + me.getMessage());
                }
            } else if (EndpointImpl.ENDPOINT_TYPE_OUTBOUND.equals(
                    currEndpoint.getValue(EndpointImpl.ENDPOINT_TYPE))) {
                // Activate an outbound endpoint
                try {
                    final QName fullServiceName = (QName) currEndpoint.getValueObj(EndpointImpl.FULL_SERVICE_NAME);
                    final ServiceEndpoint endpointReference = mContext.activateEndpoint(fullServiceName,
                            currEndpoint.getValue(EndpointImpl.ENDPOINT_NAME));
                    currEndpoint.setValueObj(EndpointImpl.ENDPOINT_REFERENCE,
                            endpointReference);

                    if (LDAPBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                        LDAPBindingLifeCycle.mLogger.info("Activated outbound endpoint " +
                                uniqueName);
                    }
                } catch (final JBIException me) {
                    LDAPBindingLifeCycle.mLogger.severe("Cannot activate endpoint " + uniqueName +
                            " Reason " + me.getMessage());
                }
            }

            currEndpoint.setValue(EndpointImpl.STATUS,
                    EndpointImpl.STATUS_RUNNING);
        }

        LDAPBindingLifeCycle.mLogger.info("Activated endpoints");
    }

    /**
     *
     * @param endpoints
     * @throws MessagingException
     */
    void deactivateEndpoints(final EndpointImpl[] endpoints)
            throws MessagingException {
        EndpointImpl currEndpoint = null;
        LDAPBindingLifeCycle.mLogger.info("Deactivating endpoints");

        for (EndpointImpl element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();
            mEndpoints.remove(uniqueName);

            if (currEndpoint.getValue(EndpointImpl.ENDPOINT_TYPE).equals(EndpointImpl.ENDPOINT_TYPE_INBOUND)) {
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointImpl.ENDPOINT_REFERENCE);
                    mContext.deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    LDAPBindingLifeCycle.mLogger.severe("Cannot deactivate endpoint " + uniqueName +
                            " Reason " + me.getMessage());
                }

                mInboundReceiver.removeInboundMessageProcessor(currEndpoint);
            } else {
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointImpl.ENDPOINT_REFERENCE);
                    mContext.deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    LDAPBindingLifeCycle.mLogger.severe("Cannot deactivate endpoint " + uniqueName +
                            " Reason " + me.getMessage());
                }
            }

            currEndpoint.setValue(EndpointImpl.STATUS,
                    EndpointImpl.STATUS_STOPPED);
        }

        LDAPBindingLifeCycle.mLogger.info("Deactivated endpoints");
    }

    /**
     *
     * @throws JBIException
     */
    void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext);
        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00220.LDAPBLC_started_inbound"));
    }

    /**
     *
     * @throws JBIException
     */
    void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext);
        mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();
        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00221.LDAPBLC_started_outbound"));
    }

    void stopInbound() {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }

        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00223.LDAPBLC_stopped_inbound"));
    }

    void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }

        LDAPBindingLifeCycle.mLogger.info(mMessages.getString("LDAPBC-R00222.LDAPBLC_stopped_outbound"));
    }
}
