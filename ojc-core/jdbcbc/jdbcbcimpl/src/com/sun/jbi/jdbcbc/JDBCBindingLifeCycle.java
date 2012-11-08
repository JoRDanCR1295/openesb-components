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
 * @(#)JDBCBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import org.xml.sax.SAXException;
import java.io.File;
import java.io.IOException;
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
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.xml.sax.SAXException;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.jdbcbc.util.AlertsUtil;
import com.sun.jbi.alerter.NotificationEvent;

/**
 * BC life cycle implementation of the jbi framework
 *
 */
public class JDBCBindingLifeCycle implements ComponentLifeCycle, Component {
    private static final Messages mMessages = Messages.getMessages(JDBCBindingLifeCycle.class);
    public static Logger mLogger = Messages.getLogger(JDBCBindingLifeCycle.class);
    private static final Map<String,JDBCBindingLifeCycle> initializedLifeCycles = new HashMap<String,JDBCBindingLifeCycle>();
    public static final String SHORT_DISPLAY_NAME = "JDBC BC";
    private JDBCComponentContext mContext = JDBCComponentContext.getInstance();
    private OutboundReceiver mOutboundReceiver;
    private InboundReceiver mInboundReceiver;
    private Thread mOutboundReceiverThread;
    Map<String,EndpointBean> mEndpoints = new HashMap<String,EndpointBean>();
    Map mEndpointMapping = new HashMap();
    private ObjectName mExtensionMBeanName;
    private MessagingChannel mChannel;
    private ObjectName mDeployerMBeanName;
    private JDBCBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    public JDBCBindingLifeCycle() {
    }

    /**
     *
     * @return
     */
    //@Override
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    /**
     *
     * @return
     */
    //@Override
    public ServiceUnitManager getServiceUnitManager() {
        return mDeployer;
    }

    /**
     *
     * @return
     */
    private ObjectName getDeploymentMBeanName() {
        return mDeployerMBeanName;
    }

    /**
     *
     * @return
     */
    //@Override
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    /**
     *
     * @param jbiContext
     * @throws JBIException
     */
   //@Override
    public void init(final ComponentContext jbiContext) throws JBIException {
        if (jbiContext == null) {
            throw new JBIException("Component Context is null");
        }

        mContext.setContext(jbiContext);
        Messages.registerContext(jbiContext);
        JDBCBindingLifeCycle.mLogger = Messages.getLogger(JDBCBindingLifeCycle.class);
        JDBCBindingLifeCycle.initializedLifeCycles.put(mContext.getContext().getComponentName(), this);

        final MBeanServer mbServer = jbiContext.getMBeanServer();
        final MBeanNames mbnHndl = jbiContext.getMBeanNames();
        mDeployer = new JDBCBindingDeployer(mContext.getContext(), this);

        final String componentName = jbiContext.getComponentName();

        try {
            mStatusProviderHelper = new StatusProviderHelper(JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_BINDING,
                    jbiContext.getComponentName(), jbiContext.getMBeanServer());
            mStatusProviderHelper.registerMBean();

            if (JDBCBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                JDBCBindingLifeCycle.mLogger.log(Level.INFO, "SQLSE_R00102.JDBCBLC_Register_mbean", componentName);
            }
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.WARNING, "SQLSE_E00103.JDBCBLC_Failed_register_mbean", ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "SQLSE_E00103.JDBCBLC_Failed_register_mbean"), ex);
        }

        try {
            mRuntimeConfig = new RuntimeConfiguration(mContext.getContext().getWorkspaceRoot());
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    jbiContext.getComponentName(), jbiContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);

            if (JDBCBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
                JDBCBindingLifeCycle.mLogger.log(Level.INFO, "SQLSE_R00104.JDBCBLC_Register_config_mbean",
                    componentName);
            }
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.WARNING,
                JDBCBindingLifeCycle.mMessages.getString("SQLSE_E00105.JDBCBLC_Failed_register_config_mbean"), ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "SQLSE_E00105.JDBCBLC_Failed_register_config_mbean"), ex);
        }

		try {
            //mChannel = mContext.getDeliveryChannel();
            mChannel = new BaseMessagingChannel(mContext.getContext());
            mContext.setBindingChannel(mChannel);
        } catch(MessagingException me) {
            String text = mMessages.getString("JDBCBC-E00141.No_binding_channel", me.getLocalizedMessage());
            mLogger.log(Level.SEVERE, text, me);
            AlertsUtil.getAlerter().critical(text, 
                                             JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "JDBCBC-E00141");
            throw me;
        }

        //try {
            mChannel = mContext.getBindingChannel();
        //} catch (final MessagingException me) {
        //    JDBCBindingLifeCycle.mLogger.log(Level.SEVERE, "SQLSE_E00106.JDBCBLC_No_Dev_Channel", me.getMessage());
        //    throw me;
        //}

        try {
            startOutbound();
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.SEVERE,
                JDBCBindingLifeCycle.mMessages.getString("SQLSE_E00107.JDBCBLC_Failed_start_outbound",
                    ex.getMessage()), ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "SQLSE_E00107.JDBCBLC_Failed_start_outbound", ex.getMessage()), ex);
        }

        try {
            startInbound();
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.SEVERE,
                JDBCBindingLifeCycle.mMessages.getString("JDBCBLC_Failed_start_inbound",
                    ex.getMessage()), ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "SQLSE_E00108.JDBCBLC_Failed_start_inbound", ex.getMessage()), ex);
        }
    }

    /**
     *
     * @throws JBIException
     */
    //@Override
    public void shutDown() throws JBIException {
        JDBCBindingLifeCycle.mLogger.info("SQLSE_R00109.JDBCBLC_Shutdown_JDBCBC");

        try {
            stopOutbound();
            stopInbound();
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.WARNING,
                "Failed to stop the outbound receiver: " + ex.getMessage(), ex);
            throw new JBIException("Failed to stop the outbound receiver: " +
                ex.getMessage(), ex);
        }

        if (mChannel != null) {
            mChannel.close();
        }

        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.WARNING,
                JDBCBindingLifeCycle.mMessages.getString("SQLSE_E00110.JDBCBLC_Failed_stop_outbound",
                    ex.getMessage()), ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "JDBCBLC_Failed_stop_outbound", ex.getMessage()), ex);
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (final Exception ex) {
            JDBCBindingLifeCycle.mLogger.log(Level.WARNING,
                JDBCBindingLifeCycle.mMessages.getString("SQLSE_E00110.JDBCBLC_Failed_unregister_mbean",
                    mContext.getContext().getComponentName()), ex);
            throw new JBIException(JDBCBindingLifeCycle.mMessages.getString(
                    "JDBCBLC_Failed_unregister_mbean",
                    mContext.getContext().getComponentName()), ex);
        }

        JDBCBindingLifeCycle.initializedLifeCycles.remove(mContext.getContext().getComponentName());
        mLogger.log(Level.SEVERE,mMessages.getString("SQLSE_E00113.JDBCBLC_Failed_Complete_JDBCBC_shutdown"));
    }

    /**
     *
     * @throws JBIException
     */
    //@Override
    public void start() throws JBIException {
        if (JDBCBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingLifeCycle.mLogger.info("SQLSE_R00114.JDBCBLC_JDBCBC_started");
        }
    }

    /**
     *
     * @throws JBIException
     */
    //@Override
    public void stop() throws JBIException {
        if (JDBCBindingLifeCycle.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingLifeCycle.mLogger.info("SQLSE_R00118.JDBCBLC_stopped");
        }
    }

    /**
     *
     * @param endpoint
     * @param exchange
     * @return
     */
    //@Override
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
    //@Override
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
    //@Override
    public Document getServiceDescription(final ServiceEndpoint endpoint) {
        Document result = null;

        // TODO: The document returned should be a stand-alone document (no imports or includes)
        // TODO: consider whether it should only return the abstract wsdl concerning the endpoint
        // TODO: Beware for service engines that they HAVE TO include a specific binding type defined for Service Engines
        final String uniqueName = EndpointBean.getUniqueName(endpoint.getServiceName()
                                                               .toString(),
                endpoint.getEndpointName(), EndpointBean.ENDPOINT_TYPE_OUTBOUND);
        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = null;
        final EndpointBean foundEndpoint = mEndpoints.get(uniqueName);

        if (foundEndpoint == null) {
            JDBCBindingLifeCycle.mLogger.info("Endpoint " + uniqueName +
                " could not be located in this binding component.");
        } else {
            try {
                final File matchedWSDL = (File) foundEndpoint.getValueObj(EndpointBean.WSDL_FILE);
                result = documentBuilder.parse(matchedWSDL);
            } catch (final SAXException ex) {
                mLogger.log(Level.SEVERE,"Endpoint " + result +
                    "Cannot get the getServiceDescription Exception in SAXException",ex);
            } catch (final IOException exIO) {
                mLogger.log(Level.SEVERE,"Endpoint " + result +
                    "Cannot get the getServiceDescription Exception in IOException",exIO);

                // throw exIO;
            } catch (final Exception exception) {
                mLogger.log(Level.SEVERE,"Endpoint " + result +
                    "Cannot get the getServiceDescription Exception in Exception",exception);

                //throw exception;
            }
        }

        return result;
    }

    /**
     *
     * @param fragment
     * @return
     */
   //@Override
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

    /**
     *
     * @param componentName
     * @return
     */
    public static JDBCBindingLifeCycle getInstanceForId(final String componentName) {
        return JDBCBindingLifeCycle.initializedLifeCycles.get(componentName);
    }

    /**
     *
     * @return
     */
    public MessagingChannel getMessagingChannel() {
        return mChannel;
    }

    public DeliveryChannel getDeliveryChannel(){
       return mContext.getBindingChannel();
    }
    
    /**
     *
     * @param context
     * @return
     */
    public EndpointBean getEndpointBeanForContext(final String context) {
        return (EndpointBean) mEndpointMapping.get(context);
    }

    /**
     *
     * @param endpoints
     * @throws MessagingException
     * @throws java.lang.Exception 
     */
    public void activateEndpoints(final EndpointBean[] endpoints) throws MessagingException , Exception {
        JDBCBindingLifeCycle.mLogger.info("Activating endpoints");

        EndpointBean currEndpoint = null;

        for (EndpointBean element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();

			if(!mEndpoints.containsKey(uniqueName)) {
				mEndpoints.put(uniqueName, currEndpoint);
			} else  {
				throw new Exception( mMessages.getString("SQLSE_E00120.JDBCBLC_Failed_deploy_EndPoint"));
			}

           // mEndpoints.put(uniqueName, currEndpoint);

            // For inbound endpoints, build an additional mapping from the URL
            // context to the endpoint
            if (currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
                try {
                    mInboundReceiver.addInboundMessageProcessor(currEndpoint);
                } catch (final FaultException ex) {
                    mLogger.log(Level.SEVERE, "SQLSE_R00137.SQLSELC_SU_Failed_start_inbound_EP",
                        uniqueName + " Reason " + ex.getMessage());
                } catch (final JBIException me) {
                    mLogger.severe("Cannot activate endpoint " + uniqueName +
                        " Reason " + me.getMessage());
                }
            } else if (EndpointBean.ENDPOINT_TYPE_OUTBOUND.equals(
                        currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE))) {
                // Activate an outbound endpoint
                try {
                    final QName fullServiceName = (QName) currEndpoint.getValueObj(EndpointBean.FULL_SERVICE_NAME);
                    final ServiceEndpoint endpointReference = mContext.getContext().activateEndpoint(fullServiceName,
                            currEndpoint.getValue(EndpointBean.ENDPOINT_NAME));
                    currEndpoint.setValueObj(EndpointBean.ENDPOINT_REFERENCE,
                        endpointReference);

                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO,"INFO,SQLSE_R00319.SQLSESU_ENDPOINT_ACTIVATED",
                            uniqueName);
                    }
                } catch (final JBIException me) {
                    mLogger.severe("Cannot activate endpoint " + uniqueName +
                        " Reason " + me.getMessage());
                }
            }

            currEndpoint.setValue(EndpointBean.STATUS,
                EndpointBean.STATUS_RUNNING);
        }

        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00319.SQLSESU_ENDPOINT_ACTIVATED"));
    }

    /**
     *
     * @param endpoints
     * @throws MessagingException
     */
    public void deactivateEndpoints(final EndpointBean[] endpoints)
        throws MessagingException {
        EndpointBean currEndpoint = null;
        mLogger.info("Deactivating endpoints");

        for (EndpointBean element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();
            mEndpoints.remove(uniqueName);

            if (currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointBean.ENDPOINT_REFERENCE);
                    mContext.getContext().deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    JDBCBindingLifeCycle.mLogger.severe("Cannot deactivate endpoint " + uniqueName +
                        " Reason " + me.getMessage());
                }

                mInboundReceiver.removeInboundMessageProcessor(currEndpoint);
            } else {
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointBean.ENDPOINT_REFERENCE);
                    mContext.getContext().deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    mLogger.log(Level.SEVERE,"Cannot deactivate endpoint " + uniqueName +
                        " Reason " + me.getMessage());
                }
            }

            currEndpoint.setValue(EndpointBean.STATUS,
                EndpointBean.STATUS_STOPPED);
        }

        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00320.SQLSESU_ENDPOINT_DEACTIVATED"));
    }

    /**
     *
     * @throws JBIException
     */
    public void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext.getContext());
        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00116.JDBCBLC_started_inbound"));
    }

    /**
     *
     * @throws JBIException
     */
    public void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext);
        mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();
        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00117.JDBCBLC_started_outbound"));
    }

    public void stopInbound() {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }

        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00119.JDBCBLC_stopped_inbound"));
    }

    public void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }

        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00118.JDBCBLC_stopped_outbound"));
    }
}
