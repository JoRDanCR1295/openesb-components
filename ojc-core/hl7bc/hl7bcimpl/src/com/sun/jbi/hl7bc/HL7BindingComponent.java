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
 * @(#)HL7BindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.io.File;
import java.io.IOException;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.Component;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.management.MBeanNames;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.naming.InitialContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.DBSchemaCreation;
import com.sun.jbi.hl7bc.extservice.server.HL7ServerFactory;
import com.sun.jbi.hl7bc.extservice.server.OutboundTcpServerHL7ConnectorPool;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.hl7bc.management.HL7BCManagement;
import com.sun.jbi.hl7bc.management.HL7BCManagementMBean;
import com.sun.jbi.hl7bc.management.HL7BCManagementMBeanHelper;
import com.sun.jbi.hl7bc.util.ReadWriteTextFile;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.common.util.Util;

/**
 * This class implements the ComponentLifeCycle and Component interfaces. The implementation of the
 * Component interface allows the JBI framework to query the HL7 BC component for various types of
 * information. The implementation of the ComponentLifeCycle interface provides initialization,
 * start, stop, and shutdown life cycle processing.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */

public class HL7BindingComponent implements ComponentLifeCycle, Component {

    private static Logger mLogger = Logger.getLogger(HL7BindingComponent.class.getName());

    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification/v1.0"; // NO

    // i18n

    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n

    private static final String JBI_COMPVER_ATTR = "component-version"; // NO i18n

    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n

    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION = "Normalization"; // NOI18N

    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N

    public static final String[] HL7BC_PERF_CATEGORIES = new String[] { PERF_CAT_NORMALIZATION,
            PERF_CAT_DENORMALIZATION };

    /**
     * Display name for this Binding Component
     */
    // public static final String SHORT_DISPLAY_NAME = mMessages.getString("HL7_BC");
    // A short display name
    public static final String SHORT_DISPLAY_NAME = "sun-hl7-binding";

    private ComponentContext mContext;

    private DocumentBuilder mDocumentBuilder;

    private OutboundReceiver mOutboundReceiver;

    private InboundReceiver mInboundReceiver;

    private ObjectName mExtensionMBeanName;

    private MessagingChannel mChannel;

    private HL7BindingDeployer mDeployer;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfiguration mRuntimeConfig;

    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    private HL7BCManagementMBean mManagementMBean;

    private HL7BCManagementMBeanHelper mManagementMBeanHelper;

    private OutboundTcpServerHL7ConnectorPool mOutboundServerPool;
    
    /**
     * Default constructor
     */
    public HL7BindingComponent() {
        DocumentBuilderFactory aFactory = DocumentBuilderFactory.newInstance();
        aFactory.setIgnoringComments(true);
        aFactory.setNamespaceAware(true);
        try {
            mDocumentBuilder = aFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        }
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
            throw new JBIException(I18n.msg("E0150: Component Context is null"));
        }
        mContext = jbiContext;
        HL7ComponentContext.getInstance().setContext(mContext);
        HL7ComponentContext.getInstance().setAssociatedLifeCycle(this);

        // Messages.registerContext(mContext);
        // mLogger = Messages.getLogger(HL7BindingComponent.class);
        mLogger = Util.getLogger(mContext, HL7BindingComponent.class.getName());
        mDeployer = new HL7BindingDeployer(mContext, this);
        String componentName = jbiContext.getComponentName();

        // Register MBeans
        initMBeans();
        // create Database schema
        // get the properties
        Properties prop = mRuntimeConfig.getProperties();
        // get the Initial Context
        InitialContext context = mContext.getNamingContext();
        try {
            DBConnectionFactory fac = new DBConnectionFactory(prop, context, mContext.getInstallRoot());
            DBSchemaCreation.getInstance().checkAndCreateDatabaseObjects(prop, fac);
        } catch (HL7RuntimeException hl7re) {
            mLogger.log(Level.SEVERE, I18n.msg("E0151: Failed to create Database Schema", hl7re.getMessage()), hl7re);
            throw new JBIException(I18n.msg("E0151: Failed to create Database Schema", hl7re.getMessage()), hl7re);
        }

        try {
            mChannel = new BaseMessagingChannel(mContext);
            HL7ComponentContext.getInstance().setBindingChannel(mChannel);
        } catch (MessagingException me) {
            mLogger.log(Level.SEVERE,
                    I18n.msg("E0152: Cannot get Delivery Channel from context : {0}", me.getMessage()), me);
            AlertsUtil.getAlerter().critical(
                    I18n.msg("E0152: Cannot get Delivery Channel from context : {0}", me.getMessage()), componentName,
                    HL7BindingComponent.SHORT_DISPLAY_NAME, AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0152");
            throw me;
        }
    }

    public void start() throws JBIException {
        mOutboundServerPool = HL7ServerFactory.createOutboundHL7Server();
    	
    	try {
            startOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, I18n.msg("E0153: Failed to start the outbound receiver : {0}", ex.getMessage()),
                    ex);
            AlertsUtil.getAlerter().critical(
                    I18n.msg("E0153: Failed to start the outbound receiver : {0}", ex.getMessage()),
                    HL7BindingComponent.SHORT_DISPLAY_NAME, null, AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0153");
            throw new JBIException(I18n.msg("E0153: Failed to start the outbound receiver : {0}", ex.getMessage()), ex);
        }

        try {
            startInbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, I18n.msg("E0154: Failed to start the inbound receiver : {0}", ex.getMessage()),
                    ex);
            AlertsUtil.getAlerter().critical(
                    I18n.msg("E0154: Failed to start the inbound receiver : {0}", ex.getMessage()),
                    HL7BindingComponent.SHORT_DISPLAY_NAME, null, AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0154");
            throw new JBIException(I18n.msg("E0154: Failed to start the inbound receiver : {0}", ex.getMessage()), ex);
        }

        if (mLogger.isLoggable(Level.INFO)) {
            logComponentInfo();
        }
        
        
        
    }

    public void shutDown() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0104: Component {0} shutting down...", mContext.getComponentName()));
        }
        try {
            stopInbound();
        } catch (Exception ex) {
            String msg = I18n.msg("W0107: Failed to stop the inbound receiver : {0}", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0107");
            throw new JBIException(msg, ex);
        }

        if (mChannel != null) {
            mChannel.close();
        }

        shutdownMBeans();

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0105: Component {0} shutdown completed.", mContext.getComponentName()));
        }
        
        mOutboundServerPool.stopPool(mContext);
    }

    public void stop() throws JBIException {

        try {
            stopOutbound();
        } catch (Exception ex) {
            String msg = I18n.msg("W0108: Failed to stop the outbound receiver : {0}", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0108");
            throw new JBIException(msg, ex);
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0106: HL7 Binding component stopped"), mContext.getComponentName());
        }
    }

    /***********************************************************************************************
     * HL7 Binding Component specific methods
     **********************************************************************************************/
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }

    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return (RuntimeConfigurationMBean) mRuntimeConfig;
    }

    /**
     * Initializes the MBeans associated with this Binding Component. Currently, three MBeans are
     * created for use with this Bindng Component:
     * <ul>
     * <li>StatusProviderMBean - Provides status about this Binding Component</li>
     * <li>RuntimeConfigurationMBean - Provides configurability for this Binding component</li>
     * <li>ManagementMBean - Manages suspending or resuming of the endpoints</li>
     * </ul>
     * 
     * @exception JBIException if any error occurs
     */
    private void initMBeans() throws JBIException {

        assert (mContext != null);

        /**
         * Initializes the MBeans associated with this Binding Component. Currently, three MBeans
         * are created for use with this Binding Component: 1. StatusProviderMBean - Provides status
         * about this Binding Component 2. RuntimeConfigurationMBean - Provides configurability for
         * this Binding component 3. ManagementMBean - Manages suspending or resuming of the
         * endpoints
         */

        MBeanServer mbServer = mContext.getMBeanServer();
        String componentName = mContext.getComponentName();
        MBeanNames mbNames = mContext.getMBeanNames();

        try {
            ObjectName statusMBeanObjName = mbNames.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME, statusMBeanObjName, mbServer);
            // mStatusProviderHelper.registerMBean();
            mStatusProviderHelper.registerMBean(HL7BC_PERF_CATEGORIES, new HL7BCPerformanceMeasurement());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, I18n.msg("I0107: Registered Status Provider MBean for {0}", componentName));
            }
        } catch (Exception ex) {
            String msg = I18n.msg("E0155: Failed to register status provider MBean. An exception was raised :{0}",
                    ex.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0155");
            throw new JBIException(msg, ex);
        }

        try {

            ObjectName runtimeConfigMBeanObjName = mbNames.createCustomComponentMBeanName("Configuration");
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot());
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(runtimeConfigMBeanObjName, mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, I18n.msg("Registered runtime configuration MBean for {0}", componentName));
            }
        } catch (Exception ex) {
            String msg = I18n.msg("E0156: Failed to register configuration MBean. An exception was raised: {0}",
                    ex.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg, componentName, HL7BindingComponent.SHORT_DISPLAY_NAME,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0156");
            throw new JBIException(msg, ex);
        }

        try {
            ObjectName managementMBeanObjName = mbNames.createCustomComponentMBeanName("Administration");
            mManagementMBean = new HL7BCManagement(mDeployer, mRuntimeConfig.getProperties(), mContext);
            mManagementMBeanHelper = new HL7BCManagementMBeanHelper(managementMBeanObjName, mbServer);
            mManagementMBeanHelper.registerMBean(mManagementMBean);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, I18n.msg("Registered management MBean for {0} with name {1}", componentName,
                        managementMBeanObjName));
            }
        } catch (Exception ex) {
            String text = I18n.msg("E0157: Failed to register Administration MBean. An exception was raised: {0}",
                    ex.getMessage());
            AlertsUtil.getAlerter().critical(text, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0157");
            throw new JBIException(text, ex);
        }
    }

    /**
     * Shuts down the MBeans associated with this Binding Component.
     * 
     * @exception JBIException if any error occurs
     */
    private void shutdownMBeans() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = I18n.msg(
                    "W0109: Failed to un-register status provider MBean for {0}. An exception was raised: {1}",
                    mContext.getComponentName(), ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0109");
            throw new JBIException(msg, ex);
        }
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = I18n.msg(
                    "W0110: Failed to un-register runtime configuration MBean for {0}. An exception was raised: {1}",
                    mContext.getComponentName(), ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0110");
            throw new JBIException(msg, ex);
        }

        try {
            mManagementMBeanHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = I18n.msg(
                    "W0111: Failed to register Administration MBean for {0}. An exception was raised: {1}",
                    mContext.getComponentName(), ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0111");
            throw new JBIException(msg, ex);
        }

    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR). In the case of
     * HL7, this thread will handle all messages from the NMR and delegate them to worker threads.
     * These threads will be responsible for binding the normalized messages to the HL7-specific
     * protocol and handling the message delivery.
     * 
     * @exception JBIException if any error occurs
     */
    private void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mContext, mChannel, mDeployer.getServiceUnits(), mRuntimeConfig, mOutboundServerPool);
        Thread mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0108: Started Outbound receiver"), mContext.getComponentName());
        }
    }

    /**
     * Shuts down the thread that handles messages from the NMR
     * 
     * @exception JBIException if any error occurs
     */
    private void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0109: Stopped Outbound receiver"), mContext.getComponentName());
        }
    }

    /**
     * @exception JBIException if any error occurs
     */
    private void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext, mChannel, mRuntimeConfig);
        mLogger.info(I18n.msg("I0110: Started inbound receiver."));
    }

    /**
     * @exception JBIException if any error occurs
     */
    private void stopInbound() throws JBIException {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }
        mLogger.info(I18n.msg("I0111: Stopped inbound receiver."));
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
    void setServiceUnitManager(HL7BindingDeployer deployer) {
        mDeployer = deployer;
    }

    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(I18n.msg(
                "I0112: Component {0} started with the following configuration:", mContext.getComponentName()));
        msgBuf.append('\n');

        // Identification information
        String installRoot = mContext.getInstallRoot();
        identity(msgBuf, installRoot);

        // Runtime configuration
        mRuntimeConfig.dump(msgBuf);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, msgBuf.toString());
        }
    }

    private void identity(StringBuffer buf, String idDescPath) {
        final String JBI_XML_PATH = idDescPath.concat("/META-INF/jbi.xml");
        String compVersion = "unknown";
        String buildNumber = "unknown";
        File desc = new File(JBI_XML_PATH);
        try {
            Document document = mDocumentBuilder.parse(desc);
            NodeList versioninfoNodes = document.getElementsByTagNameNS(JBI_DESC_ID_NS, JBI_VERSIONINFO_TAG);
            for (int i = 0; i < versioninfoNodes.getLength(); ++i) {
                Node versioninfoNode = versioninfoNodes.item(i);
                if (versioninfoNode.hasAttributes()) {
                    NamedNodeMap attrMap = versioninfoNode.getAttributes();
                    Node compVerAttrNode = attrMap.getNamedItem(JBI_COMPVER_ATTR);
                    if (compVerAttrNode != null && compVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        compVersion = compVerAttrNode.getNodeValue();
                    }
                    Node buildVerAttrNode = attrMap.getNamedItem(JBI_BUILDVER_ATTR);
                    if (buildVerAttrNode != null && buildVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        buildNumber = buildVerAttrNode.getNodeValue();
                    }
                    break;
                }
            }
        } catch (SAXException e) {
            String msg = I18n.msg(
                    "W0112: Parsing the JBI descriptor ({0}) failed. Version information for this component is not available. An exception was raised. {1}",
                    JBI_XML_PATH, e.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0112");
        } catch (IOException e) {
            String msg = I18n.msg(
                    "W0113: Reading the JBI descriptor ({0}) failed. Version information for this component is not available. An exception was raised. {1}",
                    JBI_XML_PATH, e.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, e);
        }
        buf.append("component-version: ").append(compVersion).append('\n');
        buf.append("build-number: ").append(buildNumber).append('\n');
    }
}
