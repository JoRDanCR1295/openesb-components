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
 * @(#)FileBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.filebc.management.FileBCManagement;
import com.sun.jbi.filebc.management.FileBCManagementMBean;
import com.sun.jbi.filebc.management.FileBCManagementMBeanHelper;
import com.sun.jbi.filebc.util.AlertsUtil;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.common.qos.messaging.MessagingChannel;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.JBIException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.management.MBeanNames;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This class implements the ComponentLifeCycle and Component interfaces.
 * The implementation of the Component interface allows the JBI framework
 * to query the File BC component for various types of information.
 * The implementation of the ComponentLifeCycle interface provides initialization,
 * start, stop, and shutdown life cycle processing.
 *
 * @author Sherry Weng
 */
public class FileBindingLifeCycle implements ComponentLifeCycle, Component {

    // A short display name
    public static final String SHORT_DISPLAY_NAME = "sun-file-binding";
    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification/v1.0"; // NO i18n
    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n
    private static final String JBI_COMPONENTVER_ATTR = "component-version"; // NO i18n
    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n
    private static final Messages mMessages = Messages.getMessages(FileBindingLifeCycle.class);
    private static Logger mLogger;
    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String[] FILEBC_PERF_CATEGORIES =
            new String[]{PERF_CAT_NORMALIZATION,
        PERF_CAT_DENORMALIZATION};
//    public static final String SHORT_DISPLAY_NAME = mMessages.getString("File_BC");;
    private DocumentBuilder mDocumentBuilder;
    private ComponentContext mContext;
    private ObjectName mExtensionMBeanName;
    //private DeliveryChannel mChannel;
    private MessagingChannel mChannel;
    private FileBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private InboundReceiver mInboundReceiver;
    private Receiver mOutboundReceiver;
    private FileBCManagementMBeanHelper mManagementMBeanHelper;
    private FileBCManagementMBean mManagementMBean;

    /**
     * Default constructor
     */
    public FileBindingLifeCycle() {
        DocumentBuilderFactory aFactory = DocumentBuilderFactory.newInstance();
        aFactory.setIgnoringComments(true);
        aFactory.setNamespaceAware(true);
        try {
            mDocumentBuilder = aFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        }
    }

    /****************************************************************
     * Component Interface Methods
     ****************************************************************/
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        Document theDoc = null;
        for (Iterator serviceUnits = mDeployer.getServiceUnits().values().iterator();
                serviceUnits.hasNext();) {
            for (Iterator endpoints = ((ServiceUnit) serviceUnits.next()).getEndpoints().iterator();
                    endpoints.hasNext();) {
                Endpoint endpoint = (Endpoint) endpoints.next();
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName()) &&
                        serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName()) &&
                        endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
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
        return false;
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment fragment) {
        // No dynamic endpoint(s) is supported for now
        return null;
    }

    /****************************************************************
     * ComponentLifeCycle Interface Methods
     ****************************************************************/
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    public void init(ComponentContext jbiContext) throws JBIException {
        if (jbiContext == null) {
            throw new JBIException(mMessages.getString("FILEBC-E00101.Component_context_null"));
        }
        mContext = jbiContext;
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(FileBindingLifeCycle.class);
        mDeployer = new FileBindingDeployer(mContext, this);

        FileComponentContext.getInstance().setContext(mContext);
        FileComponentContext.getInstance().setAssociatedLifeCycle(this);

        /**
         * Initializes the MBeans associated with this Binding Component.  Currently,
         * two MBeans are created for use with this Bindng Component:
         * 1. StatusProviderMBean - Provides status about this Binding Component
         * 2. RuntimeConfigurationMBean - Provides configurability for this Binding
         *    component
         * 3. 
         */
        MBeanServer mbServer = jbiContext.getMBeanServer();
        String componentName = jbiContext.getComponentName();
        MBeanNames mbnHndl = jbiContext.getMBeanNames();

        try {
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    statusMBeanObjName,
                    mbServer);
            mStatusProviderHelper.registerMBean(FILEBC_PERF_CATEGORIES, new FileBCPerformanceMeasurement());

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "FBLC_Register_mbean", componentName);
            }
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00102.Mbean_register_failed_status_provider_exception", ex.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00102");
            throw new JBIException(msg, ex);
        }
        try {
            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(mContext);

            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), keystoreUtil);
            mRuntimeConfigHelper =
                    new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    componentName,
                    mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "FBLC_Register_config_mbean", componentName);
            }
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00103.Mbean_register_failed_config_exception", ex.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg, componentName,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00103");
            throw new JBIException(msg, ex);
        }

        try {
            ObjectName managementMBeanObjName = mbnHndl.createCustomComponentMBeanName("Administration");
            mManagementMBean = new FileBCManagement(this);
            mManagementMBeanHelper = new FileBCManagementMBeanHelper(managementMBeanObjName, jbiContext.getMBeanServer());
            mManagementMBeanHelper.registerMBean(mManagementMBean);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered management MBean for " + jbiContext.getComponentName() + " with name " + managementMBeanObjName);
            }

            FileComponentContext.getInstance().setManagementMBean(mManagementMBean);
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-W00126.Exception_during_management_mbean_register", ex.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg,
                    SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-W00126");
            throw new JBIException(msg, ex);
        }

        try {
            mChannel = new BaseMessagingChannel(mContext);
            FileComponentContext.getInstance().setBindingChannel(mChannel);

        } catch (MessagingException me) {
            String msg = mMessages.getString("FILEBC-E00104.Delivery_channel_acquire_exception", me.getLocalizedMessage());
            mLogger.log(Level.SEVERE, msg, me);
            AlertsUtil.getAlerter().critical(msg,
                    componentName,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00104");
            throw me;
        }
    }

    public void start() throws JBIException {
        try {
            startOutbound();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00105.Start_receiver_outbound_failed_exception", ex.getMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00105");
            throw new JBIException(msg, ex);
        }

        try {
            startInbound();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00107.Start_receiver_inbound_failed_exception", ex.getMessage());
            mLogger.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00107");
            throw new JBIException(msg, ex);
        }

        if (mLogger.isLoggable(Level.INFO)) {
            logComponentInfo();
        }
    }

    public void stop() throws JBIException {
        try {
            stopOutbound();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00108.Stop_outbound_receiver_failed_exception", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00108");
            throw new JBIException(msg, ex);
        }

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00103.Stop_component", mContext.getComponentName());
        }
    }

    public void shutDown() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00104.Shutdown_component", mContext.getComponentName());
        }

        if (mChannel != null) {
            mChannel.close();
        }

        /**
         * Shut down the MBeans associated with this Binding Component.
         */
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00109.Mbean_deregister_failed_status_provider_exception", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00109");
            throw new JBIException(msg, ex);
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-E00110.Mbean_deregister_failed_config_exception", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00110");
            throw new JBIException(msg, ex);
        }

        try {
            mManagementMBeanHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FILEBC-W00127.Exception_during_management_mbean_deregister", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                    SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-W00127");
            throw new JBIException(msg, ex);
        }



        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00105.Shutdown_component_done", mContext.getComponentName());
        }
    }

    /****************************************************************
     * File Binding Component specific methods
     ****************************************************************/
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }

    protected void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext,
                mChannel,
                mRuntimeConfig);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00106.Start_component", mContext.getComponentName());
        }
    }

    protected void startOutbound() throws JBIException {
        mOutboundReceiver = new Receiver(mChannel,
                mDeployer.getServiceUnits(),
                mRuntimeConfig);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00107.Start_component", mContext.getComponentName());
        }
    }

    protected void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-R00108.Stop_component", mContext.getComponentName());
        }
    }

    public boolean suspendInboundEndpoint(String endpointName) {
        return mInboundReceiver.suspend(endpointName);
    }

    public boolean resumeInboundEndpoint(String endpointName) throws MessagingException, IBProcCreationException, FaultException {
        return mInboundReceiver.resume(endpointName);
    }

    public boolean isInboundEndpointActive(String endpointName) {
        return mInboundReceiver.isEndpointActive(endpointName);
    }

    public Set<String> getActiveInboundEndpointNames() {
        return mInboundReceiver.getActiveEndpointNames();
    }

    public Set<String> getInActiveInboundEndpointNames() {
        return mInboundReceiver.getInActiveEndpointNames();
    }

    public InboundReceiver getInboundReceiver() {
        return mInboundReceiver;
    }

    public Receiver getOutboundReceiver() {
        return mOutboundReceiver;
    }

    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return mRuntimeConfig;
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setServiceUnitManager(FileBindingDeployer deployer) {
        mDeployer = deployer;
    }

    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString(
                "FILEBC-R00109.Start_component_done_detailed",
                mContext.getComponentName()));
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
        String specVersion = "unknown";
        String buildNumber = "unknown";
        File desc = new File(JBI_XML_PATH);
        try {
            Document document = mDocumentBuilder.parse(desc);
            NodeList versioninfoNodes = document.getElementsByTagNameNS(JBI_DESC_ID_NS, JBI_VERSIONINFO_TAG);
            for (int i = 0; i < versioninfoNodes.getLength(); ++i) {
                Node versioninfoNode = versioninfoNodes.item(i);
                if (versioninfoNode.hasAttributes()) {
                    NamedNodeMap attrMap = versioninfoNode.getAttributes();
                    Node specVerAttrNode = attrMap.getNamedItem(JBI_COMPONENTVER_ATTR);
                    if (specVerAttrNode != null && specVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        specVersion = specVerAttrNode.getNodeValue();
                    }
                    Node buildVerAttrNode = attrMap.getNamedItem(JBI_BUILDVER_ATTR);
                    if (buildVerAttrNode != null && buildVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        buildNumber = buildVerAttrNode.getNodeValue();
                    }
                    break;
                }
            }
        } catch (SAXException e) {
            String msg = mMessages.getString("FILEBC-R00110.Jbi_xml_parse_error_ident",
                    new Object[]{JBI_XML_PATH, e.getLocalizedMessage()});
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-R00110");
        } catch (IOException e) {
            String msg = mMessages.getString("FILEBC-R00111.Jbi_xml_io_error_ident",
                    new Object[]{JBI_XML_PATH, e.getLocalizedMessage()});
            mLogger.log(Level.WARNING, msg, e);
        }
        buf.append(JBI_COMPONENTVER_ATTR + ": ").append(specVersion).append('\n');
        buf.append(JBI_BUILDVER_ATTR + ": ").append(buildNumber).append('\n');
    }
}
