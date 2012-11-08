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
 * @(#)FTPBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.ftpbc.management.FTPBCManagement;
import com.sun.jbi.ftpbc.management.FTPBCManagementMBean;
import com.sun.jbi.ftpbc.management.FTPBCManagementMBeanHelper;
import com.sun.jbi.ftpbc.util.AlertsUtil;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.io.File;
import java.io.IOException;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This class implements the ComponentLifeCycle and Component interfaces.
 * The implementation of the Component interface allows the JBI framework
 * to query the FTP BC component for various types of information.
 * The implementation of the ComponentLifeCycle interface provides initialization,
 * start, stop, and shutdown life cycle processing.
 *
 * Might need to add some external shutdown, cleanup logic in the life cycle methods
 * later when connections are pooled;
 *
 * @author jfu jim.fu@sun.com
 */
public class FTPBindingLifeCycle implements ComponentLifeCycle, Component {
    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification"; // NO i18n
    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n
    private static final String JBI_SPECVER_ATTR = "specification-version"; // NO i18n
    private static final String JBI_COMP_VER_ATTR = "component-version"; // NO i18n
    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n

    private static final Messages mMessages = Messages.getMessages(FTPBindingLifeCycle.class);
    private static Logger mLogger;
    
    public static final String SHORT_DISPLAY_NAME = "sun-ftp-binding";

    static final String PROCESSING_EXTENSION = "ProcessingExtension"; // NOI18N

    public static final String PERF_CAT_NORMALIZATION   = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String [] BC_PERF_CATEGORIES = 
                                       new String[] {PERF_CAT_NORMALIZATION,
                                                     PERF_CAT_DENORMALIZATION};
    
    private DocumentBuilder mDocumentBuilder;
    private ComponentContext mContext;
    private ObjectName mExtensionMBeanName;
    private MessagingChannel mChannel;
    private FTPBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private InboundReceiver mInboundReceiver;
    private Receiver mOutboundReceiver;
    private FTPBCManagementMBean mManagementMBean;
    private FTPBCManagementMBeanHelper mManagementMBeanHelper;
    /**
     * Default constructor
     */
    public FTPBindingLifeCycle() {
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
            for (Iterator endpoints = ((ServiceUnit)serviceUnits.next()).getEndpoints().iterator();
            endpoints.hasNext();) {
                Endpoint endpoint = (Endpoint)endpoints.next();
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName()) &&
                        serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())) {
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
        if (jbiContext ==  null) {
            throw new JBIException(mMessages.getString("FTPBC-E001035.FBLC_Null_context"));
        }
        mContext = jbiContext;
        FTPBCComponentContext.getInstance().setContext(mContext);
        FTPBCComponentContext.getInstance().setAssociatedLifeCycle(this);
        
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(FTPBindingLifeCycle.class);
        mDeployer = new FTPBindingDeployer(mContext, this);
        
        /**
         * Initializes the MBeans associated with this Binding Component.  Currently,
         * two MBeans are created for use with this Bindng Component:
         * 1. StatusProviderMBean - Provides status about this Binding Component
         * 2. RuntimeConfigurationMBean - Provides configurability for this Binding
         *    component
         */
        MBeanServer mbServer = jbiContext.getMBeanServer();
        String componentName = jbiContext.getComponentName();
        MBeanNames mbnHndl = jbiContext.getMBeanNames();
        
        try {
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_BINDING,
                    componentName,
                    mbServer);
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001008.FBLC_Register_mbean", componentName));
            }
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001004.[ALERT].FBLC_Failed_register_mbean");
            AlertsUtil.getAlerter().critical(msg, 
                                             FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-W001004");
            throw new JBIException(msg, ex);
        }
        
        try {
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME, statusMBeanObjName, jbiContext.getMBeanServer());
            mStatusProviderHelper.registerMBean(BC_PERF_CATEGORIES, new FTPBCPerformanceMeasurement());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered Status Provider MBean for " + jbiContext.getComponentName());
            }            
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001011.[ALERT].FBLC_Exception_during_status_mbean_register", ex.getMessage());
            AlertsUtil.getAlerter().critical(msg, 
                                             FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-W001011");
            throw new JBIException(msg, ex);
        }

        try {
            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(mContext);

            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), keystoreUtil);
            mRuntimeConfigHelper =
                    new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    componentName,
                    mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001009.FBLC_Register_config_mbean", componentName));
            }
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001005.[ALERT].FBLC_Failed_register_config_mbean");
            AlertsUtil.getAlerter().critical(msg, 
                                             FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-W001005");
            throw new JBIException(msg, ex);
        }
        
        try {
            ObjectName managementMBeanObjName = mbnHndl.createCustomComponentMBeanName("Administration");
            mManagementMBean = new FTPBCManagement(this);
            mManagementMBeanHelper = new FTPBCManagementMBeanHelper(managementMBeanObjName, jbiContext.getMBeanServer());
            mManagementMBeanHelper.registerMBean(mManagementMBean);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered management MBean for " + jbiContext.getComponentName() + " with name " + managementMBeanObjName);
            }
        } catch (Exception ex) {
            String text = mMessages.getString("FTPBC-W001012.[ALERT].Exception_during_management_mbean_register", ex.getMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-W001012");
            throw new JBIException(text, ex); 
        }
        
    }
    
    public void shutDown() throws JBIException {
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001010.FBLC_Shutdown_FTPBC"));
        
        if(mChannel != null) {
            mChannel.close();
        }
        
        /**
         * Shut down the MBeans associated with this Binding Component.
         */
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001006.[ALERT].FBLC_Failed_unregister_mbean",
                    mContext.getComponentName());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, 
                                            FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FTPBC-W001006");
            throw new JBIException(msg, ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001007.[ALERT].FBLC_Failed_unregister_config_mbean",
                    mContext.getComponentName());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().critical(msg, 
                                            FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FTPBC-W001007");
            throw new JBIException(msg, ex);
        }
        
        try {
            mManagementMBeanHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-W001015.[ALERT].FBLC_Failed_unregister_admin_mbean_exception", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg,
                                             SHORT_DISPLAY_NAME,
                                             null,
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-W001015");
            throw new JBIException(msg, ex);
        }

        String msg = mMessages.getString("FTPBC-R001011.[ALERT].FBLC_Complete_FTPBC_shutdown", mContext.getComponentName());
        // cleanup the connection pool
        AlertsUtil.getAlerter().info(msg, 
                                     FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                     null, 
                                     AlertsUtil.getServerType(),
                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                     NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                     NotificationEvent.EVENT_TYPE_ALERT,
                                     "FTPBC-R001011");        

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, msg);
        }
    }
    
    public void start() throws JBIException {
        try {
            mChannel = new BaseMessagingChannel(mContext);
            FTPBCComponentContext.getInstance().setBindingChannel(mChannel);
        } catch(MessagingException me) {
            String msg = mMessages.getString("FTPBC-E001036.[ALERT].FBLC_No_Dev_Channel", me.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(msg, 
                                             FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "FTPBC-E001036");
            throw new JBIException(msg, me);
        }

        try {
            startOutbound();
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-E001037.[ALERT].FBLC_Failed_start_outbound", ex.getMessage());
            AlertsUtil.getAlerter().critical(msg, 
                                            FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FTPBC-E001037");
            throw new JBIException(msg, ex);
        }
        
        try {
            startInbound();
        } catch (Exception ex) {
            String msg = mMessages.getString("FTPBC-E001038.[ALERT].FBLC_Failed_start_inbound", ex.getMessage());
            AlertsUtil.getAlerter().critical(msg, 
                                            FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FTPBC-E001038");
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
            String msg = mMessages.getString("FTPBC-W001008.[ALERT].FBLC_Failed_stop_outbound", ex.getMessage());
            AlertsUtil.getAlerter().critical(msg, 
                                            FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FTPBC-W001008");
            throw new JBIException(msg, ex);
        }
        
        // assume that all the corresponding SUs are stopped properly
        // so that the connection pools can be shutdown and semaphores registry
        // can be released and destroyed
        RemoteTargetSemaphoreManager.cleanup();

        String msg = mMessages.getString("FTPBC-R001012.[ALERT].FBLC_FTPBC_stopped", mContext.getComponentName());
        // cleanup the connection pool
        AlertsUtil.getAlerter().info(msg, 
                                     FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                     null, 
                                     AlertsUtil.getServerType(),
                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                     NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                     NotificationEvent.EVENT_TYPE_ALERT,
                                     "FTPBC-R001012");        
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, msg);
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
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001013.FBLC_started_inbound"));
    }
    
    protected void startOutbound() throws JBIException {
        mOutboundReceiver = new Receiver(mChannel,
                mDeployer.getServiceUnits(),
                mRuntimeConfig, mManagementMBean);
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001014.FBLC_started_outbound"));
    }
    
    protected void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001015.FBLC_stopped_outbound"));
    }
    
    public InboundReceiver getInboundReceiver() {
        return mInboundReceiver;
    }
    
    public Receiver getOutboundReceiver() {
        return mOutboundReceiver;
    }
    
    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setServiceUnitManager(FTPBindingDeployer deployer) {
        mDeployer = deployer;
    }
    
    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return mRuntimeConfig;
    }
    
    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString("FTPBC-R00199.[ALERT].FBLC_Component_started_detailed",
                    mContext.getComponentName()));
        msgBuf.append('\n');
        
        // Identification information
        String installRoot = mContext.getInstallRoot();
        identity(msgBuf, installRoot);
        

        // Runtime configuration
        mRuntimeConfig.dump(msgBuf);

        AlertsUtil.getAlerter().info(msgBuf.toString(), 
                                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                        null, 
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "FTPBC-R00199");
        
        if ( mLogger.isLoggable(Level.INFO) )
            mLogger.log(Level.INFO, msgBuf.toString());
    }
    
    private void identity(StringBuffer buf, String idDescPath) {
        final String JBI_XML_PATH = idDescPath.concat("/META-INF/jbi.xml");
        String specVersion = "unknown";
        String buildNumber = "unknown";
        String compVersion = "unknown";
        File desc = new File(JBI_XML_PATH);
        try {
            Document document = mDocumentBuilder.parse(desc);
            NodeList versioninfoNodes = document.getElementsByTagNameNS(JBI_DESC_ID_NS, JBI_VERSIONINFO_TAG);
            for (int i = 0; i < versioninfoNodes.getLength(); ++i) {
                Node versioninfoNode = versioninfoNodes.item(i);
                if (versioninfoNode.hasAttributes()) {
                    NamedNodeMap attrMap = versioninfoNode.getAttributes();
                    Node specVerAttrNode = attrMap.getNamedItem(JBI_SPECVER_ATTR);
                    if (specVerAttrNode != null && specVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        specVersion = specVerAttrNode.getNodeValue();
                    }
                    Node compVerAttrNode = attrMap.getNamedItem(JBI_COMP_VER_ATTR);
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
            if ( mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001009.FBLC_Parse_error_ident_info_jbi_xml", new String[] {JBI_XML_PATH, e.getLocalizedMessage()}));
        } catch (IOException e) {
            if ( mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001010.FBLC_IO_error_ident_info_jbi_xml", new String[] {JBI_XML_PATH, e.getLocalizedMessage()}));
        }
        buf.append("specification-version: ").append(specVersion).append('\n');
        buf.append("component-version: ").append(compVersion).append('\n');
        buf.append("build-number: ").append(buildNumber).append('\n');
    }
    
    // added methods for management
    public boolean suspendActivatedEndpoint(String epName) {
        return mInboundReceiver.suspendProcessor(epName);
    }
    
    public boolean resumeActivatedEndpoint(String epName) {
        return mInboundReceiver.resumeProcessor(epName);
    }
    
    // the following methods left not implemented
    public boolean isEndpointActive(String epName) {
        return true;
    }
    
    public String[] getActiveConsumingEndpoints() {
        return null;
    }
    
    public String[] getInactiveConsumingEndpoints() {
        return null;
    }
}
