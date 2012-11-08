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
 * @(#)ExecBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.execbc.util.ReadWriteTextFile;
import com.sun.jbi.internationalization.Messages;
import java.io.File;
import java.io.IOException;

import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;

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
public class ExecBindingLifeCycle implements ComponentLifeCycle, Component {
    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification"; // NO i18n
    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n
    private static final String JBI_SPECVER_ATTR = "specification-version"; // NO i18n
    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n
    
    private static final Messages mMessages = Messages.getMessages(ExecBindingLifeCycle.class);
    private static Logger mLogger;
    
    public static final String SHORT_DISPLAY_NAME = mMessages.getString("Exec_BC");; //NOI18N

    private DocumentBuilder mDocumentBuilder;
    private ComponentContext mContext;
    private ObjectName mExtensionMBeanName;
    private DeliveryChannel mChannel;
    private ExecBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private InboundReceiver mInboundReceiver;
    private Receiver mOutboundReceiver;
    
    /**
     * Default constructor
     */
    public ExecBindingLifeCycle() {
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
            throw new JBIException(mMessages.getString("FBLC_Null_context"));
        }
        mContext = jbiContext;
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(ExecBindingLifeCycle.class);
        mDeployer = new ExecBindingDeployer(mContext, this);
        
        /**
         * Initializes the MBeans associated with this Binding Component.  Currently,
         * two MBeans are created for use with this Bindng Component:
         * 1. StatusProviderMBean - Provides status about this Binding Component
         * 2. RuntimeConfigurationMBean - Provides configurability for this Binding
         *    component
         */
        MBeanServer mbServer = jbiContext.getMBeanServer();
        String componentName = jbiContext.getComponentName();
        
        try {
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_BINDING,
                    componentName,
                    mbServer);
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "FBLC_Register_mbean", componentName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "FBLC_Failed_register_mbean", ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_register_mbean"), ex);
        }
        
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
                mLogger.log(Level.INFO, "FBLC_Register_config_mbean", componentName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("FBLC_Failed_register_config_mbean"), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_register_config_mbean"), ex);
        }
        
        try {
            mChannel = mContext.getDeliveryChannel();
        } catch(MessagingException me) {
            mLogger.log(Level.SEVERE, "FBLC_No_Dev_Channel", me.getMessage());
            throw me;
        }
    }
    
    public void start() throws JBIException {

        try {
            startOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FBLC_Failed_start_outbound",
                    ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_start_outbound",
                    ex.getMessage()), ex);
        }
        
        try {
            startInbound();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FBLC_Failed_start_inbound",
                    ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_start_inbound",
                    ex.getMessage()), ex);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            logComponentInfo();
        }
    }
    
    public void stop() throws JBIException {

        try {
            stopOutbound();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("FBLC_Failed_stop_outbound", ex.getMessage()), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_stop_outbound", ex.getMessage()), ex);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.info("FBLC_ExecBC_stopped");
        }
    }
    
    public void shutDown() throws JBIException {
        mLogger.info("FBLC_Shutdown_ExecBC");
        
        if(mChannel != null) {
            mChannel.close();
        }
        
        /**
         * Shut down the MBeans associated with this Binding Component.
         */
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("FBLC_Failed_unregister_mbean",
                    mContext.getComponentName()), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_unregister_mbean",
                    mContext.getComponentName()), ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("FBLC_Failed_unregister_config_mbean",
                    mContext.getComponentName()), ex);
            throw new JBIException(mMessages.getString("FBLC_Failed_unregister_config_mbean",
                    mContext.getComponentName()), ex);
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.info("FBLC_Complete_ExecBC_shutdown");
        }
    }
    
    /****************************************************************
     * Exec Binding Component specific methods
     ****************************************************************/
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }
    
    protected void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mContext,
                mChannel,
                mRuntimeConfig);
        mLogger.info("FBLC_started_inbound");
    }
    
    protected void startOutbound() throws JBIException {
        mOutboundReceiver = new Receiver(mChannel,
                mDeployer.getServiceUnits(),
                mRuntimeConfig);
        mLogger.info("FBLC_started_outbound");
    }
    
    protected void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }
        mLogger.info("FBLC_stopped_outbound");
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
    void setServiceUnitManager(ExecBindingDeployer deployer) {
        mDeployer = deployer;
    }
    
    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString(
                    "FBLC_Component_started_detailed",
                    mContext.getComponentName()));
        msgBuf.append('\n');
        
        // Identification information
        String installRoot = mContext.getInstallRoot();
        identity(msgBuf, installRoot);
        
        // Runtime configuration
        mRuntimeConfig.dump(msgBuf);
        mLogger.log(Level.INFO, msgBuf.toString());
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
                    Node specVerAttrNode = attrMap.getNamedItem(JBI_SPECVER_ATTR);
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
            String msg = mMessages.getString("FBLC_Parse_error_ident_info_jbi_xml", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
        } catch (IOException e) {
            String msg = mMessages.getString("FBLC_IO_error_ident_info_jbi_xml", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
        }
        buf.append("specification-version: ").append(specVersion).append('\n');
        buf.append("build-number: ").append(buildNumber).append('\n');
    }
}
