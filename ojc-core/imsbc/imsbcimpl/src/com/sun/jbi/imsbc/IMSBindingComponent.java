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

package com.sun.jbi.imsbc;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.io.File;
import java.io.IOException;

import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.jbi.management.MBeanNames;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.Component;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.JBIException;
import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.imsbc.mbeans.IMSManagement;
import com.sun.jbi.imsbc.util.AlertsUtil;

/**
 * IMSBindingComponent defines a IMS JBI Component and ComponentLifeCycle.  This class
 * implements the main interfaces with the JBI framework.  In addition to implementing
 * the Component and ComponentLifeCycle interfaces, this class creates MBeans used for
 * monitoring and management, and it creates an initial thread to handle messages from
 * the Normalized Message Router (NMR).
 *
 * @author Sun Microsystems
 */
public class IMSBindingComponent implements ComponentLifeCycle, Component {

    private static final Messages mMessages = Messages.getMessages(IMSBindingComponent.class);

    private static Logger mLogger = null;

    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = "IMS BC";

    private ComponentContext mContext ;

    private OutboundReceiver mOutboundReceiver;

    private ObjectName mExtensionMBeanName;

    private MessagingChannel mChannel;

    private IMSBindingDeployer mDeployer;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfiguration mRuntimeConfig;

    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    private boolean mBCStarted = false;

    private InboundReceiver mInboundReceiver;

    private DocumentBuilder mDocumentBuilder;

    private KeyStoreUtilClient mKeySupport;

	private ObjectName mManagementMBeanObjName;

	private IMSManagement mManagementMBean;
    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION   = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String [] IMS_PERF_CATEGORIES = 
        new String[] {PERF_CAT_NORMALIZATION,
                      PERF_CAT_DENORMALIZATION};

    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification"; // NO i18n

    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n

    private static final String JBI_COMPVER_ATTR = "component-version"; // NO i18n

    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n


    
    
    public IMSBindingComponent(){
        DocumentBuilderFactory aFactory = DocumentBuilderFactory.newInstance();
        aFactory.setIgnoringComments(true);
        aFactory.setNamespaceAware(true);
        try {
            mDocumentBuilder = aFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        }
    }
    
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
        mKeySupport = new KeyStoreUtilClient(jbiContext);
        Messages.registerContext(jbiContext);        
        mLogger = Messages.getLogger(IMSBindingComponent.class);
        IMSComponentContext.getInstance().setContext(mContext);
        IMSComponentContext.getInstance().setAssociatedLifeCycle(this);
        initMBeans();

        try {
            mChannel = new BaseMessagingChannel(mContext, true);
            IMSComponentContext.getInstance().setBindingChannel(mChannel);
        } catch (MessagingException me) {
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00101.IMLC_No_Dev_Channel", me.getMessage()));
            throw me;
        }

        mDeployer = new IMSBindingDeployer(mContext, mStatusProviderHelper, mRuntimeConfig);


        //start Management MBean
        try {
            mManagementMBeanObjName =  mContext.getMBeanNames().createCustomComponentMBeanName("Administration");
            mManagementMBean = new IMSManagement(mDeployer.getServiceUnits());
            MBeanServer server = mContext.getMBeanServer();
            if(server.isRegistered(mManagementMBeanObjName) == false) {
            	server.registerMBean(mManagementMBean, mManagementMBeanObjName);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IMSBC-R00125.IMLC_Management_Mbean_Registered" , 
										new Object[]{mContext.getComponentName(), mManagementMBeanObjName}));
            }
         } catch(Exception e){
			String errMsg = mMessages.getString("IMSBC-E00123.IMLC_Management_Mbean_Register_Failed", e.getMessage());
			mLogger.log(Level.SEVERE, errMsg);
            AlertsUtil.getAlerter().critical(errMsg, 
											SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00115");  
            throw new JBIException(errMsg);                        
         }
    }

    public void stop() throws JBIException {
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00103.IMLC_Stop_IMSBC"));

        try {
            stopOutbound();
            AlertsUtil.getAlerter().info(mMessages.getString("IMSBC-R00107.IMLC_Stopped_IMSBC"), 
                                         SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "IMSBC-R00107"); 
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00104.IMLC_Failed_Stop_Outbound", ex.getMessage()), ex);

            AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-E00104.IMLC_Failed_Stop_Outbound",
										 ex.getLocalizedMessage()),
                                         SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "IMSBC-E00104"); 
 
            throw new JBIException(mMessages.getString("IMSBC-E00104.IMLC_Failed_Stop_Outbound", ex.getMessage()), ex);
        }

        mBCStarted = false;
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00107.IMLC_Stopped_IMSBC"));

		logComponentInfo();
    }

    public void shutDown() throws JBIException {
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO,  mMessages.getString("IMSBC-R00108.IMLC_Shutdown_IMSBC"));

        if (mChannel != null) {
        	mChannel.close();
        }

        shutdownMBeans();
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00109.IMLC_Shutdown_Done_IMSBC"));
    }

    public void start() throws JBIException {

        try {
            startOutbound();
        } catch (Exception ex) {
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00110.IMLC_Failed_Start_Outbound", ex.getMessage()), ex);

            AlertsUtil.getAlerter().critical( mMessages.getString("IMSBC-E00110.IMLC_Failed_Start_Outbound",
											 ex.getLocalizedMessage()),
                                             SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "IMSBC-E00110");

            throw new JBIException(mMessages.getString("IMSBC-E00110.IMLC_Failed_Start_Outbound", ex.getMessage()), ex);
        }

        mBCStarted = true;
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO,  mMessages.getString("IMSBC-R00111.IMLC_Start_Done_IMSBC"));

		logComponentInfo();
    }


    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString("IMSBC-R00126.IMLC_Binding_Started",
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
        
        AlertsUtil.getAlerter().info(msgBuf.toString(), 
                                     SHORT_DISPLAY_NAME, 
                                     null, 
                                     AlertsUtil.getServerType(),
                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                     NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                     NotificationEvent.EVENT_TYPE_ALERT,
                                     "IMSBC-R00126");                
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
            String msg = mMessages.getString("IMSBC-R00127.IMLC_Jbi_Parse_Error", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                                            SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "IMSBC-R00127");
        } catch (IOException e) {
            String msg = mMessages.getString("IMSBC-R00128.IMLC_Jbi_Read_Error", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                                            SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "IMSBC-R00128");
        }
        buf.append(JBI_COMPVER_ATTR + ": ").append(compVersion).append('\n');
        buf.append(JBI_BUILDVER_ATTR + ": ").append(buildNumber).append('\n');
    }


    ////////
    //
    //  IMSBindingComponent Public Methods
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
		String componentName = mContext.getComponentName();

		try {
            ObjectName statsMbeanName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME,
									statsMbeanName, mbServer);
			mStatusProviderHelper.registerMBean(IMS_PERF_CATEGORIES, new IMSPerformanceMeasurement());
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00112.IMLC_Status_Mbean_Registered", mContext.getComponentName()));

        } catch (Exception ex) {
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00113.IMLC_Status_Mbean_Register_Failed", ex.getMessage()), ex);

            AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-E00113.IMLC_Status_Mbean_Register_Failed",
											ex.getLocalizedMessage()), 
											SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00113");  

            throw new JBIException(mMessages.getString("IMSBC-E00113.IMLC_Status_Mbean_Register_Failed", ex.getMessage()), ex);
        }

		mKeySupport = new KeyStoreUtilClient(mContext);

        try {
            ObjectName configurationMbeanName = mbnHndl.createCustomComponentMBeanName("Configuration");
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), mKeySupport);
            mRuntimeConfig.initialize();
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(configurationMbeanName, mbServer);
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00114.IMLC_Config_Mbean_Registered", mContext.getComponentName()));
        } catch (Exception ex) {
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00115.IMLC_Config_Mbean_Register_Failed", ex.getMessage()), ex);

            AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-E00115.IMLC_Config_Mbean_Register_Failed",
											ex.getLocalizedMessage()), 
											SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00115");  

            throw new JBIException(mMessages.getString("IMSBC-E00115.IMLC_Config_Mbean_Register_Failed", ex.getMessage()), ex);
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
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00116.IMLC_Status_Mbean_Unregister_Failed", ex.getMessage()), ex);

            AlertsUtil.getAlerter().warning(mMessages.getString("IMSBC-E00116.IMLC_Status_Mbean_Unregister_Failed", 
											ex.getLocalizedMessage()),
                                            SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "IMSBC-E00116");

            throw new JBIException(mMessages.getString("IMSBC-E00116.IMLC_Status_Mbean_Unregister_Failed", ex.getMessage()), ex);
        }


        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00117.IMLC_Config_Mbean_Unregister_Failed", ex.getMessage()), ex);

            AlertsUtil.getAlerter().warning(mMessages.getString("IMSBC-E00117.IMLC_Config_Mbean_Unregister_Failed", 
											ex.getLocalizedMessage()),
                                            SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "IMSBC-E00117");

            throw new JBIException(mMessages.getString("IMSBC-E00117.IMLC_Config_Mbean_Unregister_Failed", ex.getMessage()), ex);
        }

        try {
            MBeanServer server = mContext.getMBeanServer();
            if(server.isRegistered(mManagementMBeanObjName) == true) {
            	server.unregisterMBean(mManagementMBeanObjName);
            }
        } catch(Exception ex){
			mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00124.IMLC_Management_Mbean_Unregister_Failed", ex.getMessage()), ex);

            AlertsUtil.getAlerter().warning(mMessages.getString("IMSBC-E00124.IMLC_Management_Mbean_Unregister_Failed", 
											ex.getLocalizedMessage()),
                                            SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "IMSBC-E00124");

            throw new JBIException(mMessages.getString("IMSBC-E00124.IMLC_Management_Mbean_Unregister_Failed", ex.getMessage()), ex);                        
        }

    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR).
     * In the case of IMS, this thread will handle all messages from the NMR and
     * delegate them to worker threads.  These threads will be responsible for binding
     * the normalized messages to the IMS-specific protocol and handling the message
     * delivery.
     *
     * @exception    JBIException if any error occurs
     */
    private void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mContext, mChannel, mDeployer.getServiceUnits(), 
									mRuntimeConfig);
		new Thread(mOutboundReceiver).start();
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00118.IMLC_Outbound_Started"));
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
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00119.IMLC_Outbound_Stopped"));
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setServiceUnitManager(IMSBindingDeployer deployer) {
        mDeployer = deployer;
    }
}
