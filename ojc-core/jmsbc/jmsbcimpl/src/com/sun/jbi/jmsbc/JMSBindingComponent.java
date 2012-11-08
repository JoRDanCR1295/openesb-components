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
 * @(#)JMSBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.transaction.TransactionManager;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.stc.jmsjca.core.TxMgr;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.jms.ChannelManagerImpl;
import com.sun.jbi.jmsbc.mbeans.JMSBCManagement;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfiguration;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.jmsbc.recovery.JMSConnectionInfoFilePersister;
import com.sun.jbi.jmsbc.recovery.JMSXARecovery;
import com.sun.jbi.jmsbc.recovery.JMSXARecoveryHelper;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.util.Executor;
import com.sun.jbi.jmsbc.util.JMSBCContext;



/**
 * JMSBindingComponent defines a JMS JBI Component and ComponentLifeCycle.  This class
 * implements the main interfaces with the JBI framework.  In addition to implementing
 * the Component and ComponentLifeCycle interfaces, this class creates MBeans used for
 * monitoring and management, and it creates an initial thread to handle messages from
 * the Normalized Message Router (NMR).
 * 
 */
public class JMSBindingComponent
        implements ComponentLifeCycle, Component {
    
    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification/v1.0"; // NO i18n
    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n
    private static final String JBI_COMPONENTVER_ATTR = "component-version"; // NO i18n
    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n

    private static final Messages mMessages = 
        Messages.getMessages(JMSBindingComponent.class);
    private static Logger mLogger = null;
    
    /**
     * Display name for this Binding Component
     */
    public static final String SHORT_DISPLAY_NAME = "JMS BC";
    
    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION   = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String [] JMSBC_PERF_CATEGORIES = 
        new String[] {PERF_CAT_NORMALIZATION,
                      PERF_CAT_DENORMALIZATION};
    
    private DocumentBuilder mDocumentBuilder;
    private ComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;

    private ObjectName mExtensionMBeanName;
    private JMSBindingDeployer mDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private JMSBCRuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;    
    private ChannelManager mJMSChannelMgr;
    private boolean mBCStarted = false;
    private Map mInboundMessageExchanges;
    
    private JMSXARecovery mRecovery;
    private JMSXARecoveryHelper mRecoveryHelper;
    private ConnectionInfoPersister mConnPersister;
    private ObjectName mManagementMBeanObjName;
    private JMSBCManagement mManagementMBean;
    
    // To be removed when "environment variables" is approved and implemented
    public static boolean ALLOW_CRASH_ON_COMMIT = false;

    
    //For testing
    private static final boolean TEST_ENV; 
	static{
		String str = System.getProperty("com.sun.jbi.framework.osgi.install.TestTransactionManager"); 
		if (str != null && str.equalsIgnoreCase("true")) {
			TEST_ENV = true;
		}else{
			TEST_ENV = false;
		}
	}

    
    public JMSBindingComponent() {
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
        Document result = null;
        Iterator serviceUnits =
            mDeployer.getServiceUnits().iterator();
        while (serviceUnits.hasNext()) {
            Iterator endpoints =
                ((ServiceUnit)serviceUnits.next()).getEndpoints().iterator();
            while(endpoints.hasNext()) {
                Endpoint endpoint = (Endpoint)endpoints.next();
                if (serviceEndpoint.getServiceName().equals(endpoint.getServiceName())
                    && serviceEndpoint.getEndpointName().equals(endpoint.getEndpointName())
                    && endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {

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
        return mExtensionMBeanName;
    }
    
    public void init(ComponentContext jbiContext) throws JBIException {
        mContext = jbiContext;
        
        //For testing
        if(TEST_ENV){
        	TxMgr.setUnitTestTxMgr((TransactionManager)mContext.getTransactionManager());
        }
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(JMSBindingComponent.class);        
        initMBeans();

        JMSBCContext.getRef().setContext(mContext);
        // Create message exchange maps
        mInboundMessageExchanges = Collections.synchronizedMap(new HashMap());
        
        mJMSChannelMgr = new ChannelManagerImpl(mContext,
                                                mInboundMessageExchanges);
                                
        // To be removed when "environment variables" is approved and implemented
        ALLOW_CRASH_ON_COMMIT = new java.io.File(jbiContext.getWorkspaceRoot() + 
                                                 java.io.File.separator + 
                                                 "com.sun.jbi.jmsbc.allowCrashOnCommit").exists();

        if (ALLOW_CRASH_ON_COMMIT && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log (LogSupport.LEVEL_DEBUG, 
                "****** TESTING MODE ONLY ***** JMS BC is configured for crash test mode on xa commit to test recovery");
        }
        
        if (JMSXARecovery.xaRecoverySupported(jbiContext)) {
            // Needed by registerXAResource
            jbiContext.getDeliveryChannel();
            try {
                Properties persistProps = new Properties();
                persistProps.setProperty(JMSConnectionInfoFilePersister.FILE_PERSISTER_PROPS_DIR,
                                         jbiContext.getWorkspaceRoot());
                mConnPersister = new JMSConnectionInfoFilePersister();
                mConnPersister.initialize(persistProps);

                mRecoveryHelper = new JMSXARecoveryHelper();
                mRecovery = new JMSXARecovery(jbiContext,  
                                              mConnPersister,
                                              mRecoveryHelper);
                
                mRecovery.recover();
            } catch (Throwable t) {
                mLogger.log(Level.WARNING,
                            mMessages.getString("JMSBC-W0703.XARecoveryInitiationFailed",
                                new Object [] {t.getLocalizedMessage()}),
                            t);
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0703.XARecoveryInitiationFailed",
                                new Object [] {t.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0703");                              
            }
        } else {
            mLogger.log(Level.INFO, "JMSBC-I0708.XARecoveryNotInitiated");
        }
        
        mDeployer = new JMSBindingDeployer(mContext,
                                           mStatusProviderHelper,
                                           mJMSChannelMgr,
                                           mConnPersister,
                                           mRuntimeConfig,
                                           mInboundMessageExchanges);        

        //start Management MBean
        try{
            mManagementMBeanObjName =  mContext.getMBeanNames().createCustomComponentMBeanName("Administration");
            mManagementMBean = new JMSBCManagement(mDeployer.getServiceUnits());
            MBeanServer server = mContext.getMBeanServer();
            if(server.isRegistered(mManagementMBeanObjName) == false) {
            	server.registerMBean(mManagementMBean, mManagementMBeanObjName);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered management MBean for " + mContext.getComponentName() + " with name " + mManagementMBeanObjName);
            }
        }catch(Exception e){
        	  AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-E0762.CouldNotRegisterManagementMBean",
                    new Object[]{mContext.getComponentName()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0762");  
            throw new JBIException(mMessages.getString("JMSBC-E0762.CouldNotRegisterManagementMBean",
                    new Object[]{mContext.getComponentName()}),
                e);                        
        }
        
    }
    
    public void shutDown() throws JBIException {                
        if (mConnPersister != null) {
            try {
                mConnPersister.close();
            } catch (Throwable ex) {
                mLogger.log(Level.WARNING,
                            mMessages.getString("JMSBC-W0702.ConnectionInfoPersisterCloseFailed",
                               new Object[]{ex.getLocalizedMessage()}),
                            ex);
            }
        }
        
        //mContext.getDeliveryChannel().close();
        if(JMSBCContext.getRef().getChannel() != null){
            JMSBCContext.getRef().getChannel().close();
        }
        
        shutdownMBeans();
        
        mLogger.log(Level.INFO, 
                    mMessages.getString("JMSBC-I0705.ComponentLifeCycleShutdown",
                        new Object [] {mContext.getComponentName()}));
    }
    
    public void start() throws JBIException {
        
        if (mRecoveryHelper != null) {
            mRecoveryHelper.closeOpenedResources();
        }
        
        Executor.getReference().start();
        JMSBCContext.getRef().setChannel(new BaseMessagingChannel(mContext));
        
        try {
            startOutbound();
            mBCStarted = true;
        } catch (Exception ex) {
            String errMsg = mMessages.getString("JMSBC-E0719.OutboundReceiverStartFailed");
            throw new JBIException(errMsg, ex);
        }
        logComponentInfo();
    }
    
    public void stop() throws JBIException {
        try {
            stopOutbound();
            mBCStarted = false;
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("JMSBC-E0720.OutboundReceiverStopFailed"));
        }
        Executor.getReference().shutdown();
        mLogger.log(Level.INFO, "JMSBC-I0707.ComponentLifeCycleStop");
    }

    public RuntimeConfigurationMBean getRuntimeConfigurationMBean() {
        return mRuntimeConfig;
    }
    
    ////////
    //
    //  JMSBindingComponent Public Methods
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
        	ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
        	
            mStatusProviderHelper =
                new StatusProviderHelper(SHORT_DISPLAY_NAME,
                                         statusMBeanObjName,
                                         mContext.getMBeanServer());
            mStatusProviderHelper.registerMBean(JMSBC_PERF_CATEGORIES, new JMSBCPerformanceMeasurement());
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "JMSBindingComponent_STATUS_MBEAN_REG_SUCCEEDED",
                            new Object[]{mContext.getComponentName()});
            }
        } catch (Exception ex) {
            AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-E0721.RegisterStatusMBeanFailed",
                                       new Object[]{mContext.getComponentName()}), 
                    SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0721");  
            throw new JBIException(mMessages.getString("JMSBC-E0721.RegisterStatusMBeanFailed",
                                       new Object[]{mContext.getComponentName()}),
                                       ex);            
        }
        
        try {
            mRuntimeConfig = new JMSBCRuntimeConfiguration(mContext.getWorkspaceRoot(), new KeyStoreUtilClient(mContext));
            mRuntimeConfigHelper =
                new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_BINDING,
                    mContext.getComponentName(),
                    mContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "JMSBindingComponent_CONFIG_MBEAN_REG_SUCCEEDED",
                            new Object[]{mContext.getComponentName()});
            }
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("JMSBC-E0722.RegisterConfigurationMBeanFailed",
                                       new Object[]{mContext.getComponentName()}),
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
            throw new JBIException(mMessages.getString("JMSBC-E0723.UnregisterStatusMBeanFailed",
                                       new Object[]{mContext.getComponentName()}),
                                   ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("JMSBC-E0724.UnregisterConfigurationMBeanFailed",
                                       new Object[]{mContext.getComponentName()}),
                                   ex);
        }

        //stop Management MBean
        try{
            MBeanServer server = mContext.getMBeanServer();
            if(server.isRegistered(mManagementMBeanObjName) == true) {
            	server.unregisterMBean(mManagementMBeanObjName);
            }
        }catch(Exception e){
            throw new JBIException(mMessages.getString("JMSBC-E0763.CouldNotUnregisterManagementMBean",
                    new Object[]{mContext.getComponentName()}),
                e);                        
        }
    }

    /**
     * Starts the thread to handle messages from the Normalized Message Router (NMR).
     * In the case of JMS, this thread will handle all messages from the NMR and
     * delegate them to worker threads.  These threads will be responsible for binding
     * the normalized messages to the JMS-specific protocol and handling the message
     * delivery.
     *
     * @exception    JBIException if any error occurs
     */
    private void startOutbound() throws JBIException {
        mOutboundReceiver =
            new OutboundReceiver(mContext,
                                 mDeployer.getServiceUnits(),
                                 mRuntimeConfig.getThreads() == null?0:mRuntimeConfig.getThreads().intValue(), 
                                 mJMSChannelMgr,
                                 mInboundMessageExchanges);
        // Register outbound receiver for changes to the configuration
        mRuntimeConfig.addNotificationListener(mOutboundReceiver.getNotificationListener(), null, null);
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, "JMSBindingComponent_OUTBOUND_STARTED");
        }
    }
    
    /**
     * Shuts down the thread that handles messages from the NMR
     *
     * @exception    JBIException if any error occurs
     */
    private void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
            mOutboundReceiver = null; //Let GC
        }
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(Level.INFO, "JMSBindingComponent_OUTBOUND_STOPPED");
        }
    }
    
    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString(
                    "JMSBC-I0706.ComponentLifeCycleStart",
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
            String msg = mMessages.getString("JMSBC-W0704.JBIDescriptorParseFailed", 
                            new Object [] {JBI_XML_PATH, e.getLocalizedMessage()});
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0704");         
        } catch (IOException e) {       	
            String msg = mMessages.getString("JMSBC-W0705.JBIDescriptorReadError", 
                            new Object [] {JBI_XML_PATH, e.getLocalizedMessage()});
            AlertsUtil.getAlerter().warning(msg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0705");    
            mLogger.log(Level.WARNING, msg, e);
        }
        buf.append(JBI_COMPONENTVER_ATTR + ": ").append(specVersion).append('\n');
        buf.append(JBI_BUILDVER_ATTR + ": ").append(buildNumber).append('\n');
    }
}
