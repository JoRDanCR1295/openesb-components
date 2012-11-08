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
 * @(#)ExtendedComponentContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.sql.Connection;
import java.util.Properties;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.management.AttributeChangeNotification;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.MBeanServer;
import javax.management.JMException;
import javax.management.StandardMBean;
import javax.management.ObjectName;
import javax.naming.InitialContext;
import javax.transaction.TransactionManager;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.engine.iep.admin.IEPAdminMBean;
import com.sun.jbi.engine.iep.admin.IEPAdminMBeanImpl;
import com.sun.jbi.engine.iep.admin.IEPAdminMbeanRegister;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.DefaultIEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStream;
import com.sun.jbi.engine.iep.core.runtime.rmi.IEPEngineRmiRegistry;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.internationalization.Messages;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchangeFactory;

/**
 * ExtendedComponentContext.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class ExtendedComponentContext implements IEPConfig {

    private static final Messages mMessages = Messages.getMessages(ExtendedComponentContext.class);
    private static final Logger mLogger = Messages.getLogger(ExtendedComponentContext.class);
    public static final String ADMINISTRATION_KEY = "Administration";
    public static final String CONFIGURATION_KEY = "Configuration";
    private IEPEngine mEngine;
    private ComponentContext mContext;
    private InitialContext mInitialContext;
    private BaseMessagingChannel mChannel;
    private DeploymentLookup mDeploymentLookup;
    private IEPSEHeartBeatThread mHeartBeatThread;
    private IEPSEInOnlyThread mInOnlyThread;
    private IEPSEOutOnlyThread mOutOnlyThread;
    private DeploymentTable mDeploymentTable;
    private ExchangeTable mExchangeTable;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private Properties mConfigProp;
    private Properties mUpdatedConfigProp;
    private Properties mBootstrapProp;
    private IEPSERuntimeConfiguration mConfigMBean;
    private ScheduledExecutorService mPollingTableExecuterService;
    private ScheduledExecutorService mReplayExecuterService;
    private Map<String, List<FutureInfo>> mInstanceIdToSchedulePollingTableStreamMap = new HashMap<String, List<FutureInfo>>();
    private Map<String, List<FutureInfo>> mInstanceIdToReplayStreamMap = new HashMap<String, List<FutureInfo>>();    
    
    DeliveryChannel mTempChannel;
    MessageExchangeFactory mTempMEFactory;
    
    private NotificationListener mListener = new NotificationListener() {

        public void handleNotification(Notification notification, Object obj) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "IEPSELifeCycle.Handling_notification");
            }

            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.Getting_notification_attribute", new Object[]{attrName}));
                }
            }
        }
    };

    /** Creates a new instance of ExtendedComponentContext */
    public ExtendedComponentContext() {
    }

    public void init(ComponentContext context) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Initializing_IEP_ExtendedComponentContext"));
        }
        com.sun.jbi.engine.iep.core.runtime.util.Messages.setLogger(Messages.getLogger(IEPEngine.class));
        com.sun.jbi.engine.iep.core.runtime.operator.WebServiceContext.setInvocationFactory(new IEPSEWebServiceInvocationFactory(this));
        mContext = context;
        mInitialContext = context.getNamingContext();
        mChannel = new BaseMessagingChannel(context);
        mDeploymentLookup = new DeploymentLookup(context);

        // See "Prepare Installation Context" section in JBI 1.0 pr spec 6.4.2.1.1
        loadConfigProperties();

        String aieType = PropertyUtil.getString(mConfigProp, PROP_AIE_TYPE, "none");
        String aieHostname = PropertyUtil.getString(mConfigProp, PROP_AIE_HOSTNAME, "localhost");
        int aiePort = PropertyUtil.getint(mConfigProp, PROP_AIE_PORT, 6688);
        String aieId = PropertyUtil.getString(mConfigProp, PROP_AIE_ID, "aie1");
        if (AIE_TYPE_NONE.equals(aieType)) {
            mEngine = DefaultIEPEngine.getInstance();
        } else {
            Registry rmiReg = LocateRegistry.getRegistry(aieHostname, aiePort);
            IEPEngineRmiRegistry iepReg = (IEPEngineRmiRegistry) rmiReg.lookup(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME);
            if (iepReg.hasEngine(aieId)) {
                // Should never happen
                mEngine = iepReg.getEngine(aieId);
            } else {
                mEngine = iepReg.createEngine(aieId);
            }
        }
        mExchangeTable = new ExchangeTable();

        mDeploymentTable = new DeploymentTable();

        try {
            String configSchema = "";
            String configSchemaFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentDescription.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            if (configSchemaFile.exists()) {
                configSchema = IOUtil.getText(configSchemaFileLoc, "UTF-8");
            }

            String configData = "";
            String configDataFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            if (configDataFile.exists()) {
                configData = IOUtil.getText(configDataFileLoc, "UTF-8");
            }

            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE, context.getComponentName(), context.getMBeanServer());
            String workspaceRoot = mContext.getWorkspaceRoot();
            File updatedConfigFile = new File(workspaceRoot + File.separator + PROP_UPDATED_CONFIG_FILE);
            mConfigMBean = new IEPSERuntimeConfiguration(updatedConfigFile, configSchema, configData);
            registerRuntimeConfigurationMBeans(mContext, mConfigMBean);
            registerAdministrationMBeans(mContext);
        } catch (NotCompliantMBeanException ex) {
            throw new JBIException(mMessages.getString("ExtendedComponentContext.Unable_to_create_runtime_configuration_mbean"), ex);
        } catch (MalformedObjectNameException ex) {
            throw new JBIException(mMessages.getString("ExtendedComponentContext.Unable_to_create_runtime_configuration_helper"), ex);
        } 
        mConfigMBean.addNotificationListener(mListener, null, null);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Initialized_IEP_ExtendedComponentContext_successfully"));
        }

        mTempChannel = mContext.getDeliveryChannel();
        mTempMEFactory = mTempChannel.createExchangeFactory();
    }

    private void registerRuntimeConfigurationMBeans(ComponentContext jbiContext, IEPSERuntimeConfiguration configBean) throws JBIException {
        MBeanServer mbServer = jbiContext.getMBeanServer();
        MBeanNames mbnHndl = jbiContext.getMBeanNames();
        ObjectName mbn = mbnHndl.createCustomComponentMBeanName(CONFIGURATION_KEY);
        try {
            StandardMBean extensionBean = new StandardMBean(configBean, IEPSERuntimeConfigurationMBean.class);
            if (!mbServer.isRegistered(mbn)) {
                mbServer.registerMBean(extensionBean, mbn);
                if (mLogger.isLoggable(Level.FINE)) {

                    mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Registered_extension_MBean_with_name", mbn));
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Skipping_registration_of_Extension_MBean", mbn));
                }
            }
        } catch (JMException ex) {
            String text = mMessages.getString("ExtendedComponentContext.Exception_during_extension_mbean_register", ex.getMessage());
            throw new JBIException(text, ex);
        }
    }
    private IEPAdminMbeanRegister mManagementMbeanRegister = null;
    private StandardMBean adminBean = null;

    private void registerAdministrationMBeans(ComponentContext jbiContext) throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Registering_Administartion_bean"));
        }

        try {
            mManagementMbeanRegister = new IEPAdminMbeanRegister(jbiContext.getMBeanNames().createCustomComponentMBeanName(ADMINISTRATION_KEY),
                    jbiContext.getMBeanServer());
            adminBean = new StandardMBean(new IEPAdminMBeanImpl(this), IEPAdminMBean.class);
            mManagementMbeanRegister.registerMBean(adminBean);
        } catch (JMException ex) {
            String text = mMessages.getString("ExtendedComponentContext.Exception_during_extension_mbean_register", ex.getMessage());
            throw new JBIException(text, ex);
        }
    /*MBeanServer mbServer = jbiContext.getMBeanServer();
    MBeanNames mbnHndl = jbiContext.getMBeanNames();
    ObjectName mbn = mbnHndl.createCustomComponentMBeanName(ADMINISTRATION_KEY);
    adminBeanName = mbn;
    
    try {
    StandardMBean extensionBean = new StandardMBean(new IEPAdminMBeanImpl(this), IEPAdminMBean.class);
    adminBean = extensionBean;
    if (!mbServer.isRegistered(mbn)) {
    mbServer.registerMBean(extensionBean, mbn);
    if (mLogger.isLoggable(Level.FINE)) {
    
    mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Registered_extension_MBean_with_name"
    , mbn));
    }
    } else {
    if (mLogger.isLoggable(Level.FINE)) {
    mLogger.log(Level.FINE, mMessages.getString("ExtendedComponentContext.Skipping_registration_of_Extension_MBean"
    , mbn));
    }
    }
    } catch (JMException ex) {
    String text = mMessages.getString("ExtendedComponentContext.Exception_during_extension_mbean_register", ex.getMessage());
    throw new JBIException(text, ex);
    }
     */
    }

    public void startIOThreads() throws Exception {
        if (mInOnlyThread == null) {
            mInOnlyThread = new IEPSEInOnlyThread(this);
            mInOnlyThread.start();
        }
        if (mOutOnlyThread == null) {
            mOutOnlyThread = new IEPSEOutOnlyThread(this);
            mOutOnlyThread.start();
        }

        startPollingTable();
        startReplayStream();

    }

    public void startHeartBeatThread() {
        if (mHeartBeatThread == null) {
            mHeartBeatThread = new IEPSEHeartBeatThread(this);
            mHeartBeatThread.start();
        }
    }

    public void stopIOThreads() throws Exception {
        if (mOutOnlyThread != null) {
            mOutOnlyThread.stopAndWait();
            mOutOnlyThread = null;
        }
        if (mInOnlyThread != null) {
            mInOnlyThread.stopAndWait();
            mInOnlyThread = null;
        }

        if (mPollingTableExecuterService != null) {
            mPollingTableExecuterService.shutdownNow();
            mPollingTableExecuterService = null;
        }
        
        if (mReplayExecuterService != null) {
            mReplayExecuterService.shutdownNow();
            mReplayExecuterService = null;
        }
        

    }

    public void stopHeartBeatThread() throws Exception {
        if (mHeartBeatThread != null) {
            mHeartBeatThread.stopAndWait();
            mHeartBeatThread = null;
        }
    }

    public void destory() throws Exception {
        boolean ignorePendingRequests = true;

        if (mChannel != null) {
            mChannel.close();
        }

        if (mTempChannel != null) {
            mTempChannel.close();
        }
        
        mEngine.stopAll();
        mEngine.unregister();

        unregisterRuntimeConfigurationMBean();
        unregisterAdminMBean();
    }

    public ComponentContext getComponentContext() {
        return mContext;
    }

    public Properties getConfigProperties() {
        return mConfigProp;
    }

    public void loadConfigProperties() throws Exception {
        mConfigProp = new Properties();
        String workspaceRoot = mContext.getWorkspaceRoot();
        File configFile = new File(workspaceRoot + File.separator + PROP_CONFIG_FILE);
        mConfigProp.load(new FileInputStream(configFile));
        mConfigProp.put(PROP_DB_JNDI_CONTEXT, mInitialContext);
        
        //Default
        String instanceName = "stand-alone";
        if(Boolean.getBoolean(GF_IS_CLUSTERED)) {
        	//Means iep engine is running in GlassFish cluster
            instanceName = System.getProperty(GF_INSTANCE_NAME);
            if(instanceName == null || instanceName.equals("")) {
            	throw new RuntimeException(mMessages.getString(
            			"ExtendedComponentContext.Cluster_instance_name_not_specified", GF_INSTANCE_NAME));
            }
            mConfigProp.put(PROP_IS_CLUSTERED, System.getProperty(GF_IS_CLUSTERED));
        } else if (Boolean.getBoolean(IEP_IS_CLUSTERED)){
        	//Mean iep engine is running in HA and Fail-over mode
            instanceName = System.getProperty(IEP_INSTANCE_NAME);
            if(instanceName == null || instanceName.equals("")) {
            	throw new RuntimeException(mMessages.getString(
            			"ExtendedComponentContext.Cluster_instance_name_not_specified", IEP_INSTANCE_NAME));
            }
            mConfigProp.put(PROP_IS_CLUSTERED, System.getProperty(IEP_IS_CLUSTERED));
        }

        mConfigProp.put(PROP_ENGINE_ID, instanceName);
        
        String installRoot = mContext.getInstallRoot();
        String iepDerbyJarFile = installRoot + File.separator + "lib" + File.separator + "iepderby.jar";
        mConfigProp.setProperty(PROP_IEP_DERBY_JAR_PATH, iepDerbyJarFile);

        mUpdatedConfigProp = new Properties();
        File updatedConfigFile = new File(workspaceRoot + File.separator + PROP_UPDATED_CONFIG_FILE);
        mUpdatedConfigProp.load(new FileInputStream(updatedConfigFile));
    }

    public void loadBootstrapProperties() throws Exception {
        String workspaceRoot = mContext.getWorkspaceRoot();
        mBootstrapProp = new Properties();
        File bootstrapFile = new File(workspaceRoot + File.separator + PROP_BOOTSTRAP_FILE);
        mBootstrapProp.load(new FileInputStream(bootstrapFile));
        if (mBootstrapProp.getProperty(PROP_NEW_INSTALL).equals("true")) {
            mBootstrapProp.setProperty(PROP_NEW_INSTALL, "false");
            mBootstrapProp.store(new FileOutputStream(bootstrapFile), "bootstrap properties");
            mBootstrapProp.setProperty(PROP_NEW_INSTALL, "true");
        }
    }

    public boolean isNewInstall() {
        return mBootstrapProp.getProperty(PROP_NEW_INSTALL).equals("true");
    }

    public List<String> diffConfigFiles() throws Exception {
        List<String> diff = new LinkedList<String>();
        if (!mConfigProp.getProperty(PROP_DB_NON_XA_JNDI_NAME).equals(mUpdatedConfigProp.getProperty(PROP_DB_NON_XA_JNDI_NAME))) {
            diff.add(PROP_DB_NON_XA_JNDI_NAME);
        }
        if (!mConfigProp.getProperty(PROP_DB_XA_JNDI_NAME).equals(mUpdatedConfigProp.getProperty(PROP_DB_XA_JNDI_NAME))) {
            diff.add(PROP_DB_XA_JNDI_NAME);
        }
        if (!mConfigProp.getProperty(PROP_DB_SCHEMA).equals(mUpdatedConfigProp.getProperty(PROP_DB_SCHEMA))) {
            diff.add(PROP_DB_SCHEMA);
        }
        return diff;
    }

    public void mergeConfigPropertiesAndFiles() throws Exception {
        mConfigProp.setProperty(PROP_DB_NON_XA_JNDI_NAME, mUpdatedConfigProp.getProperty(PROP_DB_NON_XA_JNDI_NAME));
        mConfigProp.setProperty(PROP_DB_XA_JNDI_NAME, mUpdatedConfigProp.getProperty(PROP_DB_XA_JNDI_NAME));
        mConfigProp.setProperty(PROP_DB_SCHEMA, mUpdatedConfigProp.getProperty(PROP_DB_SCHEMA));
        mConfigProp.setProperty(PROP_GARBAGE_COLLECTION_ENABLED, mUpdatedConfigProp.getProperty(PROP_GARBAGE_COLLECTION_ENABLED));
//        mConfigProp.setProperty(PROP_THREADS_COUNT, mUpdatedConfigProp.getProperty(PROP_THREADS_COUNT));
        mConfigProp.setProperty(PROP_TRANSACTED_OUTPUT, mUpdatedConfigProp.getProperty(PROP_TRANSACTED_OUTPUT));
        mConfigProp.setProperty(PROP_MAXIMUM_BATCH_SIZE, mUpdatedConfigProp.getProperty(PROP_MAXIMUM_BATCH_SIZE));
        
        String workspaceRoot = mContext.getWorkspaceRoot();
        File configFile = new File(workspaceRoot + File.separator + PROP_CONFIG_FILE);
        mConfigProp.remove(PROP_DB_JNDI_CONTEXT);
        mConfigProp.store(new FileOutputStream(configFile), "iepse properties");
        mConfigProp.put(PROP_DB_JNDI_CONTEXT, mInitialContext);
    }

    public BaseMessagingChannel getDeliveryChannel() {
        return mChannel;
    }

    public DeploymentLookup getDeploymentLookup() {
        return mDeploymentLookup;
    }

    public IEPSEInOnlyThread getInOnlyThread() {
        return mInOnlyThread;
    }

    public IEPSEOutOnlyThread getOutOnlyThread() {
        return mOutOnlyThread;
    }

    public ExchangeTable getExchangeTable() {
        return mExchangeTable;
    }

    public DeploymentTable getDeploymentTable() {
        return mDeploymentTable;
    }

    public IEPEngine getEngine() {
        return mEngine;
    }

    private void unregisterRuntimeConfigurationMBean() throws JBIException {
        try {
            MBeanServer mbServer = mContext.getMBeanServer();
            MBeanNames mbnHndl = mContext.getMBeanNames();
            ObjectName mbn = mbnHndl.createCustomComponentMBeanName(CONFIGURATION_KEY);
            if(mbServer.isRegistered(mbn)) {
                mbServer.unregisterMBean(mbn);
            }
            //mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("ExtendedComponentContext.Failed_to_un-register_runtime_configuration_mbean_for_") + mContext.getComponentName(), ex);
        }
    }

    private void unregisterAdminMBean() throws JBIException {
        try {
            mManagementMbeanRegister.unregisterMBean();
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("ExtendedComponentContext.Failed_to_un-register_runtime_configuration_mbean_for_") + mContext.getComponentName(), ex);
        }
    }

    public TransactionManager getTransactionManager() {
        return (TransactionManager) mContext.getTransactionManager();
    }

    public boolean isTransactedOutput() {
        return Boolean.parseBoolean(getConfigProperties().getProperty(PROP_TRANSACTED_OUTPUT));
    }

    public int getMaxConcurrencyLimit(EndpointInfo edi) {
        ThrottlingConfig tc = mChannel.getServiceQuality(edi, ThrottlingConfig.class);
        if (tc == null) {
            return Integer.MAX_VALUE;
        }
        return tc.getMaxConcurrencyLimit();
    }

    public void startPollingTable(DeploymentRecord r) throws Exception {
        QueryPlan plan = r.getPlan();
        if (plan != null) {
                String instanceId = plan.getInstanceId();
                List<ExternalTablePollingStream> pollingOps = plan.getExternalTablePollingStreamList();
                Iterator<ExternalTablePollingStream> it = pollingOps.iterator();
                while (it.hasNext()) {
                        ExternalTablePollingStream op = it.next();
                        int pollingInterval = op.getPollingInterval();
                        String pollingIntervalTimeUnit = op.getPollingIntervalTimeUnit();
                        long pInterval = pollingInterval;
                        String databaseJndiName = op.getDatabaseJndiName();
                        Connection source = null;
                        Connection target = null;
                        try {
                            if (databaseJndiName != null) {
                                source = Util.getConnection(mInitialContext, databaseJndiName);
                                target = Util.getConnection(mConfigProp);
                                if (source != null && target != null) {
                                    IEPSEExternalTablePollingThread run = new IEPSEExternalTablePollingThread(source, target, op);
                                    TimeUnit tu = Util.getPollingTimeUnit(pollingIntervalTimeUnit);
                                    if (tu == null) {
                                        pInterval = Util.getAdjustedPollingIntervalInSeconds(pollingInterval, pollingIntervalTimeUnit);
                                        tu = TimeUnit.SECONDS;
                                    }
                                    ScheduledFuture sf = mPollingTableExecuterService.scheduleAtFixedRate(run, 0L, pInterval, tu);
                                    FutureInfo info = new FutureInfo(source, target, sf);
                                    List<FutureInfo> infos =  mInstanceIdToSchedulePollingTableStreamMap.get(instanceId);
                                    if(infos == null) {
                                        infos = new ArrayList<FutureInfo>();
                                        mInstanceIdToSchedulePollingTableStreamMap.put(instanceId, infos);
                                    }
                                    
                                    infos.add(info);
                                    
                                }
                            }
                        
                        } catch(Exception ex) {
                            Util.close(source);
                            Util.close(target);
                            mLogger.log(Level.SEVERE, mMessages.getString("ExtendedComponentContext.Problem_Starting_External_Table_Polling", new Object[]{databaseJndiName}), ex);
                            throw ex;
                        }
                }
        }
    }

    public void stopPollingTable(DeploymentRecord r) {
        QueryPlan plan = r.getPlan();
        if (plan != null) {
            String instanceId = plan.getInstanceId();
            List<FutureInfo> infos = mInstanceIdToSchedulePollingTableStreamMap.get(instanceId);
            if(infos == null) {
                return;
            }
            
            Iterator<FutureInfo> it = infos.iterator();
            while(it.hasNext()) {
                FutureInfo info = it.next();
                if (info != null) {
                    Future sf = info.getFuture();
                    if (sf != null) {
                        sf.cancel(true);
                    }
                }
            }

            mInstanceIdToSchedulePollingTableStreamMap.remove(instanceId);
        }
    }

    
    public void startReplayStream(DeploymentRecord r) throws Exception {
        QueryPlan plan = r.getPlan();
        if (plan != null) {
            String instanceId = plan.getInstanceId();
            List<ReplayStream> replayOps = plan.getReplayStreamList();
            Iterator<ReplayStream> it = replayOps.iterator();
            while (it.hasNext()) {
                ReplayStream op = it.next();
                String databaseJndiName = op.getDatabaseJndiName();
                Connection source = null;
                Connection target = null;
                try {
                    if (databaseJndiName != null) {
                        source = Util.getConnection(mInitialContext, databaseJndiName);
                        target = Util.getConnection(mConfigProp);
                        if (source != null && target != null) {
                            IEPSEReplayStreamThread run = new IEPSEReplayStreamThread(source, target, op);
                            ScheduledFuture sf = mReplayExecuterService.scheduleAtFixedRate(run, 0L, 100, TimeUnit.MILLISECONDS);
                            FutureInfo info = new FutureInfo(source, target, sf);
                            List<FutureInfo> infos =  mInstanceIdToReplayStreamMap.get(instanceId);
                            if(infos == null) {
                                infos = new ArrayList<FutureInfo>();
                                mInstanceIdToReplayStreamMap.put(instanceId, infos);
                            }
                            
                            infos.add(info);
                            
                        }
                    }
                } catch(Exception ex) {
                    Util.close(source);
                    Util.close(target);
                    mLogger.log(Level.SEVERE, mMessages.getString("ExtendedComponentContext.Problem_starting_Replay_Stream", new Object[]{databaseJndiName}), ex);
                    //rethrow
                    throw ex;
                } 
                

            }
        }
    }

    public void stopReplayStream(DeploymentRecord r) {
        QueryPlan plan = r.getPlan();
        if (plan != null) {
            String instanceId = plan.getInstanceId();
            List<FutureInfo> infos = mInstanceIdToReplayStreamMap.get(instanceId);
            if(infos == null) {
                return;
            }
            
            Iterator<FutureInfo> it = infos.iterator();
            while(it.hasNext()) {
                FutureInfo info = it.next();
                if (info != null) {
                    Future sf = info.getFuture();
                    if (sf != null) {
                        sf.cancel(true);
                    }
                }
            }

            mInstanceIdToReplayStreamMap.remove(instanceId);
        }
    }

    private void startPollingTable() throws Exception {
        if (mPollingTableExecuterService != null && !mPollingTableExecuterService.isShutdown()) {
            mPollingTableExecuterService.shutdownNow();
        }

        mPollingTableExecuterService = Executors.newScheduledThreadPool(5, new MySchedulerThreadFactory());
        if (mDeploymentTable != null) {
            Iterator<String> it = mInstanceIdToSchedulePollingTableStreamMap.keySet().iterator();
            while (it.hasNext()) {
                String instanceId = it.next();
                DeploymentRecord r = mDeploymentTable.getRecordByInstanceId(instanceId);
                if (r != null) {
                    startPollingTable(r);
                }
            }
        }
    }

    private void startReplayStream() throws Exception {
        if (mReplayExecuterService != null && !mReplayExecuterService.isShutdown()) {
            mReplayExecuterService.shutdownNow();
        }

        mReplayExecuterService = Executors.newScheduledThreadPool(5, new MyReplaySchedulerThreadFactory());

        if (mDeploymentTable != null) {
            Iterator<String> it = mInstanceIdToReplayStreamMap.keySet().iterator();
            while (it.hasNext()) {
                String instanceId = it.next();
                DeploymentRecord r = mDeploymentTable.getRecordByInstanceId(instanceId);
                if (r != null) {
                    startReplayStream(r);
                }
            }
        }
    }

    class MySchedulerThreadFactory implements ThreadFactory {

        private final String mName = "IEPSETablePollingThread";
        private int threadCount = 1;

        public Thread newThread(Runnable r) {

            return new Thread(r, "IEPSETablePollingThread" + threadCount++);
        }
    }
    
    class MyReplaySchedulerThreadFactory implements ThreadFactory {

        private final String mName = "IEPSEReplayStreamThread";
        private int threadCount = 1;

        public Thread newThread(Runnable r) {

            return new Thread(r, "IEPSEReplayStreamThread" + threadCount++);
        }
    }
}

