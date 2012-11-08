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
 * @(#)ETLSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.etl;

import com.sun.etl.engine.spi.DBConnectionProvider;
import java.io.File;
import java.io.FileInputStream;
import java.util.Hashtable;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.engine.etl.AlertsUtil;
import com.sun.jbi.alerter.NotificationEvent;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.etl.mbean.ETLSERuntimeConfiguration;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.engine.etl.persistence.DBSchemaCreation;
import com.sun.jbi.engine.etl.persistence.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.etl.persistence.impl.MessagePersistenceHandler;
import com.sun.jbi.internationalization.Messages;

import com.sun.etl.jdbc.DBConnectionFactory;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import com.sun.etl.engine.utils.ConnectionPoolConfigLookup;

public class ETLSELifeCycle implements ComponentLifeCycle {
    /*
     * TODO add support for i18n private static final Messages MESSAGES =
     * Messages.getMessages(ETLSELifeCycle.class);
     */

    private static transient final Logger mLogger = Logger.getLogger(ETLSELifeCycle.class.getName());
    private static final Messages mMessages = Messages.getMessages(ETLSELifeCycle.class);
    // A short display name
    public static final String SHORT_DISPLAY_NAME = "sun-etl-engine";
    private ETLSEComponent mComponent;
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private ETLSEInOutThread mThread;
    private EtlMapEntryTable mEtlMapEntryTable;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private ETLSERuntimeConfiguration runtimeConfigMBean;
    private Hashtable mWsdlMap = new Hashtable();
    private ETLInOutQueue messageQueue;

    public ETLSELifeCycle(ETLSEComponent component) {
        mComponent = component;
    }

    /**
     * Initialize the component. This performs initialization required by the
     * component but does not make it ready to process messages. This method is
     * called once for each life cycle of the component
     * 
     * @param context -
     *            the component's context.
     * @throws javax.jbi.JBIException
     *             if the component is unable to initialize.
     */
    public void init(ComponentContext context) throws javax.jbi.JBIException {

        registerStatusProviderMbean(context);
        registerRuntimeConfigurationMbean(context);
        try {
            mContext = context;
            mChannel = context.getDeliveryChannel();

            // See "Prepare Installation Context" section in JBI 1.0 pr spec
            // 6.4.2.1.1
            Properties configProp = new Properties();
            String workspaceRoot = context.getWorkspaceRoot();
            String installRoot = context.getInstallRoot();
            DBSchemaCreation.setInstallRoot(installRoot);
            String configFile = workspaceRoot + File.separator + ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
            configProp.load(new FileInputStream(configFile));
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0130.ETLSE_configProperties", configProp.toString()));
            mEtlMapEntryTable = new EtlMapEntryTable();

            ETLSEServiceUnitManager serviceUnitManager = (ETLSEServiceUnitManager) mComponent.getServiceUnitManager();
            serviceUnitManager.initialize(mEtlMapEntryTable, mContext, mChannel,
                    mStatusProviderHelper, mWsdlMap, runtimeConfigMBean);
            MessagePersistenceHandler.initializeContext(context);
            MessagePersistenceHandler.setPersistenceConfigMBean(runtimeConfigMBean);
            boolean isNonXAEnabled = false;
            boolean isXAEnabled = false;
            String dsJndiName = runtimeConfigMBean.getNonXaDSJndiName();
            if (dsJndiName != null) {
                Connection conn = MessagePersistenceHandler.getConnection(dsJndiName);
                mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0131.JndiName" + dsJndiName));
                mLogger.log(Level.INFO, mMessages.getString("ETLSE-I0132.dbConnection_Successfully"));
                PersistenceDBSchemaCreation.getInstance().checkAndCreateTables(conn);
                MessagePersistenceHandler.initializeContext(context);
                isNonXAEnabled = true;
            }
            
            
            
            String xaDsJndiName = runtimeConfigMBean.getXaDSJndiName();
            if (xaDsJndiName != null) {
                try {
                    Connection conn = MessagePersistenceHandler.getConnection(xaDsJndiName);
                    mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0131.JndiName_XApersistence"+ dsJndiName));
                    mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0132.XAdbConnection_Successfully"));
                    PersistenceDBSchemaCreation.getInstance().checkAndCreateTables(conn);
                    MessagePersistenceHandler.setPersistenceFlag(true);

                    MessagePersistenceHandler.setXAFlag(true);
                    isNonXAEnabled = true;
                } catch (Throwable e) {
                    mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0133.XApersistence_Disabled"));
                }
            }
             
            if (isXAEnabled || isNonXAEnabled) {
                MessagePersistenceHandler.setPersistenceFlag(true);

            } else {
                mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0134.No_PersistenceConfiguration_found"));
            }
            
          
            //mLogger.info("created tables successfully");
            mThread = new ETLSEInOutThread(runtimeConfigMBean, mChannel, mEtlMapEntryTable);
            ETLInOutQueue messageQueue = new ETLInOutQueue(mThread);
            serviceUnitManager.setQueue(messageQueue);
            mThread.setMessageQueue(messageQueue);
            mThread.start();
            DBConnectionFactory.initializeNamingContext(mContext.getNamingContext());
       } catch (Exception ex) {
            String errMsg = "Exception during init(): " + ex.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00001");
            throw new javax.jbi.JBIException(ex);
        }
        mLogger.log(Level.INFO, mMessages.getString("ETLSE-I0135.Initializing_ETLSE_successfully"));
        
    }

    /**
     * @param context
     * @throws JBIException
     */
    private void registerStatusProviderMbean(ComponentContext context) throws JBIException {
        try {
            mStatusProviderHelper = new StatusProviderHelper("ETLSE status provider",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE, context.getComponentName(), context.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0136.Registered_StatusProviderMBean", context.getComponentName()));
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,mMessages.getString("ETLSE-E0404.Failed"), e);
            String errMsg =mMessages.getString("ETLSE-I0136.Registered_StatusProviderMBean", e.getMessage());
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00002");
            throw new JBIException(mMessages.getString("ETLSE-E0404.Failed"), e);
        }
    }

    private void registerRuntimeConfigurationMbean(ComponentContext context) throws JBIException {
        try {
            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(context);
            String configData = "";
            String configSchema = "";
            String configDataFileLoc = context.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            if (configDataFile.exists()) {
                configData = ReadWriteTextFile.getContents(configDataFile);
            }
            String configSchemaFileLoc = context.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            if (configSchemaFile.exists()) {
                configSchema = ReadWriteTextFile.getContents(configSchemaFile);
            }

            mRuntimeConfigHelper = new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE, context.getComponentName(),
                    context.getMBeanServer());

            // runtimeConfigMBean = new ETLSERuntimeConfiguration(context.getWorkspaceRoot() + File.separator + "config.properties");

            runtimeConfigMBean = new ETLSERuntimeConfiguration(context.getWorkspaceRoot() +
                    File.separator + "config.properties", context.getWorkspaceRoot(),
                    keystoreUtil, configSchema, configData);
            

            /*
             * TODO add notification listener so when the attribute change
             * happens, etlse take care of the change
             * runtimeConfigMBean.addNotificationListener(listener, filter, handback)
             */
            mRuntimeConfigHelper.registerMBean(runtimeConfigMBean);
        } catch (MBeanRegistrationException ex) {
            //String errMsg = mLoc.t("PRSR054: Failed to register Runtime Configuration MBean") +  ex.getMessage();
            String errMsg = mMessages.getString("ETLSE-E0405.registering_RuntimeConfigurationMBean_Failed")+ ex.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00003");
            throw new JBIException("ETLSELifeCycle.unable_to_register_mbean", ex);
        } catch (NotCompliantMBeanException ex) {
            //String errMsg = mLoc.t("PRSR055: Failed to register Runtime Configuration MBean") +  ex.getMessage();
            String errMsg = mMessages.getString("ETLSE-E0405.registering_RuntimeConfigurationMBean_Failed")+ ex.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00004");
            throw new JBIException("ETLSELifeCycle.unable_to_create_runtime_configuration_mbean",
                    ex);
        } catch (MalformedObjectNameException ex) {
            //String errMsg = mLoc.t("PRSR056: Failed to register Runtime Configuration MBean") +  ex.getMessage();
            String errMsg = mMessages.getString("ETLSE-E0405.registering_RuntimeConfigurationMBean_Failed")+ ex.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00005");
            throw new JBIException("ETLSELifeCycle.unable_to_create_runtime_configuration_helper",
                    ex);
        } catch (InstanceAlreadyExistsException ex) {
            //String errMsg = mLoc.t("PRSR057: Failed to register Runtime Configuration MBean") +  ex.getMessage();
            String errMsg = mMessages.getString("ETLSE-E0405.registering_RuntimeConfigurationMBean_Failed") + ex.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00006");
            throw new JBIException("ETLSELifeCycle.unable_to_create_runtime_configuration_mbean",
                    ex);
        }

    }

    /**
     * Get the JMX ObjectName for any additional MBean for this component. If
     * there is none, return null.
     * 
     * @return ObjectName the JMX object name of the additional MBean or null if
     *         there is no additional MBean.
     */
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    /**
     * Start the component. This makes the component ready to process messages.
     * This method is called after init() completes when the JBI implementation
     * is starting up, and when the component is being restarted after a
     * previous call to shutDown(). If stop() was called previously but
     * shutDown() was not, then start() can be called again without another call
     * to init().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to start.
     */
    public void start() throws javax.jbi.JBIException {
        mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0139.Starting_ETLSE_Successful"));
        String msg = mMessages.getString("ETLSE-I0139.Starting_ETLSE_Successful");

        AlertsUtil.getAlerter().critical(msg,
                ETLSELifeCycle.SHORT_DISPLAY_NAME,
                "",
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                "ETLSE-11111");
        StringBuffer msgBuf = new StringBuffer(" ");
        if(runtimeConfigMBean != null)
        runtimeConfigMBean.dump(msgBuf);
        this.mThread.startMessageQueue();
    }

    /**
     * Stop the component. This makes the component stop accepting messages for
     * processing. After a call to this method, start() can be called again
     * without first calling init().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        if( mThread != null ) {
            mThread.stopMessageQueue();
        }        
        mLogger.info(mMessages.getString("ETLSE-I0142.Stopping_ETLSE_Successful"));
        String msg = mMessages.getString("ETLSE-I0142.Stopping_ETLSE_Successful");

        AlertsUtil.getAlerter().critical(msg,
                ETLSELifeCycle.SHORT_DISPLAY_NAME,
                "",
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                "ETLSE-99999");
    }

    /**
     * Shut down the component. This performs cleanup before the component is
     * terminated. Once this method has been called, init() must be called
     * before the component can be started again with a call to start().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to shut down.
     */
    public void shutDown() throws javax.jbi.JBIException {
        try {
            // Must close mChannel before stop mInOnlyThread
            // because mThread may be pending on accept().
            // Closing channel will wake it up from accept()
            // See ETLSEInOutThread's run()
            
           
            if (mThread != null) {
                mThread.cease();
            }
            if (mChannel != null) {
                mChannel.close();
            }
            if (mThread != null) {
                stopThread(mThread);
            }
        } catch (Exception e) {
            e.printStackTrace();
            //String errMsg = mLoc.t("PRSR058: Failed to register Runtime Configuration MBean") +  ex.getMessage();
            String errMsg =mMessages.getString("ETLSE-E0405.registering_RuntimeConfigurationMBean_Failed")+ e.getMessage();
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00007");
            throw new javax.jbi.JBIException(mMessages.getString("ETLSE-E0406.ETLSE_Shutdown_Error"), e);
        }

        unregisterRuntimeConfigurationMbean();
        unregisterStatusProviderMbean();
        mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0144.Shuttingdown_ETLSE_Successful"));
    }

    /**
     * @throws JBIException
     */
    private void unregisterRuntimeConfigurationMbean() throws JBIException {
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE,mMessages.getString("ETLSE-S0215.Failed_to_un-register_runtime_configuration_mbean", mContext.getComponentName()), ex);
            String errMsg = mMessages.getString("ETLSE-S0215.Failed_to_un-register_runtime_configuration_mbean", mContext.getComponentName());
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00008");
            throw new JBIException(
                    "ETLSELifeCycle.Failed_to_un-register_runtime_configuration_mbean_for_" + mContext.getComponentName(), ex);
        }
    }

    /**
     * @throws JBIException
     */
    private void unregisterStatusProviderMbean() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception e) {
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0145.Failed_un-registerMBean", mContext.getComponentName()), e);
            String errMsg = mMessages.getString("ETLSE-S0215.Failed_to_un-register_runtime_configuration_mbean", mContext.getComponentName());
            AlertsUtil.getAlerter().critical(errMsg,
                    ETLSELifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "ETLSE-00009");
            throw new JBIException(
                    "ETLSELifeCycle.Failed to un-register status provider MBean for " + mContext.getComponentName(), e);
        }
    }

    private void stopThread(Thread thread) throws InterruptedException {
        boolean state = true;
        while (state) {
            if (thread.isAlive()) {
                state = true;
                Thread.currentThread();
                Thread.sleep(100L);
            } else {
                state = false;
            }

        }
    }

    public static class ReadWriteTextFile {

        /**
         * Fetch the entire contents of a text file, and return it in a String. This
         * style of implementation does not throw Exceptions to the caller.
         * 
         * @param aFile
         *            is a file which already exists and can be read.
         */
        static public String getContents(File aFile) {
            // ...checks on aFile are elided
            StringBuffer contents = new StringBuffer();

            // declared here only to make visible to finally clause
            BufferedReader input = null;
            try {
                // use buffering, reading one line at a time
                // FileReader always assumes default encoding is OK!
                input = new BufferedReader(new FileReader(aFile));
                String line = null; // not declared within while loop
            /*
                 * readLine is a bit quirky : it returns the content of a line MINUS
                 * the newline. it returns null only for the END of the stream. it
                 * returns an empty String if two newlines appear in a row.
                 */
                while ((line = input.readLine()) != null) {
                    contents.append(line);
                    contents.append(System.getProperty("line.separator"));
                }
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            } finally {
                try {
                    if (input != null) {
                        // flush and close both "input" and its underlying
                        // FileReader
                        input.close();
                    }
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
            return contents.toString();
        }
    }
}
