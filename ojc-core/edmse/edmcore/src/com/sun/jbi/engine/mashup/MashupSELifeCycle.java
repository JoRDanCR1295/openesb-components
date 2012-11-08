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
package com.sun.jbi.engine.mashup;

import java.io.File;
import java.io.FileInputStream;
import java.util.Hashtable;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.management.MBeanNames;


import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.engine.mashup.mbean.MashupSERuntimeConfiguration;
import com.sun.mashup.engine.utils.ConnectionPoolConfigLookup;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.BufferedReader;
import javax.jbi.servicedesc.ServiceEndpoint;

public class MashupSELifeCycle implements ComponentLifeCycle {
    /*
     * TODO add support for i18n private static final Messages MESSAGES =
     * Messages.getMessages(MashupSELifeCycle.class);
     */

    private static final Logger mLogger = Logger.getLogger(MashupSELifeCycle.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupSELifeCycle.class);
    private MashupSEComponent mComponent;
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private MashupSEInOutThread mThread;
    private MashupMapEntryTable mMapEntryTable;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private MashupSERuntimeConfiguration runtimeConfigMBean;
    private Hashtable mWsdlMap = new Hashtable();

    public MashupSELifeCycle(MashupSEComponent component) {
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
            String configFile = workspaceRoot + File.separator + ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
            configProp.load(new FileInputStream(configFile));
            mLogger.info(mMessages.getString("EDMSE-I0324.AppConfig_Properties") + configProp.toString());

            mMapEntryTable = new MashupMapEntryTable();

            MashupSEServiceUnitManager serviceUnitManager = (MashupSEServiceUnitManager) mComponent.getServiceUnitManager();
            serviceUnitManager.initialize(mMapEntryTable, mContext, mChannel,
                    mStatusProviderHelper, mWsdlMap, runtimeConfigMBean);
            mComponent.setMashupMapEntryTable(mMapEntryTable);

            ConnectionPoolConfigLookup.initMBeanServer(mContext.getMBeanServer());

            mThread = new MashupSEInOutThread(runtimeConfigMBean, mChannel, mMapEntryTable);
            mThread.start();
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(ex);
        }
        mLogger.info(mMessages.getString("EDMSE-I0325.initialization_Success"));
    }

    /**
     * @param context
     * @throws JBIException
     */
    private void registerStatusProviderMbean(ComponentContext context) throws JBIException {
        try {
            MBeanNames mbnHndl = context.getMBeanNames();
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");

            mStatusProviderHelper = new StatusProviderHelper(context.getComponentName(),
                    statusMBeanObjName, context.getMBeanServer());

            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info(mMessages.getString("EDMSE-I0326.mBean_RegisterStatus")+ context.getComponentName());
            }
        } catch (Exception e) {
            mLogger.log(Level.WARNING, mMessages.getString("EDMSE-E0103.MBeanRegister_Failed"), e);
            throw new JBIException(mMessages.getString("EDMSE-E0103.MBeanRegister_Failed"), e);
        }
    }

    private void registerRuntimeConfigurationMbean(ComponentContext context) throws JBIException {
        try {

            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(context);
            String configData = "";
            String configSchema = "";
            String configDataFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            if (configDataFile.exists()) {
                configData = ReadWriteTextFile.getContents(configDataFile);
            }
            String configSchemaFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentConfiguration.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            if (configSchemaFile.exists()) {
                configSchema = ReadWriteTextFile.getContents(configSchemaFile);
            }


            mRuntimeConfigHelper = new RuntimeConfigurationHelper(
                    RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE, context.getComponentName(),
                    context.getMBeanServer());

            runtimeConfigMBean = new MashupSERuntimeConfiguration(context.getWorkspaceRoot() + File.separator + "config.properties", context.getWorkspaceRoot(),
                    keystoreUtil, configSchema, configData);

            /*
             * TODO add notification listener so when the attribute change
             * happens, Mashupse take care of the change
             * runtimeConfigMBean.addNotificationListener(listener, filter, handback)
             */
            mRuntimeConfigHelper.registerMBean(runtimeConfigMBean);
        } catch (MBeanRegistrationException ex) {
            throw new JBIException("MashupSELifeCycle.unable_to_register_mbean", ex);
        } catch (NotCompliantMBeanException ex) {
            throw new JBIException("MashupSELifeCycle.unable_to_create_runtime_configuration_mbean",
                    ex);
        } catch (MalformedObjectNameException ex) {
            throw new JBIException("MashupSELifeCycle.unable_to_create_runtime_configuration_helper",
                    ex);
        } catch (InstanceAlreadyExistsException ex) {
            throw new JBIException("MashupSELifeCycle.unable_to_create_runtime_configuration_mbean",
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
        //mLogger.info(mMessages.getString("EDMSE-I0327.starting_EDMSE"));
        mLogger.info(mMessages.getString("EDMSE-I0328.EDMSE_Started"));
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
        //mLogger.info(mMessages.getString("EDMSE-I0329.stoping_EDMSE"));
        mLogger.info(mMessages.getString("EDMSE-I0330.EDMSE_Stopped"));
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
            // See MashupSEInOutThread's run()
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
            throw new javax.jbi.JBIException(mMessages.getString("EDMSE-E0104.SE_ShutdownError"), e);
        }

        unregisterRuntimeConfigurationMbean();
        unregisterStatusProviderMbean();
        mLogger.info(mMessages.getString("EDMSE-I0331.done_SEShutdown"));
    }

    /**
     * @throws JBIException
     */
    private void unregisterRuntimeConfigurationMbean() throws JBIException {
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            mLogger.log(Level.WARNING,
                    "MashupSELifeCycle.Failed_to_un-register_runtime_configuration_mbean_for_" + mContext.getComponentName(), ex);
            throw new JBIException(
                    "MashupSELifeCycle.Failed_to_un-register_runtime_configuration_mbean_for_" + mContext.getComponentName(), ex);
        }
    }

    /**
     * @throws JBIException
     */
    private void unregisterStatusProviderMbean() throws JBIException {
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception e) {
            mLogger.log(Level.WARNING,
                    "MashupSELifeCycle.Failed to un-register status provider MBean for " + mContext.getComponentName(), e);
            throw new JBIException(
                    "MashupSELifeCycle.Failed to un-register status provider MBean for " + mContext.getComponentName(), e);
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
                //mLogger.info(mMessages.getString("EDMSE-I0332.mashupThread")+ thread.isAlive());
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
