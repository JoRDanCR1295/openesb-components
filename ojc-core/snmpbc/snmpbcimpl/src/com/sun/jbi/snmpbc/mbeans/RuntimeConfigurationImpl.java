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
 * @(#)RuntimeConfigurationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.mbeans;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Runtime configuration MBean, allows dynamic configuration of SNMP BC and Engine
 * 
 * @author echou
 */
public class RuntimeConfigurationImpl extends NotificationBroadcasterSupport implements RuntimeConfigurationIntf {

    private static final Messages mMessages =
        Messages.getMessages(RuntimeConfigurationImpl.class);
    private static final Logger mLogger =
        Logger.getLogger(RuntimeConfigurationImpl.class.getName());
        
    // Property names, as used in configuration file
    public static final String PROP_OUTBOUND_THREAD_POOL_MAX_SIZE = 
            "OutboundThreadPoolMaxSize";
    public static final String PROP_OUTBOUND_THREAD_POOL_CORE_SIZE = 
            "OutboundThreadPoolCoreSize";
    public static final String PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS = 
            "OutboundThreadPoolKeepAliveInMillis";
    
    
    // Configuration
    private Properties mConfig;
    private String mWorkspaceRoot;
    
    // instance variable representing in-memory configuration
    private int outboundThreadPoolMaxSize = 5;
    private int outboundThreadPoolCoreSize = 5;
    private long outboundThreadPoolKeepAliveInMillis = 60000;
    
    
    // test mbean configuration
    public static final String PROP_TEST_MODE = "TestMode";
    private boolean testMode = false;
    
    private String mConfigSchema;
    private String mConfigData;
    private ApplicationVariables mApplicationVars;
 
    public RuntimeConfigurationImpl(String workspaceRoot, String configSchema, String configData) throws Exception {
        mWorkspaceRoot = workspaceRoot;
        mConfigSchema = configSchema;
        mConfigData = configData;
        mConfig = ConfigPersistence.loadConfig(mWorkspaceRoot);
        mApplicationVars = new ApplicationVariables();
        initValues();
    }
    
    private void initValues() throws Exception {
        initOutboundThreadPoolMaxSize();
        initOutboundThreadPoolCoreSize();
        initOutboundThreadPoolKeepAliveInMillis();
        
        initTestMode();
        
        validateAllValues();
    }


    public synchronized int getInboundThreadPoolMaxSize() {
        return 0;
    }

    public synchronized void setInboundThreadPoolMaxSize(int val) throws Exception {
    }

    public synchronized int getInboundThreadPoolCoreSize() {
        return 0;
    }

    public synchronized void setInboundThreadPoolCoreSize(int val) throws Exception {
    }

    public synchronized long getInboundThreadPoolKeepAliveInMillis() {
        return 0;
    }

    public synchronized void setInboundThreadPoolKeepAliveInMillis(long val) throws Exception {
    }
    
    public synchronized long getInboundBatchSize() {
        return 0;
    }

    public synchronized void setInboundBatchSize(long val) throws Exception {
    }

    // reading from props file to memory
    private synchronized void initOutboundThreadPoolMaxSize() {
        String val = mConfig.getProperty(PROP_OUTBOUND_THREAD_POOL_MAX_SIZE);
        try {
            outboundThreadPoolMaxSize = Integer.parseInt(val);
        } catch (NumberFormatException nfe) {
            String msg = mMessages.getString("SNMPBC_E12001.INVALID_PROP_VALUE", 
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_MAX_SIZE, 
                        outboundThreadPoolMaxSize});
            mLogger.log(Level.WARNING, msg);
        }
    }
    
    public synchronized int getOutboundThreadPoolMaxSize() {
        return outboundThreadPoolMaxSize;
    }

    // write to props file and notify listener
    public synchronized void setOutboundThreadPoolMaxSize(int val) throws Exception {
        if (val <= 0 || val < outboundThreadPoolCoreSize) {
            String errMsg = mMessages.getString("SNMPBC_E12001.INVALID_OB_TP_MAX",
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_MAX_SIZE,
                        0,
                        outboundThreadPoolCoreSize});
            throw new Exception(errMsg);
        }
        
        // Apply and save the changes
        mConfig.put(PROP_OUTBOUND_THREAD_POOL_MAX_SIZE, Integer.toString(val)); 
        persistConfiguration();
        
        // Notify listeners of this change
        int oldVal = outboundThreadPoolMaxSize;
        outboundThreadPoolMaxSize = val;
        Notification notification = new AttributeChangeNotification(this, 0, 
                System.currentTimeMillis(), null, PROP_OUTBOUND_THREAD_POOL_MAX_SIZE,
                Integer.class.getName(), oldVal, outboundThreadPoolMaxSize);
        sendNotification(notification);
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                    mMessages.getString("SNMPBC_C12006.SET_OPERATION_SUCCESS",
                        new Object[] {
                            PROP_OUTBOUND_THREAD_POOL_MAX_SIZE,
                            oldVal,
                            outboundThreadPoolMaxSize}));
        }
    }

    // reading from props file to memory
    private synchronized void initOutboundThreadPoolCoreSize() {
        String val = mConfig.getProperty(PROP_OUTBOUND_THREAD_POOL_CORE_SIZE);
        try {
            outboundThreadPoolCoreSize = Integer.parseInt(val);
        } catch (NumberFormatException nfe) {
            String msg = mMessages.getString("SNMPBC_E12001.INVALID_PROP_VALUE", 
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_CORE_SIZE, 
                        outboundThreadPoolCoreSize});
            mLogger.log(Level.WARNING, msg);
        }
    }
    
    public synchronized int getOutboundThreadPoolCoreSize() {
        return outboundThreadPoolCoreSize;
    }

    public synchronized void setOutboundThreadPoolCoreSize(int val) throws Exception {
        if (val < 0 || val > outboundThreadPoolMaxSize) {
            String errMsg = mMessages.getString("SNMPBC_E12003.INVALID_INVALID_OB_TP_CORE",
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_CORE_SIZE,
                        0,
                        outboundThreadPoolCoreSize});
            throw new Exception(errMsg);
        }
        
        // Apply and save the changes
        mConfig.put(PROP_OUTBOUND_THREAD_POOL_CORE_SIZE, Integer.toString(val)); 
        persistConfiguration();
        
        // Notify listeners of this change
        int oldVal = outboundThreadPoolCoreSize;
        outboundThreadPoolCoreSize = val;
        Notification notification = new AttributeChangeNotification(this, 0, 
                System.currentTimeMillis(), null, PROP_OUTBOUND_THREAD_POOL_CORE_SIZE,
                Integer.class.getName(), oldVal, outboundThreadPoolCoreSize);
        sendNotification(notification);
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                    mMessages.getString("SNMPBC_C12006.SET_OPERATION_SUCCESS",
                        new Object[] {
                            PROP_OUTBOUND_THREAD_POOL_CORE_SIZE,
                            oldVal,
                            outboundThreadPoolCoreSize}));
        }
    }

    // reading from props file to memory
    private synchronized void initOutboundThreadPoolKeepAliveInMillis() {
        String val = mConfig.getProperty(PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS);
        try {
            outboundThreadPoolKeepAliveInMillis = Long.parseLong(val);
        } catch (NumberFormatException nfe) {
            String msg = mMessages.getString("SNMPBC_E12001.INVALID_PROP_VALUE", 
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS, 
                        outboundThreadPoolKeepAliveInMillis});
            mLogger.log(Level.WARNING, msg);
        }
    }    
    
    public synchronized long getOutboundThreadPoolKeepAliveInMillis() {
        return outboundThreadPoolKeepAliveInMillis;
    }

    public synchronized void setOutboundThreadPoolKeepAliveInMillis(long val) throws Exception {
        if (val < 0) {
            String errMsg = mMessages.getString("SNMPBC_E12004.INVALID_INVALID_OB_TP_KEEP_ALIVE",
                    new Object[] {
                        val,
                        PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS,
                        0});
            throw new Exception(errMsg);
        }
        
        // Apply and save the changes
        mConfig.put(PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS, Long.toString(val)); 
        persistConfiguration();
        
        // Notify listeners of this change
        long oldVal = outboundThreadPoolKeepAliveInMillis;
        outboundThreadPoolKeepAliveInMillis = val;
        Notification notification = new AttributeChangeNotification(this, 0, 
                System.currentTimeMillis(), null, PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS,
                Long.class.getName(), oldVal, outboundThreadPoolKeepAliveInMillis);
        sendNotification(notification);
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                    mMessages.getString("SNMPBC_C12006.SET_OPERATION_SUCCESS",
                        new Object[] {
                            PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS,
                            oldVal,
                            outboundThreadPoolKeepAliveInMillis}));
        }
    }
    
    // this method will validate all prop values, and reset them to
    // default value if any inconsistency is found
    private synchronized void validateAllValues() throws Exception {
        if (outboundThreadPoolMaxSize <= 0 || outboundThreadPoolCoreSize < 0 ||
                outboundThreadPoolCoreSize > outboundThreadPoolMaxSize) {
            mLogger.log(Level.WARNING, 
                    mMessages.getString("SNMPBC_E12005.VALIDATION_ERROR"));
            outboundThreadPoolMaxSize = 5;
            outboundThreadPoolCoreSize = 5;
        }
        if (outboundThreadPoolKeepAliveInMillis < 0) {
            mLogger.log(Level.WARNING, 
                    mMessages.getString("SNMPBC_E12005.VALIDATION_ERROR"));
            outboundThreadPoolKeepAliveInMillis = 60000;
        }
    }
    
    private void persistConfiguration() throws Exception {
        // Persist the changed configuration
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            String errMsg = mMessages.getString("SNMPBC_E12007.PERSIST_FAILED",
                                                mWorkspaceRoot);
            throw new Exception(errMsg, ex);
        }
    }

    private synchronized void initTestMode() {
        String val = mConfig.getProperty(PROP_TEST_MODE);
        testMode = Boolean.parseBoolean(val);
    }
    
    public synchronized boolean getTestMode() {
        return testMode;
    }

    public synchronized void setTestMode(boolean val) throws Exception {
        // Apply and save the changes
        mConfig.put(PROP_TEST_MODE, Boolean.toString(val)); 
        persistConfiguration();
        testMode = val;
    }

    
    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema() {
        return this.mConfigSchema;
    }
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData() {
        return this.mConfigData;
    }

    /*---------------------------------------------------------------------------------*\
     *          Operations for Application Variables Management                        *
    \*---------------------------------------------------------------------------------*/
    
    /**
     * This operation adds a new applicationvariable. If a variable already exists with 
     * the same name as that specified then the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variables to the 
     *         component. 
     */
    public void addApplicationVariable(String name, CompositeData appVar)
        throws MBeanException {

        if (mApplicationVars.hasVariable(name)) {
            throw new MBeanException(
                new Exception("Application Variable with name, "
                              + name + ", already exists." +
                              "  Cannot add variable."));
        }
        try {
            mApplicationVars.setVariable(appVar); 
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
    }
     
    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar)
        throws MBeanException {

        if (!mApplicationVars.hasVariable(name)) {
            throw new MBeanException(
                new Exception ("Application Variable with name, " + name +
                               ", doesn't exist and it should when calling" +
                               " setApplicationVariable.  Cannot set" +
                               " variable."));
        }
        try {
            mApplicationVars.setVariable(appVar);
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
    }
     
    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
    public void deleteApplicationVariable(String name) throws MBeanException {

        if (!mApplicationVars.hasVariable(name)) {
            throw new MBeanException(
                new Exception("Application Variable with name, " + name +
                              ", doesn't exist and it should when calling" +
                              " setApplicationVariable.  Cannot set" +
                              " variable."));
        }
        mApplicationVars.removeVariable(name);
    }
     
     /**
      * Get the Application Variable set for a component.
      *
      * @return  a TabularData which has all the applicationvariables set on the component. 
      */
    public TabularData getApplicationVariables() {
        return mApplicationVars.toTabularData();
    }

    /*---------------------------------------------------------------------------------*\
     *            Operations for Application Configuration Management                  *
    \*---------------------------------------------------------------------------------*/
    
     /**
      * Get the CompositeType definition for the components application configuration 
      *
      * @return the CompositeType for the components application configuration.
      */
    public CompositeType getApplicationConfigurationType() {
        return null;
    }
     
     /**
      * Add an application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "name" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "name" in the namedConfig
      * @param namedConfig - application configuration composite 
      */
    public void addApplicationConfiguration(String name, CompositeData namedConfig) {
    }
    
    /**
      * Delete an application configuration
      *
      * @param name - identification of the application configuration to be deleted
      */
    public void deleteApplicationConfiguration(String name) {
    }
     
     /**
      * Update a application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "name" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "name" in the namedConfig
      * @param namedConfig - application configuration composite 
      */
    public void setApplicationConfiguration(String name, CompositeData namedConfig) {

    }
    
    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a 
     *         component keyed by the configuration name. 
     */
    public TabularData getApplicationConfigurations() {
        return null;
    }

}
