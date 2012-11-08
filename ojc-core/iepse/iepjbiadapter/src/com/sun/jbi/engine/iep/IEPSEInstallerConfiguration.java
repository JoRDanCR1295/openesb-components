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
 * @(#)IEPSEInstallerConfiguration.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;
import java.util.Properties;
import java.io.File;
import java.util.logging.Logger;
import com.sun.jbi.common.qos.config.ComponentConfig;


/**
 * IEPSEInstallerConfiguration.java
 *
 * Created on May 23, 2005, 1:49 PM
 *
 *
 *
 * @author Bing Lu
 */
public class IEPSEInstallerConfiguration extends StandardMBean implements IEPSEInstallerConfigurationMBean, IEPConfig {
    private String mEngineExpiryInterval = "" + ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT;
    private String mDatabaseNonXaJndiName = "jdbc/iepseDerbyNonXA";
    private String mDatabaseXaJndiName = "jdbc/iepseDerbyXA";
    private String mDatabaseSchemaName = "iepseDB";

    private String mAieType = "none";
    private String mAieHostname = "architect1.sun.com";
    private String mAiePort = "6688";
    private String mAieId = "aie01";

    private String mRuntimeStyle = "standAlone";
    private String mGarbageCollectionEnabled = "true";
//    private String mNoOfThreads = "" + 5;
    private String mTransactedOutput = "true";
    
    private String mMaximumBatchSize = "2";
    
    private ComponentConfig mComponentConfig = null;
    
    private static Logger mLogger = Logger.getLogger("" + IEPSEInstallerConfiguration.class.getName());

    public IEPSEInstallerConfiguration() throws NotCompliantMBeanException {
        super(IEPSEInstallerConfigurationMBean.class);
    }

    public void setEngineExpiryInterval(String engineExpiryInterval) {
        mEngineExpiryInterval = engineExpiryInterval;
    }

    public void setDatabaseNonXaJndiName(String dbJndiName) {
        mDatabaseNonXaJndiName = dbJndiName;
    }

    public void setDatabaseXaJndiName(String dbJndiName) {
        mDatabaseXaJndiName = dbJndiName;
    }

    public void setDatabaseSchemaName(String dbSchema) {
        mDatabaseSchemaName = dbSchema;
    }

    public void setAieType(String aieType) {
        mAieType = aieType;
    }

    public void setAieHostname(String aieHostname) {
        mAieHostname = aieHostname;
    }

    public void setAiePort(String aiePort) {
        mAiePort = aiePort;
    }

    public void setAieId(String aieId) {
        mAieId = aieId;
    }

    public void setRuntimeStyle(String runtimeStyle) {
        mRuntimeStyle = runtimeStyle;
    }

    public void setGarbageCollectionEnabled(String garbageCollectionEnabled) {
        mGarbageCollectionEnabled = garbageCollectionEnabled;
    }

    public String getEngineExpiryInterval() {
        return mEngineExpiryInterval;
    }

    public String getDatabaseNonXaJndiName() {
        return mDatabaseNonXaJndiName;
    }

    public String getDatabaseXaJndiName() {
        return mDatabaseXaJndiName;
    }

    public String getDatabaseSchemaName() {
        return mDatabaseSchemaName;
    }

    public String getAieType() {
        return mAieType;
    }

    public String getAieHostname() {
        return mAieHostname;
    }

    public String getAiePort() {
        return mAiePort;
    }

    public String getAieId() {
        return mAieId;
    }

    public String getRuntimeStyle() {
        return mRuntimeStyle;
    }

    public String getGarbageCollectionEnabled() {
        return mGarbageCollectionEnabled;
    }
    
//    public void setNoOfThreads(String value){
//        mNoOfThreads = value;
//    }
//    public String getNoOfThreads() {
//        return mNoOfThreads;
//    }
//    
    public void setTransactedOutput(String value){
        mTransactedOutput = value;
    }
    public String getTransactedOutput() {
        return mTransactedOutput;
    }
    
    public void setMaximumBatchSize(String value) {
        mMaximumBatchSize = value;
    }
    
    public String getMaximumBatchSize() {
        return mMaximumBatchSize;
    }

    /*  Properties getProperties() {
        Properties bProp = new Properties();

        Iterator<Property> propIterator = prop.propertySet().iterator();
        while (propIterator.hasNext()) {
            Property componentProp = propIterator.next();
            bProp.put(componentProp.getName(), componentProp.getValue());
        }
        return bProp;
    } */
    private Properties getProperties() {
        Properties prop = new Properties();
        prop.setProperty(PROP_ENGINE_EXPIRY_INTERVAL, getEngineExpiryInterval());
        
        prop.setProperty(PROP_DB_NON_XA_JNDI_NAME, getDatabaseNonXaJndiName());
        prop.setProperty(PROP_DB_XA_JNDI_NAME, getDatabaseXaJndiName());
        prop.setProperty(PROP_DB_SCHEMA, getDatabaseSchemaName());

        prop.setProperty(PROP_AIE_TYPE, getAieType());
        prop.setProperty(PROP_AIE_HOSTNAME, getAieHostname());
        prop.setProperty(PROP_AIE_PORT, getAiePort());
        prop.setProperty(PROP_AIE_ID, getAieId());

        prop.setProperty(PROP_RUNTIME_STYLE, getRuntimeStyle());
        prop.setProperty(PROP_GARBAGE_COLLECTION_ENABLED, getGarbageCollectionEnabled());
//        prop.setProperty(PROP_THREADS_COUNT, getNoOfThreads());
        prop.setProperty(PROP_TRANSACTED_OUTPUT, getTransactedOutput());
        prop.setProperty(PROP_MAXIMUM_BATCH_SIZE, getMaximumBatchSize());
        
        return prop;
    }
    
    public ComponentConfig getComponentConfig() {
        
        mComponentConfig.getProperty(PROP_ENGINE_EXPIRY_INTERVAL).setValue(getEngineExpiryInterval());
        
        mComponentConfig.getProperty(PROP_DB_NON_XA_JNDI_NAME).setValue(getDatabaseNonXaJndiName());
        mComponentConfig.getProperty(PROP_DB_XA_JNDI_NAME).setValue(getDatabaseXaJndiName());
        mComponentConfig.getProperty(PROP_DB_SCHEMA).setValue( getDatabaseSchemaName());

        mComponentConfig.getProperty(PROP_AIE_TYPE).setValue( getAieType());
        mComponentConfig.getProperty(PROP_AIE_HOSTNAME).setValue( getAieHostname());
        mComponentConfig.getProperty(PROP_AIE_PORT).setValue( getAiePort());
        mComponentConfig.getProperty(PROP_AIE_ID).setValue( getAieId());

        mComponentConfig.getProperty(PROP_RUNTIME_STYLE).setValue( getRuntimeStyle());
        mComponentConfig.getProperty(PROP_GARBAGE_COLLECTION_ENABLED).setValue( getGarbageCollectionEnabled());
//        prop.getProperty(PROP_THREADS_COUNT).setValue( getNoOfThreads());
        mComponentConfig.getProperty(PROP_TRANSACTED_OUTPUT).setValue( getTransactedOutput());
        mComponentConfig.getProperty(PROP_MAXIMUM_BATCH_SIZE).setValue(getMaximumBatchSize());
        
        return mComponentConfig;
    }

    /**
     * Save the configuration to persistent storage.
     * @return 0 if successful.
     */
    public boolean save(File propFile) {
        Properties prop = getProperties();
        boolean succeed = PropertyUtil.store(prop, propFile);
        return succeed;
    }

    public void restore(Properties prop) {
        
        setEngineExpiryInterval(prop.getProperty(PROP_ENGINE_EXPIRY_INTERVAL,"60"));
        setDatabaseNonXaJndiName(prop.getProperty(PROP_DB_NON_XA_JNDI_NAME));
        setDatabaseXaJndiName(prop.getProperty(PROP_DB_XA_JNDI_NAME));
        setDatabaseSchemaName(prop.getProperty(PROP_DB_SCHEMA));

//        UNCOMMENT when AI engine plugin is enabled again
//        setAieType(prop.getProperty(PROP_AIE_TYPE));
//        setAieHostname(prop.getProperty(PROP_AIE_HOSTNAME));
//        setAiePort(prop.getProperty(PROP_AIE_PORT));
//        setAieId(prop.getProperty(PROP_AIE_ID));
        
        setRuntimeStyle(prop.getProperty(PROP_RUNTIME_STYLE));
        setGarbageCollectionEnabled(prop.getProperty(PROP_GARBAGE_COLLECTION_ENABLED));
//        setNoOfThreads(prop.getProperty(PROP_THREADS_COUNT));
        setTransactedOutput(prop.getProperty(PROP_TRANSACTED_OUTPUT));
        setMaximumBatchSize(prop.getProperty(PROP_MAXIMUM_BATCH_SIZE));
    }
 
    public void setValues(ComponentConfig prop) {
        this.mComponentConfig = prop;
        
        setEngineExpiryInterval(prop.getProperty(PROP_ENGINE_EXPIRY_INTERVAL).getValue());
        setDatabaseNonXaJndiName(prop.getProperty(PROP_DB_NON_XA_JNDI_NAME).getValue());
        setDatabaseXaJndiName(prop.getProperty(PROP_DB_XA_JNDI_NAME).getValue());
        setDatabaseSchemaName(prop.getProperty(PROP_DB_SCHEMA).getValue());

//        UNCOMMENT when AI engine plugin is enabled again
//        setAieType(prop.getProperty(PROP_AIE_TYPE).getValue());
//        setAieHostname(prop.getProperty(PROP_AIE_HOSTNAME).getValue());
//        setAiePort(prop.getProperty(PROP_AIE_PORT).getValue());
//        setAieId(prop.getProperty(PROP_AIE_ID).getValue());

        setRuntimeStyle(prop.getProperty(PROP_RUNTIME_STYLE).getValue());
        setGarbageCollectionEnabled(prop.getProperty(PROP_GARBAGE_COLLECTION_ENABLED).getValue());
//        setNoOfThreads(prop.getProperty(PROP_THREADS_COUNT).getValue());
        setTransactedOutput(prop.getProperty(PROP_TRANSACTED_OUTPUT).getValue()); 
        setMaximumBatchSize(prop.getProperty(PROP_MAXIMUM_BATCH_SIZE).getValue());
    }

    /**
     * Restore the configuration from persistent storage.
     * @return 0 if successful.
     */
    public boolean restore(String propFile) {
        Properties prop = new Properties();
        boolean succeed = PropertyUtil.load(prop, new File(propFile));
        if (succeed) {
            restore(prop);
            return true;
        }
        return false;
    }
}
