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
 * @(#)IEPSERuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

/*
 * IEPSERuntimeConfigurationMBean.java
 *
 * Created on August 29, 2005, 3:38 PM
 *
 * To change this template, choose Tools | Options and locate the template under
 * the Source Creation and Management node. Right-click the template and choose
 * Open. You can then make changes to the template in the Source Editor.
 */

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.io.File;

import java.util.Properties;

import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

import com.sun.jbi.internationalization.Messages;

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class IEPSERuntimeConfiguration implements IEPSERuntimeConfigurationMBean, IEPConfig, NotificationEmitter {
    private static Messages mMessages = Messages.getMessages(IEPSERuntimeConfiguration.class);
    
    private File mPropFile = null;
    
    private String mEngineExpiryInterval = "" + ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT;
    private String mDatabaseNonXaJndiName = "jdbc/iepseDBNonXA";
    private String mDatabaseXaJndiName = "jdbc/iepseDBXA";
    private String mDatabaseSchemaName = "iepseDB";
    private String mGarbageCollectionEnabled = "true";

    private String mRuntimeStyle = "standAlone";
    
    private String mConfigSchema;
    private String mConfigData;
//    private String mNoOfThreads = "5";
    private String mTransactedOutput = "true";
    private String mMaximumBatchSize = "2";
    
    private NotificationBroadcasterSupport mBroadcasterSupport = new NotificationBroadcasterSupport();

    /**
     * Creates a new IEPSERuntimeConfiguration object.
     * 
     * 
     * @param propFile DOCUMENT ME!
     * @throws NotCompliantMBeanException DOCUMENT ME!
     */
    public IEPSERuntimeConfiguration(File propFile, String configSchema, String configData) throws NotCompliantMBeanException {
        mPropFile = propFile;
        mConfigSchema = configSchema;
        mConfigData = configData;
        restore();
    }

    private void saveAndNotifyListners(String msgId, String attrName, String oldValue, String newValue) {
        save();

        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString(msgId);
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, attrName,
                attrType, oldValue, newValue);
        mBroadcasterSupport.sendNotification(notif);
    }
    
    public void setEngineExpiryInterval(Integer value) throws InvalidAttributeValueException, MBeanException {
        int temp = 1;
        try {
            temp = value.intValue();
            if (temp <= 10) {
                value = ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT;
            }
        } catch (java.lang.NumberFormatException e) {
            value = ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT;
        }
        String oldValue = getEngineExpiryInterval().toString();
        mEngineExpiryInterval = value == null? ("" + ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT) : value.toString();
        saveAndNotifyListners("IEPSERuntimeConfiguration.EngineExpiryInterval_Attribute_changed",
                PROP_ENGINE_EXPIRY_INTERVAL, oldValue, mEngineExpiryInterval);
    } 
    
    public void setDatabaseNonXaJndiName(String value) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getDatabaseNonXaJndiName();
        mDatabaseNonXaJndiName = value;
        saveAndNotifyListners("IEPSERuntimeConfiguration.DbNonXaJndiName_Attribute_changed",
                PROP_DB_NON_XA_JNDI_NAME, oldValue, value);
    }

    public void setDatabaseXaJndiName(String value) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getDatabaseXaJndiName();
        mDatabaseXaJndiName = value;
        saveAndNotifyListners("IEPSERuntimeConfiguration.DbXaJndiName_Attribute_changed",
                PROP_DB_XA_JNDI_NAME, oldValue, value);
    }

    public void setDatabaseSchemaName(String value) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getDatabaseSchemaName();
        mDatabaseSchemaName = value;
        saveAndNotifyListners("IEPSERuntimeConfiguration.DbSchema_Attribute_changed",
                PROP_DB_SCHEMA, oldValue, value);
    }

    public void setGarbageCollectionEnabled(Boolean value) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getGarbageCollectionEnabled().toString();
        mGarbageCollectionEnabled = value == null? "true" : value.toString();

        saveAndNotifyListners("IEPSERuntimeConfiguration.GarbageCollectionEnabled_Attribute_changed", 
                PROP_GARBAGE_COLLECTION_ENABLED, oldValue, mGarbageCollectionEnabled);
    }
    
    public void setRuntimeStyle(String value) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getRuntimeStyle();
        mRuntimeStyle = value;
        saveAndNotifyListners("IEPSERuntimeConfiguration.RuntimeStyle_Attribute_changed",
                PROP_RUNTIME_STYLE, oldValue, value);
    }

    public Integer getEngineExpiryInterval() {
        return Integer.valueOf(mEngineExpiryInterval);
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
    
    public String getRuntimeStyle() {
        return mRuntimeStyle;
    }
    
    public void setTransactedOutput(Boolean value)throws InvalidAttributeValueException, MBeanException {
        String oldValue = getTransactedOutput().toString();
        mTransactedOutput = value == null? "true" : value.toString();
        saveAndNotifyListners("IEPSERuntimeConfiguration.TransactedOutput_Attribute_changed", 
                PROP_TRANSACTED_OUTPUT, oldValue, mTransactedOutput);
    }
    
    public Boolean getTransactedOutput() {
        return Boolean.valueOf(mTransactedOutput);
    }
    
    public void setMaximumBatchSize(Integer value) {
        int temp = 1;
        try {
            temp = value.intValue();
            
        } catch (java.lang.NumberFormatException e) {
            value = new Integer(1);
        }
        String oldValue = getMaximumBatchSize().toString();
        mMaximumBatchSize = value == null? ("" + 2) : value.toString();
        saveAndNotifyListners("IEPSERuntimeConfiguration.MaximumBatchSize_Attribute_changed",
                PROP_MAXIMUM_BATCH_SIZE, oldValue, mMaximumBatchSize);
    }
    
    public Integer getMaximumBatchSize() {
        return Integer.valueOf(mMaximumBatchSize);
    }
    
//    public void setNoOfThreads(Integer value)throws InvalidAttributeValueException, MBeanException {
//        int temp = 1;
//        try {
//            temp = value.intValue();
//            
//        } catch (java.lang.NumberFormatException e) {
//            value = new Integer(1);
//        }
//        String oldValue = getNoOfThreads().toString();
//        mNoOfThreads = value == null? ("" + 5) : value.toString();
//        saveAndNotifyListners("IEPSERuntimeConfiguration.NoOfThreads_Attribute_changed",
//                PROP_THREADS_COUNT, oldValue, mNoOfThreads);
//    }
//    public Integer getNoOfThreads() {
//        return Integer.valueOf(mNoOfThreads);
//        
//    }
   
    
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Boolean getGarbageCollectionEnabled() {
        return Boolean.valueOf(mGarbageCollectionEnabled);
    }

    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema() {
        return mConfigSchema;
    }
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData() {
        return mConfigData;
    }


    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(
                new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE},
                AttributeChangeNotification.class.getName(), 
                    mMessages.getString("IEPSERuntimeConfiguration.Attribute_changed")
            )
        };
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     * @param filter DOCUMENT ME!
     * @param handback DOCUMENT ME!
     */
    public void addNotificationListener(
        NotificationListener listener, NotificationFilter filter, Object handback
    ) {
        mBroadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     *
     * @throws ListenerNotFoundException DOCUMENT ME!
     */
    public void removeNotificationListener(NotificationListener listener)
        throws ListenerNotFoundException {
        mBroadcasterSupport.removeNotificationListener(listener);
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     * @param filter DOCUMENT ME!
     * @param handback DOCUMENT ME!
     *
     * @throws ListenerNotFoundException DOCUMENT ME!
     */
    public void removeNotificationListener(
        NotificationListener listener, NotificationFilter filter, Object handback
    ) throws ListenerNotFoundException {
        mBroadcasterSupport.removeNotificationListener(listener, filter, handback);
    }

    /**
     * DOCUMENT ME!
     *
     * @param propFile DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean restore() {
        Properties prop = new Properties();
        boolean succeed = PropertyUtil.load(prop, mPropFile);
        try {
            if (succeed) {
                setEngineExpiryInterval(Integer.valueOf(prop.getProperty(PROP_ENGINE_EXPIRY_INTERVAL)));
                setDatabaseNonXaJndiName(prop.getProperty(PROP_DB_NON_XA_JNDI_NAME));
                setDatabaseXaJndiName(prop.getProperty(PROP_DB_XA_JNDI_NAME));
                setDatabaseSchemaName(prop.getProperty(PROP_DB_SCHEMA));
                setGarbageCollectionEnabled(Boolean.valueOf(prop.getProperty(PROP_GARBAGE_COLLECTION_ENABLED)));
//                setNoOfThreads(Integer.valueOf(prop.getProperty(PROP_THREADS_COUNT)));
                setTransactedOutput(Boolean.valueOf(prop.getProperty(PROP_TRANSACTED_OUTPUT)));
                setMaximumBatchSize(Integer.valueOf(prop.getProperty(PROP_MAXIMUM_BATCH_SIZE)));
                return true;
            }    
       } catch (Throwable  ex) {
            return false;
        } 
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param propFile DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean save() {
        // Load propFile in case user has modified it.
        Properties prop = new Properties();
        boolean succeed = PropertyUtil.load(prop, mPropFile);
        if (!succeed) {
            return false;
        }

        // Set properties
        prop.setProperty(PROP_ENGINE_EXPIRY_INTERVAL, getEngineExpiryInterval().toString());
        prop.setProperty(PROP_DB_NON_XA_JNDI_NAME,getDatabaseNonXaJndiName());
        prop.setProperty(PROP_DB_XA_JNDI_NAME,getDatabaseXaJndiName());
        prop.setProperty(PROP_DB_SCHEMA,getDatabaseSchemaName());
        prop.setProperty(PROP_GARBAGE_COLLECTION_ENABLED, getGarbageCollectionEnabled().toString());
//        prop.setProperty(PROP_THREADS_COUNT,""+getNoOfThreads());
        prop.setProperty(PROP_TRANSACTED_OUTPUT, getTransactedOutput().toString()); 
        prop.setProperty(PROP_MAXIMUM_BATCH_SIZE, ""+getMaximumBatchSize());
        
        // Save the changes
        succeed = PropertyUtil.store(prop, mPropFile);
        return succeed;
    }
    
}
