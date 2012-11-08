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
 * @(#)SQLSERuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.util.Properties;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import com.sun.jbi.configuration.ConfigPersistence;


/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 *
 */
public class SQLSERuntimeConfiguration implements SQLSERuntimeConfigurationMBean,
    NotificationEmitter {
    private static final Logger mLogger = Logger.getLogger(SQLSERuntimeConfiguration.class.getName());

    // Attribute names
    public static final String CONFIG_THREADS = "Threads";

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";

    // Configuration validation settings    
    long MIN_THREADS = 1;
    long MAX_THREADS = 10000;

    // Configuration 
    Properties mConfig;
    String mWorkspaceRoot;
    
    // config schema and config data
    private String mConfigSchema;
    private String mConfigData;
    
    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    /** Creates a new instance of InstallerExt */
    SQLSERuntimeConfiguration(final String workspaceRoot) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
    }

    /**
     * @return
     */
    //@Override
    public Integer getThreads() {
        final String val = mConfig.getProperty(SQLSERuntimeConfiguration.CONFIG_THREADS, SQLSERuntimeConfiguration.DEFAULT_THREADS);

        return Integer.valueOf(val);
    }

    /**
     *
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    //@Override
    public void setThreads(final Integer val)
        throws InvalidAttributeValueException, MBeanException {
        final String attrName = SQLSERuntimeConfiguration.CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;

        try {
            newVal = val;
        } catch (final Exception ex) {
            throw new InvalidAttributeValueException(
                "Invalid argument for setting attribute " + attrName + " :" +
                ex.getMessage());
        }

        if ((newVal.intValue() < MIN_THREADS) ||
                (newVal.intValue() > MAX_THREADS)) {
            throw new InvalidAttributeValueException("A value of " + newVal +
                " is not valid for attribute " + attrName +
                ". The valid range is " + MIN_THREADS + " - " + MAX_THREADS);
        }

        // Apply and save the changes
        mConfig.put(SQLSERuntimeConfiguration.CONFIG_THREADS, val.toString());
        persistConfiguration();

        // Notify listeners of this change
        final long seqNo = 0;
        final String msg = "Attribute changed";
        final String attrType = Integer.class.getName();
        final Integer oldVal = getThreads();
        final Notification notif = new AttributeChangeNotification(this, seqNo,
                System.currentTimeMillis(), msg, attrName, attrType, oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);
    }

    /**
     *
     * @throws MBeanException
     */
    private void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (final JBIException ex) {
            throw new MBeanException(ex,
                "Failed to persist configuration to " + mWorkspaceRoot + ": " +
                ex.getMessage());
        }
    }

    /**
     *
     * @return
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(new String[] {
                    AttributeChangeNotification.ATTRIBUTE_CHANGE
                }, AttributeChangeNotification.class.getName(),
                "Attribute changed")
        };
    }

    /**
     *
     * @param listener
     * @param filter
     * @param handback
     */
    public void addNotificationListener(final NotificationListener listener,
        final NotificationFilter filter, final Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /**
     *
     * @param listener
     * @throws ListenerNotFoundException
     */
    public void removeNotificationListener(final NotificationListener listener)
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    /**
     *
     * @param listener
     * @param filter
     * @param handback
     * @throws ListenerNotFoundException
     */
    //@Override
    public void removeNotificationListener(final NotificationListener listener,
        final NotificationFilter filter, final Object handback)
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    
       
    // addition of the configuration display data 
    
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

    // addition of the configuration display data 
    
     /**
     * Retrieves the configuration display schema
     */
    public void setConfigurationDisplaySchema(String mConfigSchema) {
        this.mConfigSchema = mConfigSchema;
    }
    
    /**
     * Retrieves the configuration display data
     */
    public void setConfigurationDisplayData(String mConfigData) {
        this.mConfigData = mConfigData;
    }
    
}
