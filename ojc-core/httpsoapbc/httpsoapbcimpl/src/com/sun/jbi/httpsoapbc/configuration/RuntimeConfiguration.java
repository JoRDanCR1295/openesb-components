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
 * @(#)RuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.configuration;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.httpsoapbc.util.HttpUrlResolverUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.Notification;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanException;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ListenerNotFoundException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {
    private static final Messages mMessages =
        Messages.getMessages(RuntimeConfiguration.class);
    private static final Logger mLogger =
        Messages.getLogger(RuntimeConfiguration.class);
    
    // Default values in the absence of configuration settings
    private static final String DEFAULT_OUTBOUND_THREADS = "100";
    
    // Configuration validation settings    
    private static long MIN_OUTBOUND_THREADS = 5;
    private static long MAX_OUTBOUND_THREADS = 2147483647;
    
    private static int MIN_PORT_NUMBER = 0;
    private static int MAX_PORT_NUMBER = 65535;
    
    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
    
    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
   
    // Application configuration row fields
    private static final String APPLICATION_CONFIG_ROW_KEY = "configurationName";
    private static final String APPLICATION_CONFIG_PROPERTY_URL = "httpUrlLocation";
    
    // Configuration 
    private Properties mConfig;
    private String mWorkspaceRoot;
    
    // Global application configurations
    private Map mAppVarMap;
    private Map mAppConfigMap;
    private CompositeType mAppVarRowType = null;
    private CompositeType mAppConfigRowType = null;
    private TabularType mAppVarTabularType = null;
    private TabularType mAppConfigTabularType = null;

    // Use delegation to support notification
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();    
    
    // Key store utility for password encryption
    private KeyStoreUtilClient mKeyStoreUtilClient;
    
    public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keystoreUtilClient) throws MBeanException {
        mWorkspaceRoot = workspaceRoot;
        mKeyStoreUtilClient = keystoreUtilClient;
        
        // Load the persisted configuration
        try {
            mConfig = ConfigPersistence.loadConfig(workspaceRoot);
            mAppVarMap = loadApplicationVariablesConfig(workspaceRoot);
            mAppConfigMap = loadApplicationConfiguration(workspaceRoot);
            mAppConfigRowType = createApplicationConfigurationCompositeType();
            mAppConfigTabularType = createApplicationConfigurationTabularType();
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("HTTPBC-E01210.Faile_to_construct_composite_data_structures", e.getLocalizedMessage()));
        }
        
    }

    public Integer getOutboundThreads() {
        String val = mConfig.getProperty(CONFIG_OUTBOUND_THREADS, DEFAULT_OUTBOUND_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_OUTBOUND_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_OUTBOUND_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        
        Integer oldVal = getOutboundThreads();
        
        // Apply and save the changes
        mConfig.put(CONFIG_OUTBOUND_THREADS, val.toString()); 
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_OUTBOUND_THREADS, (oldVal != null)? oldVal.toString(): "null", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_OUTBOUND_THREADS, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public Integer getInboundThreads() {
        String val = mConfig.getProperty(CONFIG_INBOUND_THREADS, DEFAULT_OUTBOUND_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setInboundThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_INBOUND_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_INBOUND_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        
        Integer oldVal = getInboundThreads();
        
        // Apply and save the changes
        mConfig.put(CONFIG_INBOUND_THREADS, val.toString()); 
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_INBOUND_THREADS, (oldVal != null)? oldVal.toString(): "null", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
        	CONFIG_INBOUND_THREADS, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    
    public Integer getInboundReplyThreads() {
        String val = mConfig.getProperty(CONFIG_INBOUND_REPLY_THREADS, DEFAULT_OUTBOUND_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setInboundReplyThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_INBOUND_REPLY_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_INBOUND_REPLY_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        
        Integer oldVal = getInboundReplyThreads();
        
        // Apply and save the changes
        mConfig.put(CONFIG_INBOUND_REPLY_THREADS, val.toString()); 
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_INBOUND_REPLY_THREADS, (oldVal != null)? oldVal.toString(): "null", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
        	CONFIG_INBOUND_REPLY_THREADS, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public Integer getHttpDefaultPort() {
        String val = mConfig.getProperty(CONFIG_HTTP_DEFAULT_PORT, "-1");   
        if (val == null || "".equals(val)) {
            return null;
        }
        return Integer.valueOf(val);
    }
    
    public void setHttpDefaultPort(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_HTTP_DEFAULT_PORT, "null" } ));
        }
        
        if (val.intValue() < MIN_PORT_NUMBER || val.intValue() > MAX_PORT_NUMBER) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_HTTP_DEFAULT_PORT, new Integer(MIN_PORT_NUMBER),
                                                                                 new Integer(MAX_PORT_NUMBER)}));
        }
        
        Integer httpsPortVal = getHttpsDefaultPort();
        if (httpsPortVal != null && val.equals(httpsPortVal)) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01227.Invalid_http_port_same_as_https_port", val));
        }
        
        // Apply the change
        Integer oldVal = getHttpDefaultPort();
        mConfig.put(CONFIG_HTTP_DEFAULT_PORT, val.toString()); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_HTTP_DEFAULT_PORT, (oldVal != null)? oldVal.toString(): "null", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg,
                                                             CONFIG_HTTP_DEFAULT_PORT, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public Integer getHttpsDefaultPort() {
        String val = mConfig.getProperty(CONFIG_HTTPS_DEFAULT_PORT, "-1");   
        if (val == null || "".equals(val)) {
            return null;
        }
        return Integer.valueOf(val);
    }
    
    public void setHttpsDefaultPort(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_HTTPS_DEFAULT_PORT, "null" } ));
        }
        
        if (val.intValue() < MIN_PORT_NUMBER || val.intValue() > MAX_PORT_NUMBER) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_HTTPS_DEFAULT_PORT, new Integer(MIN_PORT_NUMBER),
                                                                                  new Integer(MAX_PORT_NUMBER)}));
        }
        
        Integer httpPortVal = getHttpDefaultPort();
        if (httpPortVal != null && val.equals(httpPortVal)) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01228.Invalid_https_port_same_as_http_port", val));
        }
        
        // Apply the change
        Integer oldVal = getHttpsDefaultPort();
        mConfig.put(CONFIG_HTTPS_DEFAULT_PORT, val.toString()); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_HTTPS_DEFAULT_PORT, (oldVal != null)? oldVal.toString(): "null", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_HTTPS_DEFAULT_PORT, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    
    public Boolean getClientAuthEnabled() {
        String val = mConfig.getProperty(CONFIG_CLIENT_AUTH_ENABLED);
        if (val == null || "".equals(val)) {
            return Boolean.FALSE;
        }
        
        return Boolean.valueOf(val);
    }
    
    public void setClientAuthEnabled(Boolean val) throws InvalidAttributeValueException, MBeanException {
    	// some basic validation
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_CLIENT_AUTH_ENABLED, "null" } ));
        }
        
        // Apply the change
        Boolean oldVal = getClientAuthEnabled();
        mConfig.put(CONFIG_CLIENT_AUTH_ENABLED, val.toString()); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_CLIENT_AUTH_ENABLED, (oldVal != null)? oldVal.toString(): "false", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Boolean.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_CLIENT_AUTH_ENABLED, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public Boolean getUseJVMProxySettings() {
        String val = mConfig.getProperty(CONFIG_USE_JVM_PROXY_SETTINGS);
        if (val == null || "".equals(val) || "false".equalsIgnoreCase(val)) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    public void setUseJVMProxySettings(Boolean val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_USE_JVM_PROXY_SETTINGS, "null" } ));
        }
        
        // Apply the change
        Boolean oldVal = getUseJVMProxySettings();
        mConfig.put(CONFIG_USE_JVM_PROXY_SETTINGS, val.toString()); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_USE_JVM_PROXY_SETTINGS, (oldVal != null)? oldVal.toString(): "false", val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Boolean.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_USE_JVM_PROXY_SETTINGS, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public String getProxyType() {
    	return mConfig.getProperty(CONFIG_PROXY_TYPE);
    }

    public void setProxyType(String val) throws MBeanException {
        // Apply the change
        String oldVal = getProxyType();
        mConfig.put(CONFIG_PROXY_TYPE, val); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_PROXY_TYPE, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_PROXY_TYPE, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public String getProxyHost() {
        return mConfig.getProperty(CONFIG_PROXY_HOST);
    }
    
    public void setProxyHost(String val) throws MBeanException {
        // Apply the change
        String oldVal = getProxyHost();
        mConfig.put(CONFIG_PROXY_HOST, val); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_PROXY_HOST, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_PROXY_HOST, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public Integer getProxyPort() {
        String val = mConfig.getProperty(CONFIG_PROXY_PORT);   
        if (val == null || "".equals(val)) {
            return null;
        }
        return Integer.valueOf(val);
    }
    
    public void setProxyPort(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01202.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_PROXY_PORT, "null" } ));
        }
        
        if (val.intValue() < MIN_PORT_NUMBER || val.intValue() > MAX_PORT_NUMBER) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E01201.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_PROXY_PORT, new Integer(MIN_PORT_NUMBER),
                                                                          new Integer(MAX_PORT_NUMBER)}));
        }
        
        // Apply the change
        Integer oldVal = getProxyPort();
        mConfig.put(CONFIG_PROXY_PORT, val.toString()); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_PROXY_PORT, (oldVal != null ? oldVal.toString() : "null"), val.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_PROXY_PORT, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public String getNonProxyHosts() {
        return mConfig.getProperty(CONFIG_NON_PROXY_HOSTS);    
    }
    
    public void setNonProxyHosts(String val) throws MBeanException {
        // Apply the change
        String oldVal = getNonProxyHosts();
        mConfig.put(CONFIG_NON_PROXY_HOSTS, val); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_NON_PROXY_HOSTS, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_NON_PROXY_HOSTS, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public String getProxyUserName() {
        return mConfig.getProperty(CONFIG_PROXY_USER_NAME);
    }
    
    public void setProxyUserName(String val) throws MBeanException {
        // Apply the change
        String oldVal = getProxyUserName();
        mConfig.put(CONFIG_PROXY_USER_NAME, val); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_PROXY_USER_NAME, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_PROXY_USER_NAME, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
    
    public String getProxyPassword() {
    	String val = (String)mConfig.get(CONFIG_PROXY_PASSWORD);
    	if (val != null && !"".equals(val)) {
            return "*******";
        } 
        return val;
    }
    
    public String retrieveProxyPassword(Object obj) throws MBeanException {
    	if (obj == null || obj != this) {
    	    return null;
    	}
    	
        String encryptedProxyPassword = mConfig.getProperty(CONFIG_PROXY_PASSWORD);
        try {
            if (encryptedProxyPassword != null && !"".equals(encryptedProxyPassword)) {
                return mKeyStoreUtilClient.decrypt(encryptedProxyPassword);
            }
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
        
        return encryptedProxyPassword;
    }

    
    public void setProxyPassword(String val) throws MBeanException {
        String newVal = val; 
        String oldVal = retrieveProxyPassword(this);
        try {
            if (val != null && !"".equals(val)) {
                String base64Encoded = mKeyStoreUtilClient.encrypt(val);
                mConfig.put(CONFIG_PROXY_PASSWORD, base64Encoded); 
            }
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
        
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_PROXY_PASSWORD, "********", "********" });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_PROXY_PASSWORD, attrType, "*******", "*******");
        broadcasterSupport.sendNotification(notif);
    }
     
    /**
     * Retrieves the AM config directory for Access Manager authentication
     *  @return The path to the AM config directory.
     */
    public String getAMConfigDirectory() {
        return mConfig.getProperty(CONFIG_ACCESS_MANAGER_CONFIG_DIR, "");
    }

    /**
     * Sets the AM config directory for Access Manager authentication
     * @param amConfigDir The path to the AM config directory.
     * @throws InvalidAttributeValueException if the value is invalid
     * @throws MBeanException if the operation failed due to an MBean error
     */
    public void setAMConfigDirectory(String val) throws InvalidAttributeValueException, MBeanException {
	if (val == null || "".equals(val.trim())) {
	    mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-E01232.Invalid_am_directory", val));
	    val="";
	} else {

	    if (!new File(val.trim()).exists()) {
		throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01226.Invalid_value_generic", new Object[] { val, CONFIG_ACCESS_MANAGER_CONFIG_DIR }));
	    }

	}

	String oldVal = getAMConfigDirectory();

	// Apply and save the changes
	mConfig.put(CONFIG_ACCESS_MANAGER_CONFIG_DIR, val);
	persistConfiguration();
	if (mLogger.isLoggable(Level.CONFIG)) {
	    mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail", new Object[] { CONFIG_ACCESS_MANAGER_CONFIG_DIR, (oldVal != null) ? oldVal.toString() : "null",
		    val.toString() });
	}

	// Notify listeners of this change
	long seqNo = 0;
	String msg = "Attribute changed";
	String attrType = String.class.getName();
	Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, CONFIG_ACCESS_MANAGER_CONFIG_DIR, attrType, oldVal, val);
	broadcasterSupport.sendNotification(notif);
    }
    
    
    
     /*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean#getAMClasspath()
	 */
    public String getAMClasspath() {
	return mConfig.getProperty(CONFIG_ACCESS_MANAGER_CLASSPATH, "");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean#setAMClasspath(java.lang.String)
     */
    public void setAMClasspath(String val) throws InvalidAttributeValueException, MBeanException {

	if (val == null || "".equals(val.trim())) {
	    mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-E01231.Invalid_am_classpath", val));
	    val="";
	} else {
	    if (!isValidClasspath(val)) {
		throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01226.Invalid_value_generic", new Object[] { val, CONFIG_ACCESS_MANAGER_CLASSPATH }));
	    }
	}
	String oldVal = getAMClasspath();

	// Apply and save the changes
	mConfig.put(CONFIG_ACCESS_MANAGER_CLASSPATH, val);
	persistConfiguration();
	if (mLogger.isLoggable(Level.CONFIG)) {
	    mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail", new Object[] { CONFIG_ACCESS_MANAGER_CLASSPATH, (oldVal != null) ? oldVal.toString() : "null",
		    val.toString() });
	}

	// Notify listeners of this change
	long seqNo = 0;
	String msg = "Attribute changed";
	String attrType = String.class.getName();
	Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, CONFIG_ACCESS_MANAGER_CLASSPATH, attrType, oldVal, val);
	broadcasterSupport.sendNotification(notif);

    }

    /**
      * Retrieves the comma delimited host names
      *  @return a String representing the semi-colon delimited hostnames to be allowed
      */
    public String getValidHostnames() {
        return mConfig.getProperty(CONFIG_WS_PROVIDER_HOSTNAMES);    
    }


   /**
      * Sets the comma delimited list of hostnames to be allowed for hostname validation
      * @param hostnames the commadelimited list of hostnames
      * @throws MBeanException if the operation failed due to an MBean error
      */
    public void setValidHostnames(String val) throws MBeanException {
    	if (val == null || val.trim().equals("")) {
    	    return;     // no harm, simply ignoring it...
    	}
    	
        // Apply the change
        String oldVal = getValidHostnames();
        mConfig.put(CONFIG_WS_PROVIDER_HOSTNAMES, val); 
        // Save the change
        persistConfiguration();
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "HTTPBC-C01201.Attr_changed_detail",
                    new Object[] { CONFIG_WS_PROVIDER_HOSTNAMES, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, 
                                                             CONFIG_WS_PROVIDER_HOSTNAMES, attrType, oldVal, val);
        broadcasterSupport.sendNotification(notif);
    }
      
    private boolean isValidClasspath(String val) {

	if(val == null || val.equals("")){
	    return false;
	}
	
	StringTokenizer tokenizer = new StringTokenizer(val, ",");
	
	while( tokenizer.hasMoreTokens()){
	    String token = tokenizer.nextToken();
	    
	    File f = new File (token.trim());
	    if(!f.exists()) {
		return false;
            }
	}
	
	return true;
    }
    
    /**
      * Get the CompositeType definition for the components application configuration 
      *
      * @return the CompositeType for the components application configuration.
      */
     public CompositeType queryApplicationConfigurationType() {
         return mAppConfigRowType;
     }
     
     /**
      * Add an application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "name" in the namedConfig
      * @param appConfig - application configuration composite 
      * @throws MBeanException if the application configuration cannot be added.
      */
    public void addApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {
        if (mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01211.Application_config_name_already_exists", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 2) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01212.Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        } 
        
        String appConfigValue = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_URL);
        if (appConfigValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01214.Invalid_app_config_composite_data_null_url", name));
        }
        
    	validateHttpUrl(appConfigValue);
        mAppConfigMap.put(name, appConfigValue);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01205.New_application_configuration_added", new Object[] { name, appConfigValue }));
        }
        persistApplicationConfigurationObjects();
        
    }
    
     /**
      * Delete an application configuration. 
      *
      * @param name - identification of the application configuration to be deleted
      * @throws MBeanException if the configuration cannot be deleted.
      */
    public void deleteApplicationConfiguration(String name) throws MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01215.Application_configuration_does_not_exist_for_delete", name)));
        }
        
        mAppConfigMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01206.Application_configuration_deleted", name));
        }
        persistApplicationConfigurationObjects();
    }
     
     /**
      * Update a application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "configurationName" in the appConfig
      * @param appConfig - application configuration composite
      * @throws MBeanException if there are errors encountered when updating the configuration.
      */
    public void setApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01216.Application_configuration_does_not_exist_for_set", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 2) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01212.Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        } 
        String appConfigValue = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_URL);
        if ( appConfigValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01214.Invalid_app_config_composite_data_null_url", name));
        }
        validateHttpUrl(appConfigValue);
        
        mAppConfigMap.put(name, appConfigValue);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01207.Application_configuration_updated", new Object[] { name, appConfigValue }));
        }
        persistApplicationConfigurationObjects();
    }
    
    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a 
     *         component keyed by the configuration name. 
     */
    public TabularData getApplicationConfigurations() {
        TabularData tabularData = new TabularDataSupport(mAppConfigTabularType);
        for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String value = (String) mAppConfigMap.get(name);
            Object[] data = new Object[] {name, value};
            
            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                                                                 new String[] { APPLICATION_CONFIG_ROW_KEY, APPLICATION_CONFIG_PROPERTY_URL },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("HTTPBC-E01217.Unable_to_construct_composite_data_for_app_config"), e);
            }
        }
        
        return tabularData;
    }
    
    /**
     * This operation adds a new application variable. If a variable with the same name 
     * already exists, the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variable to the 
     *         component. 
     */
    public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01218.Application_variable_name_already_exists", name)));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }
        
        if ( appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        if (mLogger.isLoggable(Level.CONFIG)) {
            if ("PASSWORD".equalsIgnoreCase(appVarType)) {
                mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01203.New_application_variable_added", new Object[] { name, "*******"} ));               
            } else {
                mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01203.New_application_variable_added", new Object[] { name, appVarValue }));
            }
        }
        persistApplicationVariablesConfig();
        
    }
    
    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
     public void deleteApplicationVariable(String name) throws MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01223.Application_variable_does_not_exist_for_delete", name)));
        }
        
        mAppVarMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01204.Application_variable_deleted", name));
        }
        persistApplicationVariablesConfig();
    }
     
    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("HTTPBC-E01224.Application_variable_does_not_exist_for_set", name)));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }
        
        if ( appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("HTTPBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        if (mLogger.isLoggable(Level.CONFIG)) {
            if ("PASSWORD".equalsIgnoreCase(appVarType)) {
                mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01202.Application_variable_updated", new Object[] { name, "*******" }));
            } else {
                mLogger.log(Level.CONFIG, mMessages.getString("HTTPBC-C01202.Application_variable_updated", new Object[] { name, appVarValue }));
            }
        }
    
        // persist the application variable properties
        persistApplicationVariablesConfig();
    }
    
    public TabularData getApplicationVariables() {
    	TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            Object[] data = ("PASSWORD".equalsIgnoreCase(type) && value != null && !"".equals(value))? 
                            new Object[] { name, "*******", type } : new Object[] {name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                                                                 new String[] { APPLICATION_VARIABLES_ROW_KEY , 
                                                                                APPLICATION_VARIABLES_VALUE_FIELD , 
                                                                                APPLICATION_VARIABLES_TYPE_FIELD },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("HTTPBC-E01225.Unable_to_construct_composite_data_for_app_variable"), e);
            }
        }
        
        return tabularData;
    }
    
    public Map retrieveApplicationVariablesMap() {
        return mAppVarMap;
    }
    
    public void updateApplicationVariablesMap(Map appVarMap) throws MBeanException {
        mAppVarMap = appVarMap;
        persistApplicationVariablesConfig();
    }
    
    public Map retrieveApplicationConfigurationsMap() {
        return mAppConfigMap;
    }
    
    public void updateApplicationConfigurationsMap(Map appConfigMap) throws MBeanException {
        mAppConfigMap = appConfigMap;
        persistApplicationConfigurationObjects();
    }
    
    private CompositeType createApplicationVariableCompositeType() throws OpenDataException {
    	if (mAppVarRowType != null) {
    	    return mAppVarRowType;
    	}
    	
        String[] appVarRowAttrNames = { APPLICATION_VARIABLES_ROW_KEY, APPLICATION_VARIABLES_VALUE_FIELD, APPLICATION_VARIABLES_TYPE_FIELD };
        String[] appVarRowAttrDesc = { "Application Variable Name", "Application Variable Value", "Application Variable Type" };
        OpenType[] appVarRowAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING };
        
        mAppVarRowType = new CompositeType("ApplicationVariables",
                                           "Application Variable Composite Data",
                                           appVarRowAttrNames,
                                           appVarRowAttrDesc,
                                           appVarRowAttrTypes);
        
        return mAppVarRowType;
    }
    
    private TabularType createApplicationVariableTabularType() throws OpenDataException {
    	if (mAppVarTabularType != null) {
            return mAppVarTabularType;
        }
        
        if (mAppVarRowType == null) {
            mAppVarRowType = createApplicationVariableCompositeType();
        }
        
        mAppVarTabularType = new TabularType("ApplicationVariableList",
                                             "List of Application Variables",
                                             mAppVarRowType,
                                             new String[] { APPLICATION_VARIABLES_ROW_KEY } );
        
        return mAppVarTabularType;
    }
    
    private CompositeType createApplicationConfigurationCompositeType() throws OpenDataException {
    	if (mAppConfigRowType != null) {
    	    return mAppConfigRowType;
    	}
    	
        String[] appConfigRowAttrNames = { APPLICATION_CONFIG_ROW_KEY, APPLICATION_CONFIG_PROPERTY_URL };
        String[] appConfigAttrDesc = { "Application Configuration Name", "HTTP URL Location" };
        OpenType[] appConfigAttrTypes = { SimpleType.STRING, SimpleType.STRING };
        
        mAppConfigRowType = new CompositeType("AppliationConfigurationObject",
                                              "Application Configuration Composite Data",
                                              appConfigRowAttrNames,
                                              appConfigAttrDesc,
                                              appConfigAttrTypes);
        
        return mAppConfigRowType;
    }
     
    private TabularType createApplicationConfigurationTabularType() throws OpenDataException {
    	if (mAppConfigTabularType != null) {
    	    return mAppConfigTabularType;
    	}
    	
    	if (mAppConfigRowType == null) {
            mAppConfigRowType = createApplicationConfigurationCompositeType();
        }
        
        mAppConfigTabularType = new TabularType("ApplicationConfigurationObjectList",
                                                "List of Application Configuration Objects",
                                                mAppConfigRowType,
                                                new String[] { APPLICATION_CONFIG_ROW_KEY });
       
       return mAppConfigTabularType;
    }
    
    private void persistApplicationVariablesConfig() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appVarPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appVarPersistFileName);
            for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String[] metadata = (String[]) mAppVarMap.get(key);
               String value = metadata[0];
               String type = metadata[1];
               if (type.equalsIgnoreCase("PASSWORD")) {
                   value = mKeyStoreUtilClient.encrypt(value);
               }
               String prop = (value != null)? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("HTTPBC-E01207.Failed_to_persist_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME)); 
        } 
    }
    
    private Map loadApplicationVariablesConfig(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map appVarMap = new HashMap();
        
        File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!appVarPersistFileName.exists()) {
            return appVarMap;
        }
        
        try {
            BufferedReader in = new BufferedReader(new FileReader(appVarPersistFileName));
            String line;
            while ((line = in.readLine()) != null) {

                //read name
                int index = line.indexOf("=");
                String name = line.substring(0, index);
                String metadata = line.substring(index + 1);

                //read value and type
                index = metadata.indexOf("{");
                String type = metadata.substring(index + 1, metadata.length() - 1);
                String value = (index == 0) ? "" : metadata.substring(0, index);
                if (type.equalsIgnoreCase("PASSWORD")) {
                    value = mKeyStoreUtilClient.decrypt(value);
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("HTTPBC-E01208.Failed_to_load_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
        
        return appVarMap;
    }
    
    private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appConfigPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String value = (String) mAppConfigMap.get(key);
               String prop = (value != null)? key + "=" + value + "\n" : key + "=\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("HTTPBC-E01229.Failed_to_persist_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME)); 
        } 
    }
    
    private Map loadApplicationConfiguration(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map appConfigMap = new HashMap();
        
        File appConfigPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        if (!appConfigPersistFileName.exists()) {
            return appConfigMap;
        }
        
        try {
            InputStream is = new FileInputStream(appConfigPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String value = persistedConfig.getProperty(name);
                appConfigMap.put(name, value);
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("HTTPBC-E01230.Failed_to_load_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }
        
        return appConfigMap; 
    }
    
    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            throw new MBeanException(ex, mMessages.getString("HTTPBC-E01209.Failed_to_persist_mbean_config", mWorkspaceRoot));
        }
    }

    void validateHttpUrl(String urlLocation) throws InvalidAttributeValueException {
        try {
            new HttpUrlResolverUtil().validateHttpUrl(urlLocation);
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(ex.getMessage());
        }	
    }
    
    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    
    public void dump(StringBuffer msgBuf) {
    	msgBuf.append(CONFIG_OUTBOUND_THREADS + " (max outbound threads)");
        msgBuf.append(": ").append(getOutboundThreads()).append('\n');
        msgBuf.append(CONFIG_INBOUND_THREADS + " (max inbound threads)");
        msgBuf.append(": ").append(getInboundThreads()).append('\n');
        msgBuf.append(CONFIG_INBOUND_REPLY_THREADS + " (max inboundReply threads)");
        msgBuf.append(": ").append(getInboundReplyThreads()).append('\n');
        msgBuf.append(CONFIG_HTTP_DEFAULT_PORT);
        msgBuf.append(": ").append(getHttpDefaultPort()).append('\n');
        msgBuf.append(CONFIG_HTTPS_DEFAULT_PORT);
        msgBuf.append(": ").append(getHttpsDefaultPort()).append('\n');
        msgBuf.append(CONFIG_CLIENT_AUTH_ENABLED);
        msgBuf.append(": ").append(getClientAuthEnabled()).append('\n');
        msgBuf.append(CONFIG_ACCESS_MANAGER_CONFIG_DIR);
        msgBuf.append(": ").append(getAMConfigDirectory()).append('\n');
        msgBuf.append(CONFIG_ACCESS_MANAGER_CLASSPATH);
        msgBuf.append(": ").append(getAMClasspath()).append('\n');
        msgBuf.append(CONFIG_PROXY_TYPE);
        msgBuf.append(": ").append(getProxyType()).append('\n');
        msgBuf.append(CONFIG_PROXY_HOST);
        msgBuf.append(": ").append(getProxyHost()).append('\n');
        msgBuf.append(CONFIG_PROXY_PORT);
        msgBuf.append(": ").append(getProxyPort()).append('\n');
        msgBuf.append(CONFIG_NON_PROXY_HOSTS);
        msgBuf.append(": ").append(getNonProxyHosts()).append('\n');
        msgBuf.append(CONFIG_PROXY_USER_NAME);
        msgBuf.append(": ").append(getProxyUserName()).append('\n');
        msgBuf.append(CONFIG_USE_JVM_PROXY_SETTINGS);
        msgBuf.append(": ").append(getUseJVMProxySettings()).append('\n');
        msgBuf.append(CONFIG_WS_PROVIDER_HOSTNAMES);
        msgBuf.append(": ").append(getValidHostnames()).append('\n');
       
        msgBuf.append(CONFIG_APPLICATON_VARIABLES).append(": { ");
        for (Iterator it = mAppVarMap.keySet().iterator(); it.hasNext(); ) {
            String name = (String)it.next();
            msgBuf.append('[').append(name).append(',');
            String[] valueType = (String[]) mAppVarMap.get(name);
            if ("PASSWORD".equalsIgnoreCase(valueType[1])) {
                msgBuf.append("*******").append(']');
            } else {
                msgBuf.append(valueType[0]).append(']');
            }
        }
        msgBuf.append(" }\n");
        msgBuf.append(CONFIG_APPLICATION_CONFIGURATIONS).append(": { ");
        for (Iterator it = mAppConfigMap.keySet().iterator(); it.hasNext(); ) {
            String name = (String)it.next();
            msgBuf.append('[').append(name).append(',');
            String value = (String) mAppConfigMap.get(name);
            msgBuf.append(value).append(']');
        }
        msgBuf.append(" }");
    }

    
}
