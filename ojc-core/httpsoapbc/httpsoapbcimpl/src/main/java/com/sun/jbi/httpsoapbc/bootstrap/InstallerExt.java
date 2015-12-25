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
 * @(#)InstallerExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.InvalidAttributeValueException;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 */
public class InstallerExt implements InstallerExtMBean {
    private static final Messages mMessages =
        Messages.getMessages(InstallerExt.class);
    private static final Logger mLogger =
        Messages.getLogger(InstallerExt.class);
   
    // Configuration validation settings    
    private static long MIN_OUTBOUND_THREADS = 5;
    private static long MAX_OUTBOUND_THREADS = 2147483647;
    private static String PROXY_TYPE_SOCKS = "SOCKS";
    private static String PROXY_TYPE_HTTP = "HTTP";
    private static String PROXY_TYPE_DIRECT = "DIRECT";
        
    private ComponentConfig mConfigProps;
    private Integer mOutboundThreads;
    private Integer mInboundThreads; 
    private Integer mInboundReplyThreads; 
    private Integer mHttpDefaultPort;
    private Integer mHttpsDefaultPort;
    private Boolean mClientAuthEnabled;
    
    // Access Manager configuration
    private String mAMConfigDir;
    private String amClasspath;
    
    // proxy settings configurations
    private String mProxyType;
    private String mProxyHost;
    private Integer mProxyPort;
    private String mNonProxyHosts;
    private String mProxyUserName;
    private String mProxyPassword;
    private Boolean mUseJVMProxySettings;
    
    // Allowed hostnames or aliases
    private String mValidHostnames;
    
    /** Creates a new instance of InstallerExt */
    public InstallerExt() {}
    
    public Integer getOutboundThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Number of outbound threads set at install time is: " + mOutboundThreads);
        }
        return mOutboundThreads;
    }
    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException {
    	if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_OUTBOUND_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00114.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_OUTBOUND_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting number of outbound threads to: " + val);
        }
        mOutboundThreads = val;
    }
    
    
    public Integer getInboundThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Number of outbound threads set at install time is: " + mInboundThreads);
        }
        return mInboundThreads;
    }
    public void setInboundThreads(Integer val) throws InvalidAttributeValueException {
    	if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_INBOUND_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00114.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_INBOUND_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting number of outbound threads to: " + val);
        }
        mInboundThreads = val;
    }
    
    
    public Integer getInboundReplyThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Number of outbound threads set at install time is: " + mInboundReplyThreads);
        }
        return mInboundReplyThreads;
    }
    public void setInboundReplyThreads(Integer val) throws InvalidAttributeValueException {
    	if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_INBOUND_REPLY_THREADS, "null" } ));
        }
        if (val.intValue() < MIN_OUTBOUND_THREADS || val.intValue() > MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00114.Invalid_value_for_attribute",
                                    new Object[] {val, CONFIG_INBOUND_REPLY_THREADS, new Long(MIN_OUTBOUND_THREADS),
                                                                                      new Long(MAX_OUTBOUND_THREADS)}));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting number of outbound threads to: " + val);
        }
        mInboundReplyThreads = val;
    }
    
    
    
    public Integer getHttpDefaultPort() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTP Port: " + mHttpDefaultPort);
        }
        return mHttpDefaultPort;
    }
    
    public void setHttpDefaultPort(Integer val) throws InvalidAttributeValueException {
    	if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_HTTP_DEFAULT_PORT, "null" } ));
        }
        
        Integer httpPortVal = getHttpDefaultPort();
        if (httpPortVal != null && val.equals(httpPortVal)) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00113.Invalid_http_port_same_as_https_port", new Object[] {CONFIG_HTTP_DEFAULT_PORT, val} ));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting HTTP Port to: " + val);
        }
        mHttpDefaultPort = val;
    }
    
    public Integer getHttpsDefaultPort() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPS Port: " + mHttpsDefaultPort);
        }
        return mHttpsDefaultPort;
    }
    
    public void setHttpsDefaultPort(Integer val) throws InvalidAttributeValueException {
        if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_HTTPS_DEFAULT_PORT, "null" } ));
        }
        
        Integer httpPortVal = getHttpDefaultPort();
        if (httpPortVal != null && val.equals(httpPortVal)) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00115.Invalid_https_port_same_as_http_port", new Object[] {CONFIG_HTTPS_DEFAULT_PORT, val} ));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting HTTPS Port to: " + val);
        }
        mHttpsDefaultPort = val;
    }
    
    public Boolean getClientAuthEnabled() {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "ClientAuthEnabled flag: " + mClientAuthEnabled);
        }
        return mClientAuthEnabled;
    }
    
    public void setClientAuthEnabled(Boolean val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Settng ClientAuthEnabled flag to: " + val);
        }
        mClientAuthEnabled = val;
    }
    
    public String getAMConfigDirectory() {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Access Manager Configuration Directory: " + mAMConfigDir);
        }
        return mAMConfigDir;
    }
    
    public void setAMConfigDirectory(String val) {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Access Manager Configuration Directory to: " + val);
        }
        mAMConfigDir = val; 
    }
    
    public String getAMClasspath() {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Access Manager classpath: " + amClasspath);
        }
        return amClasspath;
    }
    
    public void setAMClasspath(String val) {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Access Manager Classpath to: " + val);
        }
    	amClasspath = val; 
    }
    
    public Boolean getUseJVMProxySettings() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "JVM Proxy Settings flag: " + mUseJVMProxySettings);
        }
        return mUseJVMProxySettings;
    }
    
    public void setUseJVMProxySettings(Boolean val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Settng JVM Proxy Settings flag to: " + val);
        }
        mUseJVMProxySettings = val;
    }
    
    public String getProxyType() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Proxy type: " + mProxyType);
        }
        return mProxyType;
    }
    
    public void setProxyType(String val) throws InvalidAttributeValueException {
    	if (val != null && 
    	    !val.equals(PROXY_TYPE_SOCKS) &&
    	    !val.equals(PROXY_TYPE_HTTP) &&
    	    !val.equals(PROXY_TYPE_DIRECT)) {
    	    throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_PROXY_TYPE, val} ));
    	}
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Proxy type to: " + val);
        }
        mProxyType = val;
    }
    
    public String getProxyHost() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Proxy Host: " + mProxyHost);
        }
        return mProxyHost;
    }
    
    public void setProxyHost(String val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Proxy Host to: " +  val);
        }
        mProxyHost = val;
    }
    
    public Integer getProxyPort() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Proxy Port: " + mProxyPort);
        }
        return mProxyPort;
    }
    
    public void setProxyPort(Integer val) throws InvalidAttributeValueException {
    	if (val == null) {
            throw new InvalidAttributeValueException(
                mMessages.getString("HTTPBC-E00112.Invalid_argument_for_setting_attribute",
                        new Object[] { CONFIG_PROXY_PORT, "null" } ));
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Proxy Port to: " + val);
        }
        mProxyPort = val;
    }
    
    public String getNonProxyHosts() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Non-Proxy Hosts: " + mNonProxyHosts);
        }
        return mNonProxyHosts;
    }
    
    public void setNonProxyHosts(String val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Non-Proxy Hosts to: " + val);
        }
        mNonProxyHosts = val;
    }
    
    public String getProxyUserName() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Proxy User Name: " + mProxyUserName);
        }
        return mProxyUserName;
    }
    
    public void setProxyUserName(String val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Setting Proxy User Name to: " + val);
        }
        mProxyUserName = val;
    }
    
    public String getProxyPassword() {
        return mProxyPassword;
    }
    
    public void setProxyPassword(String val) {
        mProxyPassword = val;
    }
    
    public String getValidHostnames() {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Valid hostnames: " + mValidHostnames);
        }
        return mValidHostnames;
    }
    
    public void setValidHostnames(String val) {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Setting valid hostnames to: " + val);
        }
        mValidHostnames = val;
    }
    
    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        
        mOutboundThreads = Integer.valueOf(props.getProperty(CONFIG_OUTBOUND_THREADS).getValue());
        mInboundThreads = Integer.valueOf(props.getProperty(CONFIG_INBOUND_THREADS).getValue());
        mInboundReplyThreads = Integer.valueOf(props.getProperty(CONFIG_INBOUND_REPLY_THREADS).getValue());
        mHttpDefaultPort = Integer.valueOf(props.getProperty(CONFIG_HTTP_DEFAULT_PORT).getValue());
        mHttpsDefaultPort = Integer.valueOf(props.getProperty(CONFIG_HTTPS_DEFAULT_PORT).getValue());
        mClientAuthEnabled = Boolean.valueOf(props.getProperty(CONFIG_CLIENT_AUTH_ENABLED).getValue());
        mAMConfigDir = props.getProperty(CONFIG_ACCESS_MANAGER_CONFIG_DIR).getValue();
        amClasspath = props.getProperty(CONFIG_ACCESS_MANAGER_CLASSPATH).getValue();
        mProxyType = props.getProperty(CONFIG_PROXY_TYPE).getValue();
        mProxyHost = props.getProperty(CONFIG_PROXY_HOST).getValue();
        mProxyPort = Integer.valueOf(props.getProperty(CONFIG_PROXY_PORT).getValue());
        mNonProxyHosts = props.getProperty(CONFIG_NON_PROXY_HOSTS).getValue();
        mProxyUserName = props.getProperty(CONFIG_PROXY_USER_NAME).getValue();
        mProxyPassword = props.getProperty(CONFIG_PROXY_PASSWORD).getValue();
        mUseJVMProxySettings = Boolean.valueOf(props.getProperty(CONFIG_USE_JVM_PROXY_SETTINGS).getValue());
        mValidHostnames = props.getProperty(CONFIG_WS_PROVIDER_HOSTNAMES).getValue();
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(CONFIG_OUTBOUND_THREADS).setValue(mOutboundThreads.toString());
        mConfigProps.getProperty(CONFIG_INBOUND_THREADS).setValue(mInboundThreads.toString());
        mConfigProps.getProperty(CONFIG_INBOUND_REPLY_THREADS).setValue(mInboundReplyThreads.toString());
        mConfigProps.getProperty(CONFIG_HTTP_DEFAULT_PORT).setValue(mHttpDefaultPort.toString());
        mConfigProps.getProperty(CONFIG_HTTPS_DEFAULT_PORT).setValue(mHttpsDefaultPort.toString());
        mConfigProps.getProperty(CONFIG_CLIENT_AUTH_ENABLED).setValue(mClientAuthEnabled.toString());
        mConfigProps.getProperty(CONFIG_ACCESS_MANAGER_CONFIG_DIR).setValue(mAMConfigDir);
        mConfigProps.getProperty(CONFIG_ACCESS_MANAGER_CLASSPATH).setValue(amClasspath);
        mConfigProps.getProperty(CONFIG_PROXY_TYPE).setValue(mProxyType);
        mConfigProps.getProperty(CONFIG_PROXY_HOST).setValue(mProxyHost);
        mConfigProps.getProperty(CONFIG_PROXY_PORT).setValue(mProxyPort.toString());
        mConfigProps.getProperty(CONFIG_NON_PROXY_HOSTS).setValue(mNonProxyHosts);        
        mConfigProps.getProperty(CONFIG_PROXY_USER_NAME).setValue(mProxyUserName);               
        mConfigProps.getProperty(CONFIG_PROXY_PASSWORD).setValue(mProxyPassword);  
        mConfigProps.getProperty(CONFIG_USE_JVM_PROXY_SETTINGS).setValue(mUseJVMProxySettings.toString());     
        mConfigProps.getProperty(CONFIG_WS_PROVIDER_HOSTNAMES).setValue(mValidHostnames);        
        
        return mConfigProps;
    }
}
