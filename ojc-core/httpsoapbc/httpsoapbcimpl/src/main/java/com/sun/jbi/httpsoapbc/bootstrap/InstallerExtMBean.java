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
 * @(#)InstallerExtMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;

import javax.management.InvalidAttributeValueException;

/**
 * MBean interface
 */
public interface InstallerExtMBean {
    // Attribute names
    public static final String CONFIG_OUTBOUND_THREADS = "OutboundThreads";
    public static final String CONFIG_INBOUND_THREADS = "InboundThreads";
    public static final String CONFIG_INBOUND_REPLY_THREADS = "InboundReplyThreads";
    public static final String CONFIG_HTTP_DEFAULT_PORT = "HttpDefaultPort";
    public static final String CONFIG_HTTPS_DEFAULT_PORT = "HttpsDefaultPort";
    public static final String CONFIG_CLIENT_AUTH_ENABLED = "ClientAuthEnabled";
    
    // Directory containing the Access Manager client configuration props file
    public static final String CONFIG_ACCESS_MANAGER_CONFIG_DIR = "AMConfigDirectory";
    public static final String CONFIG_ACCESS_MANAGER_CLASSPATH = "AMClasspath";
    
    // Proxy setting configurations
    public static final String CONFIG_USE_JVM_PROXY_SETTINGS = "UseJVMProxySettings";
    public static final String CONFIG_PROXY_TYPE = "ProxyType";
    public static final String CONFIG_PROXY_HOST = "ProxyHost";
    public static final String CONFIG_PROXY_PORT = "ProxyPort";
    public static final String CONFIG_NON_PROXY_HOSTS = "NonProxyHosts";
    public static final String CONFIG_PROXY_USER_NAME = "ProxyUserName";
    public static final String CONFIG_PROXY_PASSWORD = "ProxyPassword";
    
    // Allowed valid hostnames
    public static final String CONFIG_WS_PROVIDER_HOSTNAMES = "ValidHostnames";
    
    public Integer getOutboundThreads();
    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException;
    
    public Integer getInboundThreads();
    public void setInboundThreads(Integer val) throws InvalidAttributeValueException;
    
    public Integer getInboundReplyThreads();
    public void setInboundReplyThreads(Integer val) throws InvalidAttributeValueException;
    
    
    public Integer getHttpDefaultPort();
    public void setHttpDefaultPort(Integer val) throws InvalidAttributeValueException;
    
    public Integer getHttpsDefaultPort();
    public void setHttpsDefaultPort(Integer val) throws InvalidAttributeValueException;
    
    public Boolean getClientAuthEnabled();
    public void setClientAuthEnabled(Boolean val);
    
    public String getAMConfigDirectory();
    public void setAMConfigDirectory(String val);
    
    public void setAMClasspath(String val);
    public String getAMClasspath();    
    
    public Boolean getUseJVMProxySettings();
    public void setUseJVMProxySettings(Boolean val);
    
    public String getProxyType();
    public void setProxyType(String val) throws InvalidAttributeValueException;
    
    public String getProxyHost();
    public void setProxyHost(String val);
    
    public Integer getProxyPort();
    public void setProxyPort(Integer val) throws InvalidAttributeValueException;
    
    public String getNonProxyHosts();
    public void setNonProxyHosts(String val);
    
    public String getProxyUserName();
    public void setProxyUserName(String val);
    
    public String getProxyPassword();
    public void setProxyPassword(String val);
    
    public String getValidHostnames();
    public void setValidHostnames(String val);
    
    public void setInitialConfigurations(ComponentConfig props);

    public ComponentConfig getInstallationConfigurationProperties();
}

