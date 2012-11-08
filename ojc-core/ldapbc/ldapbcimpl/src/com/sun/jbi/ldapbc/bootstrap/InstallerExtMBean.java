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

package com.sun.jbi.ldapbc.bootstrap;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

import com.sun.jbi.common.qos.config.ComponentConfig;


/**
 * MBean interface
 */
public interface InstallerExtMBean {
	public static final String CONFIG_THREADS = "Threads";
	public static final String CONFIG_MAX_RETRIES_COUNT = "RetryCount";
	public static final String CONFIG_RETRY_INTERVAL = "RetryInterval";
	public static final String CONFIG_RECOURCE_ACTION = "RecoveryType";
	public static final String CONFIG_ALLOW_DYNAMIC_ENDPOINT = "AllowDynamicEndpoint";
	public static final String CONFIG_ALLOW_CONNECTION_POOLING = "AllowConnectionPooling";
	public static final String CONFIG_CONNPOOL_PREF_SIZE = "ConnectionPoolPrefSize";
	public static final String CONFIG_CONNPOOL_MAX_SIZE = "ConnectionPoolMaxSize";
	public static final String CONFIG_CONNPOOL_MAX_IDLE_TIME = "ConnectionMaxIdleTimeout";
	public static final String CONFIG_CONNPOOL_PROTOCOL = "ConnectionProtocol";
	public static final String CONFIG_CONNPOOL_AUTHENTICATION = "ConnectionAuthentication";
    public Integer getThreads();
    public void setThreads(Integer val);
    public String getUrl();
    public void setUrl(String url);
    public String getUsername();
    public void setUsername(String name);
    public String getPassword();
    public void setPassword(String password);
	public void setRetryCount(Integer val);
	public Integer getRetryCount();
	public Integer getRetryInterval();
	public void setRetryInterval(Integer val);
	public String getRecoveryType();
	public void setRecoveryType(String val);
	public void setInitialConfigurations(ComponentConfig props);
	public ComponentConfig getInstallationConfigurationProperties();
    public Boolean getAllowDynamicEndpoint();
    public void setAllowDynamicEndpoint(Boolean val) throws InvalidAttributeValueException, MBeanException;	
	public Boolean getAllowConnectionPooling();
	public void setAllowConnectionPooling(Boolean connPool);
	public Integer getConnectionPoolPrefSize();
	public void setConnectionPoolPrefSize(Integer prefSize);
	public Integer getConnectionPoolMaxSize();
	public void setConnectionPoolMaxSize(Integer maxSize);
	public Integer getConnectionMaxIdleTimeout();
	public void setConnectionMaxIdleTimeout(Integer maxIdleTime);
	public String getConnectionProtocol();
	public void setConnectionProtocol(String protocol);
	public String getConnectionAuthentication();
	public void setConnectionAuthentication(String auth);
}