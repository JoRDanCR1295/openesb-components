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

package com.sun.jbi.hl7bc.bootstrap;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * MBean interface
 * 
 * @author aegloff, S. Nageswara Rao
 */
public interface InstallerExtMBean {

    public static final String CONFIG_THREADS = "Threads";

    public static final String CONFIG_INBOUNDREPLY_THREADS = "InboundReplyThreads";

    public static final String CONFIG_JNDI_NAME = "DatabaseJNDIName";

    public static final String CONFIG_ALLOW_DYN_EP = "AllowDynamicEndpoint";

    public static final String CONFIG_CREATESNEWCONNECTION_NAME = "AlwaysCreatesNewConnection";

    public static final String CONFIG_POOL_MIN_SZ = "ConnectionPoolMinSize";

    public static final String CONFIG_POOL_MAX_SZ = "ConnectionPoolMaxSize";

    public static final String CONFIG_CONN_MAX_IDLE_TIMEOUT = "ConnectionMaxIdleTimeout";

    /**
     * MBean Getter for web page configuration
     * 
     * @return number of threads
     */

    public Integer getThreads();

    /**
     * MBean Setter for web page configuration
     * 
     * @param val number of threads
     */
    public void setThreads(Integer val);

    /**
     * MBean Getter for web page configuration
     * 
     * @return number of threads
     */

    public Integer getInboundReplyThreads();

    /**
     * MBean Setter for web page configuration
     * 
     * @param val number of threads
     */
    public void setInboundReplyThreads(Integer val);

    /**
     * MBean Setter for web page configuration
     * 
     * @param flag DB JNDI Name
     */
    public void setDatabaseJNDIName(String flag);

    /**
     * MBean Getter for web page configuration
     * 
     * @return JNDI Name
     */
    public String getDatabaseJNDIName();

    /**
     * MBean Getter for the attribute AllowDynamicEndpoint
     * 
     * @return boolean
     */
    public Boolean getAllowDynamicEndpoint();

    /**
     * MBean Setter for the attribute AllowDynamicEndpoint
     * 
     * @param val
     */
    public void setAllowDynamicEndpoint(Boolean val);

    public void setInitialConfigurations(ComponentConfig props);

    public ComponentConfig getInstallationConfigurationProperties();

    /**
     * MBean Getter for the attribute Always creates new connection
     * 
     * @return boolean
     */
    public Boolean isAlwaysCreatesNewConnEnabled();

    /**
     * MBean Setter for the attribute Always creates new connection
     * 
     * @param val
     */
    public void setAlwaysCreatesNewConnection(Boolean val);

    /**
     * MBean Getter for the attribute connection min pool size
     * 
     * @return integer
     */
    public Integer getConnectionPoolMinSize();

    /**
     * MBean setter for the attribute connection min pool size
     * 
     * @param val
     */
    public void setConnectionPoolMinSize(Integer val) throws InvalidAttributeValueException;

    /**
     * MBean Getter for the attribute connection max pool size
     * 
     * @return integer
     */
    public Integer getConnectionPoolMaxSize();

    /**
     * MBean setter for the attribute connection max pool size
     * 
     * @param val
     */

    public void setConnectionPoolMaxSize(Integer val) throws InvalidAttributeValueException;

    /**
     * MBean Getter for the attribute connection max idel timeout
     * 
     * @return integer
     */

    public Integer getConnectionMaxIdleTimeout();

    /**
     * MBean setter for the attribute connection max idel timeout
     * 
     * @param val
     */
    public void setConnectionMaxIdleTimeout(Integer val) throws InvalidAttributeValueException;
}
