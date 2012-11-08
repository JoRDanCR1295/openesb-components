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

package com.sun.jbi.hl7bc.bootstrap;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.hl7bc.I18n;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 * 
 * @author aegloff
 */
public class InstallerExt implements InstallerExtMBean {

    private final Logger mLogger = Logger.getLogger(getClass().getName());

    /** Database JNDI Name setting when there is no setting defined in jbi.xml */
    private static final String DB_JNDI_NAME_FACTORYDEFAULT = "jdbc/__default"; //$NON-NLS-1$

    private Integer mThreads;

    private Integer mInboundReplyThreads;

    private String mJNDIName;

    private Integer mPoolMinSize = 2;

    private Integer mPoolMaxSize = 32;

    private Integer mConnMaxIdle = 60000;

    private Boolean mAllowDyanamicEndpoint;

    private Boolean mAlwaysCreatesNewConnection;

    private ComponentConfig mConfigProps;

    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
    }

    public Integer getThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getThreads(): " + mThreads + " threads");
        }
        return mThreads;
    }

    /**
     * setter for number of Threads
     * 
     * @param val number of threads
     */
    public void setThreads(Integer val) {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0100: setThreads():  {0} threads.", val));
        }
        mThreads = val;
    }

    public Integer getInboundReplyThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getInboundReplyThreads(): " + mInboundReplyThreads + " threads");
        }
        return mInboundReplyThreads;
    }

    /**
     * setter for number of Threads
     * 
     * @param val number of threads
     */
    public void setInboundReplyThreads(Integer val) {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, I18n.msg("I0177: setInboundReplyThreads():  {0} threads.", val));
        }
        mInboundReplyThreads = val;
    }

    /**
     * setter for JNDI name Does not allow for null value settings. Instead returns FACTORYDEFAULT
     * value.
     * 
     * @param jndiname jndi name
     */
    public void setDatabaseJNDIName(String jndiname) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Setting_jndiname" + jndiname);
        }
        if (jndiname == null) {
            jndiname = DB_JNDI_NAME_FACTORYDEFAULT;
        }
        mJNDIName = jndiname;
    }

    /**
     * getter for JNDI name. Does not allow for null value settings. Instead returns FACTORYDEFAULT
     * value.
     * 
     * @return mJNDIName JNDI name
     */
    public String getDatabaseJNDIName() {
        if (mJNDIName == null) {
            mJNDIName = DB_JNDI_NAME_FACTORYDEFAULT;
        }
        return mJNDIName;
    }

    /**
     * getter for Connection Pool Min size. Does not allow for null value settings. Instead returns DEFAULT
     * value.
     * 
     * @return mPoolMinSize mix size
     */

    public Integer getConnectionPoolMinSize() {
        return mPoolMinSize;
    }

    /**
     * setter for Connection Pool Min size Does not allow for null value settings. Instead returns DEFAULT
     * value.
     * 
     * @param jndiname jndi name
     */

    public void setConnectionPoolMinSize(Integer val) throws InvalidAttributeValueException {
        mPoolMinSize = val;
    }

    public Integer getConnectionPoolMaxSize() {
        return mPoolMaxSize;
    }

    public void setConnectionPoolMaxSize(Integer val) throws InvalidAttributeValueException {
        mPoolMaxSize = val;
    }

    public Integer getConnectionMaxIdleTimeout() {
        return mConnMaxIdle;
    }

    public void setConnectionMaxIdleTimeout(Integer val) throws InvalidAttributeValueException {
        mConnMaxIdle = val;
    }

    public Boolean getAllowDynamicEndpoint() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getAllowDynamicEndpoint(): " + mAllowDyanamicEndpoint);
        }
        return mAllowDyanamicEndpoint;
    }

    public void setAllowDynamicEndpoint(Boolean val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "setAllowDynamicEndpoint(): " + val);
        }
        mAllowDyanamicEndpoint = val;
    }

    public Boolean isAlwaysCreatesNewConnEnabled() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "isAlwaysCreatesNewConnEnabled(): " + mAlwaysCreatesNewConnection);
        }
        return mAlwaysCreatesNewConnection;
    }

    public void setAlwaysCreatesNewConnection(Boolean val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "setAlwaysCreatesNewConnection(): " + val);
        }
        mAlwaysCreatesNewConnection = val;
    }

    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        mThreads = Integer.valueOf(props.getProperty(CONFIG_THREADS).getValue());
        mInboundReplyThreads = Integer.valueOf(props.getProperty(CONFIG_INBOUNDREPLY_THREADS).getValue());
        mJNDIName = props.getProperty(CONFIG_JNDI_NAME).getValue();
        mAllowDyanamicEndpoint = Boolean.valueOf(props.getProperty(CONFIG_ALLOW_DYN_EP).getValue());
        mAlwaysCreatesNewConnection = Boolean.valueOf(props.getProperty(CONFIG_CREATESNEWCONNECTION_NAME).getValue());
        // connection pool
        mPoolMinSize = Integer.valueOf(props.getProperty(CONFIG_POOL_MIN_SZ).getValue());
        mPoolMaxSize = Integer.valueOf(props.getProperty(CONFIG_POOL_MAX_SZ).getValue());
        mConnMaxIdle = Integer.valueOf(props.getProperty(CONFIG_CONN_MAX_IDLE_TIMEOUT).getValue());
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(CONFIG_THREADS).setValue(mThreads.toString());
        mConfigProps.getProperty(CONFIG_INBOUNDREPLY_THREADS).setValue(mInboundReplyThreads.toString());
        mConfigProps.getProperty(CONFIG_JNDI_NAME).setValue(mJNDIName);
        mConfigProps.getProperty(CONFIG_ALLOW_DYN_EP).setValue(mAllowDyanamicEndpoint.toString());
        mConfigProps.getProperty(CONFIG_CREATESNEWCONNECTION_NAME).setValue(mAlwaysCreatesNewConnection.toString());
        // connection pool
        mConfigProps.getProperty(CONFIG_POOL_MIN_SZ).setValue(getConnectionPoolMinSize().toString());
        mConfigProps.getProperty(CONFIG_POOL_MAX_SZ).setValue(getConnectionPoolMaxSize().toString());
        mConfigProps.getProperty(CONFIG_CONN_MAX_IDLE_TIMEOUT).setValue(getConnectionMaxIdleTimeout().toString());
        return mConfigProps;
    }
}
