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
package com.sun.jbi.ftpbc.bootstrap;

import com.sun.jbi.ftpbc.FTPBCComponentContext;

import com.sun.jbi.common.qos.config.ComponentConfig;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

/**
 * This allows configuration to be changed before installation
 * @author jfu
 */
public class InstallerExt implements InstallerExtMBean {

    private ComponentConfig mConfigProps;
    private Integer mOutboundThreads;
    private Integer mInvokeTimeout;
    private Boolean mUseProxy;
    private Boolean mUsePassiveFTP;
    private String mProxyURL;
    private String mProxyUserID;
    private String mProxyUserPassword;
    private Integer mPoolMinSize;
    private Integer mPoolMaxSize;
    private Integer mConnMaxIdle;
    private Boolean bEnableNMProps;
    private Boolean bEnableClusterAware;
    private String mTokenPersistenceURL;
    private String mTokenDBJDBCDriverClazz;

    public InstallerExt() {
    }

    public Integer getOutboundThreads() {
        return mOutboundThreads != null ? mOutboundThreads : new Integer(FTPBCComponentContext.MIN_OUTBOUND_THREADS);
    }

    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException {
        mOutboundThreads = val;
    }

    public Integer getInvokeTimeout() {
        return mInvokeTimeout != null ? mInvokeTimeout : new Integer(500);
    }

    public void setInvokeTimeout(Integer val) throws InvalidAttributeValueException {
        mInvokeTimeout = val;
    }

    public Boolean getUseProxy() {
        return mUseProxy != null ? mUseProxy : new Boolean(false);
    }

    public void setUseProxy(Boolean val) throws InvalidAttributeValueException {
        mUseProxy = val;
    }

    public Boolean getUsePassiveFTP() {
        return mUsePassiveFTP != null ? mUsePassiveFTP : new Boolean(true);
    }

    public void setUsePassiveFTP(Boolean val) throws InvalidAttributeValueException {
        mUsePassiveFTP = val;
    }

    public String getProxyURL() {
        return mProxyURL != null ? mProxyURL : "";
    }

    public void setProxyURL(String val) throws InvalidAttributeValueException {
        mProxyURL = val;
    }

    public String getProxyUserID() {
        return mProxyUserID != null ? mProxyUserID : "";
    }

    public void setProxyUserID(String val) throws InvalidAttributeValueException {
        mProxyUserID = val;
    }

    public String getProxyUserPassword() {
        return mProxyUserPassword != null ? mProxyUserPassword : "";
    }

    public void setProxyUserPassword(String val) throws InvalidAttributeValueException {
        mProxyUserPassword = val;
    }

    public Integer getConnectionPoolMinSize() {
        return mPoolMinSize;
    }

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

    public Boolean getEnableNMProps() {
        return bEnableNMProps;
    }

    public void setEnableNMProps(Boolean val) throws InvalidAttributeValueException, MBeanException {
        bEnableNMProps = val;
    }

    public Boolean getEnableClusterAware() {
        return bEnableClusterAware;
    }

    public void setEnableClusterAware(Boolean val) throws InvalidAttributeValueException, MBeanException {
        bEnableClusterAware = val;
    }

    public String getTokenPersistenceURL() {
        return mTokenPersistenceURL != null ? mTokenPersistenceURL : "";
    }

    public void setTokenPersistenceURL(String val) throws InvalidAttributeValueException {
        mTokenPersistenceURL = val;
    }

    public String getTokenDBJDBCDriverClass() {
        return mTokenDBJDBCDriverClazz != null ? mTokenDBJDBCDriverClazz : "";
    }

    public void setTokenDBJDBCDriverClass(String val) throws InvalidAttributeValueException {
        mTokenDBJDBCDriverClazz = val;
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_OUTBOUND_THREADS).setValue(getOutboundThreads().toString());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT).setValue(getInvokeTimeout().toString());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_PROXY_URL).setValue(getProxyURL());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_ID).setValue(getProxyUserID());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_PASSWD).setValue(getProxyUserPassword());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_USE_PASSIVE_FTP).setValue(getUsePassiveFTP().toString());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_USE_PROXY).setValue(getUseProxy().toString());

        // connection pool
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_POOL_MIN_SZ).setValue(getConnectionPoolMinSize().toString());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_POOL_MAX_SZ).setValue(getConnectionPoolMaxSize().toString());
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_CONN_MAX_IDLE_TIMEOUT).setValue(getConnectionMaxIdleTimeout().toString());

        // enable NM props
        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_ENABLE_NM_PROPS).setValue(getEnableNMProps().toString());
        // clustering aware support
//        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_ENABLE_CLUSTER_AWARE).setValue(getEnableClusterAware().toString());
//        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_TOKEN_PERSIST_URL).setValue(getTokenPersistenceURL());
//        mConfigProps.getProperty(FTPBCComponentContext.CONFIG_DB_DRV_CLASS).setValue(getTokenDBJDBCDriverClass());

        return mConfigProps;
    }

    public void setInitialConfigurations(ComponentConfig props) {
        mOutboundThreads = Integer.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_OUTBOUND_THREADS).getValue());
        mInvokeTimeout = Integer.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT).getValue());
        mUseProxy = Boolean.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_USE_PROXY).getValue());
        mUsePassiveFTP = Boolean.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_USE_PASSIVE_FTP).getValue());
        mProxyURL = props.getProperty(FTPBCComponentContext.CONFIG_PROXY_URL).getValue();
        mProxyUserID = props.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_ID).getValue();
        mProxyUserPassword = props.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_PASSWD).getValue();

        // connection pool
        mPoolMinSize = Integer.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_POOL_MIN_SZ).getValue());
        mPoolMaxSize = Integer.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_POOL_MAX_SZ).getValue());
        mConnMaxIdle = Integer.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_CONN_MAX_IDLE_TIMEOUT).getValue());
        // enable NM props
        bEnableNMProps = Boolean.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_ENABLE_NM_PROPS).getValue());
        // clustering aware support
//        bEnableClusterAware = Boolean.valueOf(props.getProperty(FTPBCComponentContext.CONFIG_ENABLE_CLUSTER_AWARE).getValue());
//        mTokenPersistenceURL = props.getProperty(FTPBCComponentContext.CONFIG_TOKEN_PERSIST_URL).getValue();
//        mTokenDBJDBCDriverClazz = props.getProperty(FTPBCComponentContext.CONFIG_DB_DRV_CLASS).getValue();

        mConfigProps = props;
    }
}
