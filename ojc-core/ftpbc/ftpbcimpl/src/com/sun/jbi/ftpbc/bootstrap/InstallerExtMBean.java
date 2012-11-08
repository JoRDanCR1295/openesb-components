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
package com.sun.jbi.ftpbc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

/**
 * InstallerExt MBean interface.
 *
 * @author Sun Microsystem
 */
public interface InstallerExtMBean {
    /**
     * 
     * @return 
     */
    public Integer getOutboundThreads();

    /**
     * 
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException;

    /**
     * 
     * @return 
     */
    public Integer getInvokeTimeout();

    /**
     * 
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    public void setInvokeTimeout(Integer val) throws InvalidAttributeValueException;

    /**
     * 
     * @return 
     */
    public Boolean getUseProxy();

    /**
     * 
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    public void setUseProxy(Boolean val) throws InvalidAttributeValueException;

    /**
     * 
     * @return 
     */
    public Boolean getUsePassiveFTP();

    /**
     * 
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    public void setUsePassiveFTP(Boolean val) throws InvalidAttributeValueException;

    /**
     * 
     * @return 
     */
    public String getProxyURL();

    /**
     * 
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    public void setProxyURL(String val) throws InvalidAttributeValueException;

    public String getProxyUserID();

    public void setProxyUserID(String val) throws InvalidAttributeValueException;

    public String getProxyUserPassword() throws Exception;

    public void setProxyUserPassword(String val) throws InvalidAttributeValueException;

    public Integer getConnectionPoolMinSize();

    public void setConnectionPoolMinSize(Integer val) throws InvalidAttributeValueException;

    public Integer getConnectionPoolMaxSize();

    public void setConnectionPoolMaxSize(Integer val) throws InvalidAttributeValueException;

    public Integer getConnectionMaxIdleTimeout();

    public void setConnectionMaxIdleTimeout(Integer val) throws InvalidAttributeValueException;

    public Boolean getEnableNMProps();

    public void setEnableNMProps(Boolean val) throws InvalidAttributeValueException, MBeanException;

//    public Boolean getEnableClusterAware();
//
//    public void setEnableClusterAware(Boolean val) throws InvalidAttributeValueException, MBeanException;
//
//    public String getTokenPersistenceURL();
//
//    public void setTokenPersistenceURL(String val) throws InvalidAttributeValueException;
//
//    public String getTokenDBJDBCDriverClass();
//
//    public void setTokenDBJDBCDriverClass(String val) throws InvalidAttributeValueException;
    public void setInitialConfigurations(ComponentConfig props);

    public ComponentConfig getInstallationConfigurationProperties();
}
