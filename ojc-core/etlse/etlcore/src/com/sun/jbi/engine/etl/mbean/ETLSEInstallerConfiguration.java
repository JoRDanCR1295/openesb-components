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
 * @(#)ETLSEInstallerConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.etl.mbean;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * ETLSEInstallerConfiguration.java
 *
 * Created on August 24, 2006, 1:44 PM
 *
 * @author Sujit Biswas
 */
public class ETLSEInstallerConfiguration extends StandardMBean implements ETLSEInstallerConfigurationMBean {

    private Integer maxThreadCount = Integer.valueOf(10);
    private String axiondbDataRoot;
    private String axiondbWorkingDirectory;

    private String externalConnectionProvider = "com.sun.etl.engine.utils.RuntimeConnectionProvider";
    private ComponentConfig mConfigProps;

    public ETLSEInstallerConfiguration() throws NotCompliantMBeanException {
        super(ETLSEInstallerConfigurationMBean.class);
    }

    /**
     * @return the maxThreadCount
     */
    public String getMaxThreadCount() {
        return maxThreadCount.toString();
    }

    /**
     * @param maxThreadCount the maxThreadCount to set
     */
    public void setMaxThreadCount(String maxThreadCount) {
        this.maxThreadCount = Integer.parseInt(maxThreadCount);
    }

    public String getAxiondbWorkingDirectory() throws InvalidAttributeValueException, MBeanException {
        return axiondbWorkingDirectory;
    }

    public void setAxiondbWorkingDirectory(String axiondbWorkingDir) throws InvalidAttributeValueException, MBeanException {
        this.axiondbWorkingDirectory = axiondbWorkingDir;
    }

    /**
     * @return the axiondbDataRoot
     */
    public String getAxiondbDataDirectory() throws InvalidAttributeValueException, MBeanException {
        return axiondbDataRoot;
    }

    /**
     * @param axiondbDataRoot the axiondbDataRoot to set
     */
    public void setAxiondbDataDirectory(String axiondbDataDir) throws InvalidAttributeValueException, MBeanException {
        this.axiondbDataRoot = axiondbDataDir;
    }

    public String getExternalConnectionProvider() throws InvalidAttributeValueException, MBeanException {
        return externalConnectionProvider;
    }
    
    public void setExternalConnectionProvider(String providerClassName) throws InvalidAttributeValueException, MBeanException {
        this.externalConnectionProvider = providerClassName;
    }



    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        maxThreadCount = new Integer(10);
        axiondbWorkingDirectory = props.getProperty(AXIONDB_WORKING_DIR).getValue();
        axiondbDataRoot = props.getProperty(AXIONDB_DATA_DIR).getValue();
        externalConnectionProvider = props.getProperty(EXTERNAL_CON_PROVIDER).getValue();
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(MAX_THREAD_COUNT).setValue(maxThreadCount.toString());
        mConfigProps.getProperty(AXIONDB_WORKING_DIR).setValue(axiondbWorkingDirectory);
        mConfigProps.getProperty(AXIONDB_DATA_DIR).setValue(axiondbDataRoot);
        mConfigProps.getProperty(EXTERNAL_CON_PROVIDER).setValue(externalConnectionProvider);
        return mConfigProps;
    }
}
