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
package com.sun.jbi.engine.mashup.mbean;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * MashupSEInstallerConfiguration.java
 *
 * Created on August 24, 2006, 1:44 PM
 *
 * @author Nilesh Apte
 * @authro Jawed
 */
public class MashupSEInstallerConfiguration extends StandardMBean implements MashupSEInstallerConfigurationMBean {

    private Integer maxThreadCount = Integer.valueOf(5);
    private Integer pageSize;
    private String axiondbInstance="DB_INSTANCE";
    private String axiondbWorkingDirectory;
    private String axiondbDataDirectory;
    private String externalConnectionProvider = "com.sun.mashup.engine.utils.RuntimeConnectionProvider";
    
    private ComponentConfig mConfigProps;

    public MashupSEInstallerConfiguration() throws NotCompliantMBeanException {
        super(MashupSEInstallerConfigurationMBean.class);
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

    /**
     * @return the pageSize
     */
    public String getPageSize() {
        return pageSize.toString();
    }

    /**
     * @param pageSize the pageSize to set
     */
    public void setPageSize(String pageSize) {
        this.pageSize = Integer.parseInt(pageSize);
    }

    public String getAxiondbWorkingDirectory() throws InvalidAttributeValueException, MBeanException {
        return axiondbWorkingDirectory;
    }

    public void setAxiondbWorkingDirectory(String axiondbWorkingDir) throws InvalidAttributeValueException, MBeanException {
        this.axiondbWorkingDirectory = axiondbWorkingDir;
    }

    public String getAxiondbDataDirectory() throws InvalidAttributeValueException, MBeanException {
        return axiondbDataDirectory;
    }

    public void setAxiondbDataDirectory(String axiondbDataDir) throws InvalidAttributeValueException, MBeanException {
        this.axiondbDataDirectory = axiondbDataDir;
    }
    
    public String getExternalConnectionProvider() throws InvalidAttributeValueException, MBeanException {
        return externalConnectionProvider;
    }
    
    public void setExternalConnectionProvider(String providerClassName) throws InvalidAttributeValueException, MBeanException {
        this.externalConnectionProvider = providerClassName;
    }
    
    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        
        maxThreadCount = Integer.valueOf(props.getProperty(MAX_THREAD_COUNT).getValue());
        pageSize = Integer.valueOf(props.getProperty(AXION_PAGE_SIZE).getValue());
        axiondbInstance = props.getProperty(AXIONDB_INSTANCE_NAME).getValue();
        axiondbWorkingDirectory = props.getProperty(AXIONDB_WORKING_DIR).getValue();
        axiondbDataDirectory = props.getProperty(AXIONDB_DATA_DIR).getValue();
        externalConnectionProvider = props.getProperty(EXTERNAL_CON_PROVIDER).getValue();
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(MAX_THREAD_COUNT).setValue(maxThreadCount.toString());
        mConfigProps.getProperty(AXION_PAGE_SIZE).setValue(pageSize.toString());
        mConfigProps.getProperty(AXIONDB_INSTANCE_NAME).setValue(axiondbInstance);
        mConfigProps.getProperty(AXIONDB_WORKING_DIR).setValue(axiondbWorkingDirectory);
        mConfigProps.getProperty(AXIONDB_DATA_DIR).setValue(axiondbDataDirectory); 
        mConfigProps.getProperty(EXTERNAL_CON_PROVIDER).setValue(externalConnectionProvider);
        return mConfigProps;
    }
}
