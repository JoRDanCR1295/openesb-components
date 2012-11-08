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

import com.sun.jbi.common.qos.config.ComponentConfig;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
/**
 * MashupSEInstallerConfigurationMBean.java
 *
 * Created on August 24, 2006, 1:44 PM
 *
 * @author Nilesh Apte
 * @author Jawed
 */
public interface MashupSEInstallerConfigurationMBean {

    // Attribute names
    public static final String MAX_THREAD_COUNT = "MaxThreadCount";
    public static final String AXION_PAGE_SIZE = "PageSize";
    public static final String AXIONDB_INSTANCE_NAME = "AxiondbInstanceName";
    public static final String AXIONDB_WORKING_DIR = "AxiondbWorkingDirectory";
    public static final String AXIONDB_DATA_DIR = "AxiondbDataDirectory";
    public static final String EXTERNAL_CON_PROVIDER = "ExternalConnectionProvider";
    /**
     * @return the maxThreadCount
     */
    public String getMaxThreadCount();

    /**
     * @param maxThreadCount the maxThreadCount to set
     */
    public void setMaxThreadCount(String maxThreadCount);

     /**
     * @return the pageSize
     */
    public String getPageSize();

    /**
     * @param pageSize the pageSize to set
     */
    public void setPageSize(String pageSize);

     /**
     * @return the axiondbWorkingDir
     */
    public String getAxiondbWorkingDirectory() throws InvalidAttributeValueException, MBeanException;

    /**
     * @param axiondbWorkingDir
     *            the axiondbWorkingDir to set
     */
    public void setAxiondbWorkingDirectory(String axiondbWorkingDir) throws InvalidAttributeValueException, MBeanException;

    /**
     * @return the axiondbDataDir
     */
    public String getAxiondbDataDirectory() throws InvalidAttributeValueException, MBeanException;

    /**
     * @param axiondbDataDir
     *            the axiondbDataDir to set
     */
    public void setAxiondbDataDirectory(String axiondbDataDir) throws InvalidAttributeValueException, MBeanException;
    
    public String  getExternalConnectionProvider() throws InvalidAttributeValueException, MBeanException;
    
    public void setExternalConnectionProvider(String providerClassName) throws InvalidAttributeValueException, MBeanException;
        
    public void setInitialConfigurations(ComponentConfig props);

    public ComponentConfig getInstallationConfigurationProperties();
}
