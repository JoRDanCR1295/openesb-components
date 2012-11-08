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
 * @(#)BPELSERuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel;

import java.util.Map;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;

public interface BPELSERuntimeConfigurationMBean {

    String CONFIG_APPLICATON_VARIABLES = "ApplicationVariables";

    public Boolean getDebugEnabled();

    public Integer getDebugPort();

    public Boolean getPersistenceEnabled();

    public String getDatabaseNonXAJNDIName();

    public String getDatabaseXAJNDIName();

    public Integer getThreadCount();

    public Integer getEngineExpiryInterval();

    public Integer getWaitingRequestLifeSpan();

    public Boolean getMonitoringEnabled();

    public Boolean getMonitoringVariableEnabled();

    public Boolean getKPIEnabled();

    public String getTransformEngine();

    public void setDebugEnabled(Boolean newValue) throws MBeanException;

    public void setDebugPort(Integer newValue) throws MBeanException, InvalidAttributeValueException;

    public void setPersistenceEnabled(Boolean newValue) throws MBeanException;

    public void setDatabaseNonXAJNDIName(String newValue) throws MBeanException;

    public void setDatabaseXAJNDIName(String newValue)
            throws InvalidAttributeValueException, MBeanException;

    public void setThreadCount(Integer newValue) throws MBeanException;

    public void setEngineExpiryInterval(Integer newValue) throws MBeanException;

    public void setWaitingRequestLifeSpan(Integer waitingRequestLifeSpan) throws MBeanException;

    public void setMonitoringEnabled(Boolean newValue) throws MBeanException;

    public void setMonitoringVariableEnabled(Boolean newValue) throws MBeanException;

    public void setKPIEnabled(Boolean newValue) throws MBeanException;

    public void setTransformEngine(String engine) throws MBeanException, InvalidAttributeValueException;

    /**
     * This operation adds a new application variable. If a variable with the same name
     * already exists, the operation fails.
     *
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variable to the
     *         component.
     */
    public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException;

    /**
     * This operation sets an application variable. If a variable does not exist with
     * the same name, its an error.
     *
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException;

    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
     public void deleteApplicationVariable(String name) throws MBeanException;

     /**
     * Get the Application Variable set for a component.
     *
     * @return  a TabularData which has all the applicationvariables set on the component.
     */
    public TabularData getApplicationVariables();

    /** Retrieves the application variables map. The map key is the application
     * variable name, the value is a String[] containing detailed information
     * about the application variable. This method is used to communicate
     * application variable data with in the component and is not intended
     * for MBean clients
     *
     * @return a Map containing application variable information
     */
    public Map retrieveApplicationVariablesMap();

    /** Updates the application variable map.
     * This method is used to communicate application configuration data within the component, and
     * not intended for MBean clients
     *
     * @param a Map containing application variable information
     */
    public void updateApplicationVariablesMap(Map val) throws MBeanException;

    /**
     * sets validation enabled flag value
     * @param validationEnabled - validation enabled flag
     */
    public void setValidationEnabled(Boolean validationEnabled) throws MBeanException;

    /**
     * returns validation enabled flag
     * @return ValidationEnabled - validation enabled flag
     */
    public Boolean getValidationEnabled();
}
