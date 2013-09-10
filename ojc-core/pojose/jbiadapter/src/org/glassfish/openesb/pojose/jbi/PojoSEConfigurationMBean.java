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
 * @(#)PojoSEConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import java.util.Map;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotificationEmitter;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;


/**
 * Defines configurable properties of PojoSE.
 *
 * @author Girish Patil
 */
public interface PojoSEConfigurationMBean extends NotificationEmitter {
    public static final String CORE_THREAD_POOL_SIZE = "CoreThreadPoolSize"; //NOI18N
    public static final String MAX_THREAD_POOL_SIZE = "MaxThreadPoolSize"; //NOI18N
    public static final String THREAD_POOL_BLOCKING_QUEUE_SIZE = "ThreadPoolBlockingQueueSize"; //NOI18N
    
    public static final Integer DEFAULT_CORE_THREAD_POOL_SIZE = 15; 
    public static final Integer DEFAULT_MAX_THREAD_POOL_SIZE = 50; 
    public static final Integer DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE = 50; 

    public Integer getCoreThreadPoolSize();
    public void setCoreThreadPoolSize(Integer pz);

    public Integer getMaxThreadPoolSize();
    public void setMaxThreadPoolSize(Integer pz);
    
    /**
     * Returns ThreadPoolBlockingQueueSize. Note ThreadPoolBlockingQueueSize can
     * only be set during bootstrap process. Hence the setter is in bootstrap
     * config MBean.
     * 
     * @return Integer ThreadPoolBlockingQueueSize
     */    
    public Integer getThreadPoolBlockingQueueSize();
    public void setThreadPoolBlockingQueueSize(Integer cz);
    
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
}    
