/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.mbeans;

import java.util.Map;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

/**
 * MBean interface for run-time configuration
 *
 * @author Chandrakanth Belde
 */
public interface RuntimeConfigurationMBean {
	
	/**
	 * Get threads configured
	 *
	 * @return no of threads present
	 */
    public Integer getThreads();
    
    /**
     * Set threads to be configured
     *
	 * @param no of threads to assign
     * @throws InvalidAttributeValueException
     * @throws MBeanException
     */
    public void setThreads(Integer val) 
		throws InvalidAttributeValueException, MBeanException;

	/**
	 * Get Environment variables in tabulardata
	 *
	 * @return tabulardata
	 * @throws OpenDataException
	 */
	public TabularData getEnvironmentVariables() throws OpenDataException;
    
	/**
	 * Set Environment variables using tabular data
	 *
	 * @param tabulardata value
	 */
	public void setEnvironmentVariables(TabularData val) 
		throws InvalidAttributeValueException, OpenDataException, MBeanException;
    
	/**
	 * Returns environment variable map
	 */
	public Map retrieveEnvVariablesMap();
    
	/**
	 * Update Environment variable map
	 * 
	 * @param Map with updated values
	 */
	public void updateEnvVariablesMap(Map val) throws MBeanException;    
}
