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
 * @(#)RuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.mbeans;

import java.util.Map;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

/**
 * MBean interface for run-time configuration
 *
 * @author Sun Microsystems
 */
public interface RuntimeConfigurationMBean {
	
	/**
	 * Get threads configured
	 * @return Integer
	 */
    public Integer getThreads();
    
    /**
     * Set threads to be configured
     * @param Integer
     * @throws InvalidAttributeValueException
     * @throws MBeanException
     */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
    public TabularData getEnvironmentVariables() throws OpenDataException;
    public void setEnvironmentVariables(TabularData val) throws InvalidAttributeValueException, OpenDataException, MBeanException;
    public Map retrieveEnvVariablesMap();
    public void updateEnvVariablesMap(Map val) throws MBeanException;    
}
