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
 * @(#) HL7BCManagementMBean.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.management;

import java.util.List;
import javax.management.MBeanException;


/**
 * The Management MBean to suspend and resume
 * endpoints
 *
 */
public interface HL7BCManagementMBean {
	
    /** Suspend a endpoint
      *@param endpointName a endpoint identifier
      *@throws MBeanException on error
      */
    boolean suspend(String endpointName) throws MBeanException;
     
    /** Resume a endpoint
      * @param endpointname a endpoint identifier
      * @throws MBeanException on error
      */
    boolean resume (String endpointName) throws MBeanException;
    
    /** Returns true if a endpoint is active,
      * or false if it is suspended
      *
      * @param endpointName a endpoint identifier
      * @return true if endpoint is active, false if it is suspended
      */
    boolean isEndpointActive(String endpointName) throws MBeanException;
    
    /** Returns identifiers for all active manged endpoints.
      *
      * @return An array with zero or more endpoint identifiers.
      */
    String[] listActiveEndpoints();
    
    /** Returns identifiers for all suspended managed endpoints.
      *
      * @return An array with zero or more endpoint identifiers.
      */
    String[] listInactiveEndpoints();

    /** Returns all the managed endpoints.
      *
      * @return A list of endpoint identifiers.
      */
    List<String> listEndpoints();

    /** Returns the managed endpoints for the service unit.
      *
      * @return A list of endpoint identifiers for the service unit.
      */
    List<String> getEndpoints(String serviceUnit);
    
    /**
     * Returns list of service units
     * @return a string list of  service units
     */
    List<String> listServiceUnits();

    /**
     * Returns the sequence number for the service unit
     *
     * @param serviceUnit
     * @return sequence number
     */
    long getSequenceNumber(String serviceUnit) throws MBeanException ;

}