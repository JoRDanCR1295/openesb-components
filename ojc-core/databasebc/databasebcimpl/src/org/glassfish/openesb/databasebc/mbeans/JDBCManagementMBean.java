package org.glassfish.openesb.databasebc.mbeans;

import javax.management.MBeanException;

/**
 * The Management MBean to suspend and resume
 * endpoints
 *
 * @author Sun Microsystems
 */

public interface JDBCManagementMBean {

	/** Suspend a consuming endpoint
     * No-op for provisioning endpoints and operation will return false
     *
     *@param endpointName an unique consuming endpoint identifier
     *@return boolean indicating if suspend succeeds
     *@throws MBeanException on error
     */
   boolean suspend(String consumingEndpointName) throws MBeanException;
    
   /** Resume a consuming endpoint
     * No-op for provisioning endpoints and operation will return false.
     *
     * @param endpointname an unique consuming endpoint identifier
     * *@return boolean indicating if suspend succeeds
     * *@throws MBeanException on error
     */
   boolean resume (String consumingEndpointName) throws MBeanException;
   
   /** Returns true if an consuming endpoint is active,
     * or false if it is suspended
     * Always return true for provisioning endpoints.
     *
     * @param endpointName an unique consuming endpoint identifier
     * @return true if endpoint is active, false if it is suspended
     */
   boolean isEndpointActive(String consumingEndpointName) throws MBeanException;
   
   /** Returns an array of unique endpoint names for active
     * consuming endpoints.
     *
     * @return an array of unique endpoint names for active consuming endpoints
     */
   String[] listActiveEndpoints();
   
   /** Returns an array of unique endpoint names for inactive (suspended)
     * consuming endpoints.
     *
     * @return an array of unique endpoint names for inactive(suspended) consuming endpoints
     */
   String[] listInactiveEndpoints();
	
}
