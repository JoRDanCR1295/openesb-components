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
package com.sun.jbi.dcombc;

import java.util.Collection;

import javax.jbi.JBIException;


/**
 * ServiceUnit represents a Service Unit managed by a ServiceUnitManager.  
 * This interface provides lifecycle management of a ServiceUnit and Endpoint
 * management of Endpoints associated with this ServiceUnit.
 * ServiceUnit has a set of Endpoints which can be in various states
 * (initialized, started, stopped, etc).  This interface provides read-only
 * access to the set of Endpoints contained by this ServiceUnit.  Clients to this
 * interface should not have the ability to remove or add Endpoints.  The only
 * interface that allows this ability is through the ServiceUnitInitializer.
 *
 * @author Chandrakanth Belde
 */
public interface ServiceUnit {

    /**
     * Retrieves the Id of this ServiceUnit.
     *
     * @return       the name of the Service as a QName
     */
    public String getServiceUnitId();

    /**
     * Initializes the ServiceUnit
     * @param        serviceunitid Id of this ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    void init(String serviceUnitId) throws JBIException;

    /**
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.
     * <p>
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if a any Endpoint fails
     * to activate
     */
    void start() throws JBIException;

    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit;
     * <p>
     * TODO: What should happen if not all Endpoints deactivate?
     * Unlike the activate() method, I'm NOT going to assume
     * this is transactional.  It seems silly to deactivate a number of
     * Endpoint, and if one fails, re-activate them.  I'll just throw
     * an error, and have the user decide how to deal with it.
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    void stop() throws JBIException;

    /**
     * Shuts down this ServiceUnit
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    void shutdown() throws JBIException;

    /**
     * Retrieves an Endpoint from this ServiceUnit 
     *
     * @param        serviceName the name of the service
     * @param        endpointName the name of the Endpoint to retrieve
     * @param        endpointType the endpoint type, either 
     *               Endpoint.EndpointType.INBOUND or Endpoint.EndpointType.OUTBOUND
     * @returns      The Endpoint instance or null if no endpoint is found
     *               that matches the given endpointName and serviceName.
     */
    public Endpoint getEndpoint(String serviceName, String endpointName, int endpointType);

    /**
     * Retrieves the Collection of Endpoints handled by this ServiceUnit.  This is
     * an unmodifiable Collection
     *
     * @return       the set of Endpoints
     */
    Collection getEndpoints();
}
