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

package com.sun.jbi.dcombc.packaging;

import java.util.List;

/**
 * EndpointConfiguration is an interface representing all BC endpoint
 * configurations.
 * 
 * @author Chandrakanth Belde
 */
public interface EndpointConfiguration {

    /**
     *  Adds an endpoint configuration to the collection of endpoint configurations
	 *
     *  @param ep The Endpoint instance to add.
     */
    public void addEndpoint(EndpointData ep);

    /**
     *  Adds an endoint configurationto the collection of endpoint configurations
	 *
     *  @param interfaceName The interface name
     *  @param service The service name
     *  @param endpoint The endpoint name
     *  @param direction The endpoint direction
     */
    public void addEndpoint(String interfaceName, String service, String endPoint, int direction);

    /**
     *  Returns a collection of all endpoint configurations
	 *
     *  @return The List of all endpoint configurations
     */
    public List endpoints();
}
