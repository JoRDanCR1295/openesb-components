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

/**
 * Interface representing "raw" data endpoint information independent of the
 * format of the endpoint configuration file
 *  
 * @author Chandrakanth Belde
 */
public interface EndpointData {

    /**
     * Gets the interface name
     * @return The interface name.
     */
    public String getInterface();

    /**
     * Gets the service name
     * @return The service name.
     */
    public String getService();

    /**
     * Gets the service name
     * @return The endpoint name.
     */
    public String getEndpoint();

    /**
     * Gets the direction
     * @return The endpoint direction.
     */
    public int getDirection();

}
