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
 * Class which represents the "raw" endpoint 
 * information independent of the format of the endpoint configuration file
 * 
 * @author Chandrakanth Belde
 */
public class EndpointDataImpl implements EndpointData {
	/**
	 *
	 */
    private String mInterface;

    private String mService;

    private String mEndPoint;

    private int mDirection;
	
	/**
	 * @Constructor
	 */
    protected EndpointDataImpl(String interfaceName, String service, String endPoint, int direction) {
        mInterface = interfaceName;
        mService = service;
        mEndPoint = endPoint;
        mDirection = direction;
    }
	
	/**
	 * @return interface
	 */
    public String getInterface() {
        return mInterface;
    }
	
	/**
	 * @return service
	 */
    public String getService() {
        return mService;
    }
	
	/**
	 * @return endpoint
	 */
    public String getEndpoint() {
        return mEndPoint;
    }

	/**
	 * @return direction
	 */ 
    public int getDirection() {
        return mDirection;
    }

}
