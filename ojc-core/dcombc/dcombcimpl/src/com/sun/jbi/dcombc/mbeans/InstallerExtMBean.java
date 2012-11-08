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

/**
 * MBean interface
 * 
 * @author Chandrakanth Belde
 */

public interface InstallerExtMBean {
	/**
	 *
	 */
    public String getThreads();

    public void setThreads(String val);
}
