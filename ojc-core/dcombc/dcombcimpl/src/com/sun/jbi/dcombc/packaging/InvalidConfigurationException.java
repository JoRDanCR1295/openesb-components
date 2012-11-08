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
 * Configuration exception class
 *
 * @author Chandrakanth Belde
 */
public class InvalidConfigurationException extends Exception {
	/**
	 *
	 */
    private static final long serialVersionUID = 1L;

    /** 
     * Creates a new instance of InvalidConfigurationException 
     */
    public InvalidConfigurationException() {
    }

    /** 
     * Creates a new instance of InvalidConfigurationException 
     *
     * @param str Exception message String.
     */
    public InvalidConfigurationException(String str) {
        super(str);
    }

    /** 
     * Creates a new instance of InvalidConfigurationException 
     *
     * @param str Exception message String.
     * @param cause The root cause of the exception.
     */
    public InvalidConfigurationException(String str, Throwable cause) {
        super(str, cause);
    }
}