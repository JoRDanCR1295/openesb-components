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

/**
 * DCOM BC application level exception class
 * 
 * @author Chandrakanth Belde
 */
public class DCOMException extends Exception {
	/**
	 *
	 */
    private static final long serialVersionUID = -3387516993124229948L;

    private static String appString = "\n ***************** DCOM BC Exception *********************** \n";

    /**
     * Creates a new instance of DCOMException
     * 
     * @param msg The exception message.
     */
    public DCOMException(String msg) {
        super(appString + msg);
    }

    /**
     * Creates a new instance of DCOMException
     */
    public DCOMException() {
    }

    /**
     * Creates a new instance of DCOMException
     * 
     * @param msg The exception message.
     * @param cause The cause of the exception.
     */
    public DCOMException(String msg, Throwable cause) {
        super(appString + msg, cause);
    }

    /**
     * Creates a new instance of DCOMException
     * 
     * @param cause The cause of the exception.
     */
    public DCOMException(Throwable cause) {
        super(cause);
    }

}
