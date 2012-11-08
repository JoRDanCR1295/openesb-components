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

package com.sun.jbi.dcombc.extensions;

import java.io.Serializable;

/**
 * Represents dcom:output extensibility element of dcom:operation element
 * 
 * @author Chandrakanth Belde
 */
public class DCOMOutput implements Serializable {
	/**
	 *
	 */
    private static final long serialVersionUID = 1L;

    private DCOMMessage dcomMessage = null;
	
	/**
	 * Get dcom message from dcom:input extensiblity element
	 *
	 * @return DCOMMessage representing the message of dcom:input element
	 */
    public DCOMMessage getDCOMMessage() {
        return dcomMessage;
    }
	
	/**
	 * Set dcom message for dcom:input extensibility element
	 *
	 * @param DCOMMessage representing the message part of dcom:input element
	 */
    public void setDCOMMessage(DCOMMessage dcomMessage) {
        this.dcomMessage = dcomMessage;
    }
}
