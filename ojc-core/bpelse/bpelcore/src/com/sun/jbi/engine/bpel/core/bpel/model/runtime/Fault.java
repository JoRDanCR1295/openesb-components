/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import javax.xml.namespace.QName;

/**
 * @author Sun Inc
 * Jul 25, 2007
 */
public interface Fault {

    /**
     * @return fault name as QName. Can return null.
     */
    public QName getName();
    
    /**
     * @return fault data as WSMessage. Can return null.
     */
    public WSMessage getData();
    
    /**
     * @return The exception for which this fault has been created
     */
    public Exception getException();
}
