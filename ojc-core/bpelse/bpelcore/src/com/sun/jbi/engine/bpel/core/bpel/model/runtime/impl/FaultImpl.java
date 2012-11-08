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
package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;

/**
 * @author Sun Inc
 * Jul 25, 2007
 */
public class FaultImpl implements Fault {

    private QName mFaultName;
    private WSMessage mFaultData;
    private Exception mException; 
    
    /**
     * @param faultName
     * @param faultData
     */
    public FaultImpl(QName faultName, WSMessage faultData) {
        mFaultName = faultName;
        mFaultData = faultData;
    }
    
	/**
	 * @param faultName
	 * @param faultData
	 * @param exception The exception for which the fault was created so that it
	 * could be handled by a fault handler
	 */
	public FaultImpl(QName faultName, WSMessage faultData, Exception exception) {
		mFaultName = faultName;
		mFaultData = faultData;
		mException = exception;
	}

	/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault#getData()
     */
    public WSMessage getData() {
        return mFaultData;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault#getName()
     */
    public QName getName() {
        return mFaultName;
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault#getException()
	 */
	public Exception getException() {
		return mException;
	}
}
