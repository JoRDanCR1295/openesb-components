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

import javax.xml.namespace.QName;

import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;
import com.sun.jbi.dcombc.extensions.DCOMMessage;
import com.sun.jbi.dcombc.extensions.DCOMOperation;

/**
 * Class to hold message exchange context for DCOM message reply
 * 
 * @author Chandrakanth Belde
 */
public class ReplyContext {
	/**
	 *
	 */
    private DCOMMessage dcomMessage;

    private DCOMOperation dcomOp;

    private QName bindingOpQName;

    private DCOMInput dcomIn;

    private DCOMOutput dcomOut;

    public ReplyContext(DCOMMessage dcomMessage, QName bindingOpQName, DCOMOperation dcomOp, DCOMInput dcomIn,
            DCOMOutput dcomOut) {
        this.dcomMessage = dcomMessage;
        this.bindingOpQName = bindingOpQName;
        this.dcomOp = dcomOp;
        this.dcomIn = dcomIn;
        this.dcomOut = dcomOut;
    }

    public DCOMMessage getDCOMMessage() {
        return dcomMessage;
    }

    public QName getBindingOperationQName() {
        return bindingOpQName;
    }

    public DCOMOperation getDCOMOperation() {
        return dcomOp;
    }

    public DCOMInput getDCOMInput() {
        return dcomIn;
    }

    public DCOMOutput getDCOMOutput() {
        return dcomOut;
    }
}
