/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)WorkflowException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;
import java.io.PrintStream;

import com.sun.jbi.engine.workflow.util.I18n;

/**
 * BPELException Class
 *
 * @author SeeBeyond Technology Corporation
 * @version 
 *
 * @since 5.0
 */
public class WorkflowException extends RuntimeException {

   /** Other Error */
    public static final String OTHER_ERROR = "OTHER_ERROR";

    /** DOCUMENT ME! */
    private String faultCode = null;

    /** DOCUMENT ME! */
    private Throwable targetThrowable = null;

    /** DOCUMENT ME! */
    private String location = null;
  
    /**
     * Create a new BPELException object.
     *
     * @param faultCode Fault Code
     * @param msg Description
     * @param t Target Exception
     */
    public WorkflowException (String faultCode, String msg, Throwable t) {
        super(msg, t);
        setFaultCode (faultCode);
        setTargetException (t);
    }
    
    public WorkflowException(String  msg) {
        super(msg);
        setFaultCode(OTHER_ERROR);
    }

    public WorkflowException(String  msg, Throwable th) {
        super(msg);
        setFaultCode(OTHER_ERROR);
        setTargetException(th);
    }
    
    public WorkflowException(Throwable t) {
        super(t);
        setFaultCode(OTHER_ERROR);
        setTargetException(t);
    }

    /**
     * Create a new BPELException object.
     *
     * @param faultCode Fault Code
     * @param msg Description
     */
    public WorkflowException (String faultCode, String msg) {
        this(faultCode, msg, null);
    }

    /**
     * Set the fault code on the exception
     *
     * @param faultCode Fault Code
     */
    public void setFaultCode (String faultCode) {
        this.faultCode = faultCode;
    }

    /**
     * Get the fault code on the exception
     *
     * @return Fault code
     */
    public String getFaultCode () {
        return faultCode;
    }

    /**
     * Set the target exception
     *
     * @param targetThrowable Target exception to set
     */
    public void setTargetException (Throwable targetThrowable) {
        this.targetThrowable = targetThrowable;
    }

    /**
     * Get the target exception
     *
     * @return Target exception
     */
    public Throwable getTargetException () {
        return targetThrowable;
    }

    /**
     * Set the location using an XPath expression. Used for error messages.
     *
     * @param location an XPath expression describing the location where the
     *        exception occurred.
     */
    public void setLocation (String location) {
        this.location = location;
    }

    /**
     * Get the location, if one was set. Should be an XPath expression which is
     * used for error messages.
     *
     * @return DOCUMENT ME!
     */
    public String getLocation () {
        return location;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getMessage () {
        if (location != null) {
            try {
                return "location: " + super.getMessage ();
            } catch (IllegalArgumentException e) {
                return "location : ***Exception occurred in executing the task";
            }
        }

        StringBuffer strBuf = new StringBuffer();
        strBuf.append ("WorkflowException");

        if (faultCode != null) {
            strBuf.append (": FaultCode = " + faultCode);
        }

        String thisMsg = super.getMessage ();
        String targetMsg = (targetThrowable != null)
            ? targetThrowable.getMessage () : null;

        if ((thisMsg != null)
                && ((targetMsg == null) || !thisMsg.equals (targetMsg))) {
            strBuf.append (": " + thisMsg);
        }

        if (targetMsg != null) {
            strBuf.append (": " + targetMsg);
        }

        return strBuf.toString ();
    }

    /** 
     * puts the stacktrace into the stream supplied.
     * @param stream the PrintStream
     */
    public void printStackTrace(PrintStream stream) {
        super.printStackTrace(stream);
        if (targetThrowable != null) {
            stream.println("caused by :");
            targetThrowable.printStackTrace(stream);
        }
    }

}
