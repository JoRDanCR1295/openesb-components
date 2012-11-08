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
 * @(#)BPELRuntimeException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.exception;

import java.io.PrintStream;

import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * BPELRuntimeException Class
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public class BPELRuntimeException extends RuntimeException {
    
    /** Invalid BPEL error. */
    public static final String INVALID_BPEL = "INVALID_BPEL";

    /** Parser Error */
    public static final String PARSER_ERROR = "PARSER_ERROR";

    /** Other Error */
    public static final String OTHER_ERROR = "OTHER_ERROR";

    /** Configuration Error */
    public static final String CONFIGURATION_ERROR = "CONFIGURATION_ERROR";

    /** persistence error */
    public static final String PERSISTENCE_ERROR = "Persistence Error";
    
    /** Correlation Error  */
    public static final String CORRELATION_ERROR = "Correlation Error";

    /** DOCUMENT ME! */
    private String mFaultCode;

    /** DOCUMENT ME! */
    private Throwable mTargetThrowable = null;

    /** DOCUMENT ME! */
    private String location = null;

    /**
     * Create a new BPELRuntimeException object.
     *
     * @param faultCode Fault Code
     * @param msg Description
     * @param t Target Exception
     */
    public BPELRuntimeException(String faultCode, String msg, Throwable t) {
        super(msg);
        setFaultCode(faultCode);
        setTargetException(t);
    }

    /**
     * constructor
     *
     * @param t Throwable
     */
    public BPELRuntimeException(Throwable t) {
        super(t.getMessage());
        setFaultCode(OTHER_ERROR);
        setTargetException(t);
    }

    /**
     * Create a new BPELRuntimeException object.
     *
     * @param faultCode Fault Code
     * @param msg Description
     */
    public BPELRuntimeException(String faultCode, String msg) {
        this(faultCode, msg, null);
    }

    /**
     * Set the fault code on the exception
     *
     * @param faultCode Fault Code
     */
    public void setFaultCode(String faultCode) {
        this.mFaultCode = faultCode;
    }

    /**
     * Get the fault code on the exception
     *
     * @return Fault code
     */
    public String getFaultCode() {
        return mFaultCode;
    }

    /**
     * Set the target exception
     *
     * @param targetThrowable Target exception to set
     */
    public void setTargetException(Throwable targetThrowable) {
        this.mTargetThrowable = targetThrowable;
    }

    /**
     * Get the target exception
     *
     * @return Target exception
     */
    public Throwable getTargetException() {
        return mTargetThrowable;
    }

    /**
     * Set the location using an XPath expression. Used for error messages.
     *
     * @param location an XPath expression describing the location where the exception occurred.
     */
    public void setLocation(String location) {
        this.location = location;
    }

    /**
     * Get the location, if one was set. Should be an XPath expression which is used for error
     * messages.
     *
     * @return DOCUMENT ME!
     */
    public String getLocation() {
        return location;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getMessage() {
        if (location != null) {
            try {
                return location + ": " + super.getMessage();
            } catch (IllegalArgumentException e) {
                return location + ": " + I18n.loc("BPCOR-6043: *** IllegalArgumentException ***");
            }
        }

        StringBuffer strBuf = new StringBuffer();
        strBuf.append(I18n.loc("BPCOR-3018: BPELRuntimeException"));

        if (mFaultCode != null) {
            strBuf.append(I18n.loc("BPCOR-3019: : faultCode={0}", mFaultCode));
        }

        String thisMsg = super.getMessage();
        String targetMsg = (mTargetThrowable != null)
            ? mTargetThrowable.getMessage() : null;

        if ((thisMsg != null) &&
                ((targetMsg == null) || !thisMsg.equals(targetMsg))) {
            strBuf.append(": " + thisMsg);
        }

        if (targetMsg != null) {
            strBuf.append(": " + targetMsg);
        }

        return strBuf.toString();
    }

    /**
     * puts the stacktrace into the stream supplied.
     *
     * @param stream the PrintStream
     */
    public void printStackTrace(PrintStream stream) {
        super.printStackTrace(stream);

        if (mTargetThrowable != null) {
            stream.println(I18n.loc("BPCOR-3020: caused by:"));
            mTargetThrowable.printStackTrace(stream);
        }
    }
}
