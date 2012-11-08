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
 * @(#)BaseException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * The Base Exception, which extends a cascading Exception, that allows
 * printing .
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public class BaseException extends CascadingException {
    /**
     * Construct a new BaseException instance.
     *
     * @param message a message to be stored with the exception.
     */
    public BaseException(String message) {
        super(message, null);
    }

    /**
     * Creates a new BaseException instance.
     *
     * @param ex an Exception object.
     */
    public BaseException(Exception ex) {
        super(ex.getMessage(), ex);
    }

    /**
     * Construct a new BaseException that references a parent Exception.
     *
     * @param message a message to be stored with the exception.
     * @param t a Throwable object.
     */
    public BaseException(String message, Throwable t) {
        super(message, t);
    }

    /**
     * Convert the Exception with the nested exceptions to a string.
     *
     * @return a string representation of the exception tree.
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append(super.toString());

        if (getCause() != null) {
            s.append(": ");
            s.append(getCause().toString());
        }

        return s.toString();
    }

    /**
     * print the stack trace for this Exception and all nested Exceptions.
     */
    public void printStackTrace() {
        super.printStackTrace();

        if (getCause() != null) {
            getCause().printStackTrace();
        }
    }

    /**
     * print the stack trace for this Exception and all nested Exceptions to
     * the PrintStream.
     *
     * @param s a PrintStream object.
     */
    public void printStackTrace(PrintStream s) {
        super.printStackTrace(s);

        if (getCause() != null) {
            getCause().printStackTrace(s);
        }
    }

    /**
     * print the stack trace for this Exception and all nested Exceptions. to
     * the PrintWriter.
     *
     * @param s a PrintWriter object.
     */
    public void printStackTrace(PrintWriter s) {
        super.printStackTrace(s);

        if (getCause() != null) {
            getCause().printStackTrace(s);
        }
    }
}
