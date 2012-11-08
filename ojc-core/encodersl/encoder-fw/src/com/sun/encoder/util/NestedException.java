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
 * @(#)NestedException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.util;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * This exception is meant to wrap non-RuntimeException errors into a plain
 * RuntimeException, so that they can be re-thrown from a method whose
 * signature does not allow it to throw non-RuntimeExceptions directly
 * (normally because it implements an interface that does not declare them)
 * wtihout losing the original stack trace info.
 *
 * @author Michael Libourel
 * @version 
 */
public class NestedException
    extends RuntimeException {

    /**
     * The original cause, wrapped by this exception.
     * Null if this is the original exception itself.
     */
    private final Throwable mCause;

    /** The original message. */
    private String mMsg;

    /**
     * Creates a new NestedException object.
     *
     * @param cause the original source of the exception
     */
    public NestedException (Throwable cause) {
        this(cause.getMessage(), cause);
    }

    /**
     * Creates a new NestedException object.
     *
     * @param msg the error text
     */
    public NestedException (String msg) {
        this(msg, null);
    }

    /**
     * constructor that also keeps track of the throwable at fault.
     *
     * @param msg the error text
     * @param cause the original source of the exception
     */
    public NestedException (String msg, Throwable cause) {
        super(msg);
        mCause = cause;
        mMsg = msg;
    }

    /**
     * Prints the stack trace to the given stream.
     * Overrides the superclass to show the original cause, if any.
     *
     * @param stream  the output stream
     */
    @Override
    public void printStackTrace (PrintStream stream) {
        super.printStackTrace(stream);
        if (mCause != null) {
            stream.println("-- caused by --");
            mCause.printStackTrace(stream);
        }
    }

    /**
     * Prints the stack trace to the given writer.
     * Overrides the superclass to show the original cause, if any.
     *
     * @param writer  the output stream
     */
    @Override
    public void printStackTrace (PrintWriter writer) {
        super.printStackTrace(writer);
        if (mCause != null) {
            writer.println("-- caused by --");
            mCause.printStackTrace(writer);
        }
    }
}
