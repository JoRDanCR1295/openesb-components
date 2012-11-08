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
 * @(#)MarshalException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * This exception type occurs for an error during the call to marshal() in the
 * OtdRoot interface.
 *
 * @author stc
 * @version 
 */
public class MarshalException
    extends RuntimeException {
    /**
     * The original cause, wrapped by this exception.
     * Null if this is the original exception itself.
     */
    private final Throwable mCause;

    /**
     * The location, or null if none.
     */
    private final OtdLocation mLocation;

    /**
     * Creates a new MarshalException object.
     *
     * @param loc  the location, or null if unknown
     * @param msg  the error message text, or null
     * @param cause  the originating exception if any, else null
     */
    public MarshalException (OtdLocation loc, String msg, Throwable cause) {
        super(msg);
        mLocation = loc;
        mCause = cause;
    }

    /**
     * Creates a new MarshalException object.
     *
     * @param loc  the location, or null if unknown
     * @param msg  the error message text, or null
     */
    public MarshalException (OtdLocation loc, String msg) {
        this(loc, msg, null);
    }

    /**
     * Creates a new MarshalException object.
     *
     * @param msg  the error message text, or null
     */
    public MarshalException (String msg) {
        this(null, msg, null);
    }

    /**
     * Creates a new MarshalException object.
     *
     * @param msg  the error message text, or null
     * @param cause  the originating exception if any, else null
     */
    public MarshalException (String msg, Throwable cause) {
        this(null, msg, cause);
    }

    /**
     * Creates a new MarshalException object.
     *
     * @param cause  the originating exception if any, else null
     */
    public MarshalException (Throwable cause) {
        this(null, null, cause);
    }

    /**
     * Prints the stack trace to the given stream.
     * Overrides the superclass to show the original cause, if any.
     *
     * @param stream  the output stream
     */
    @Override
    public void printStackTrace(PrintStream stream) {
        if (mLocation != null) {
            stream.print("at " + mLocation.text() + ": ");
        }
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
    public void printStackTrace(PrintWriter writer) {
        if (mLocation != null) {
            writer.print("at " + mLocation.text() + ": ");
        }
        super.printStackTrace(writer);
        if (mCause != null) {
            writer.println("-- caused by --");
            mCause.printStackTrace(writer);
        }
    }

    /**
     * Retrieves the location information.
     *
     * @return the location, or null if none
     */
    public OtdLocation getLocation () { return mLocation; }
}
