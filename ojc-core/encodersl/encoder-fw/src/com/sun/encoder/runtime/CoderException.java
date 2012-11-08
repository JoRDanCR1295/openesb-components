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
 * @(#)CoderException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Exception class to represent problems in converting byte sequences
 * to string and vice versa.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * XXX: Add OtdLocation info for offset and nesting...
 *
 * @author Michael Libourel
 * @version 
 */
public class CoderException
    extends RuntimeException {

    /**
     * The original cause, wrapped by this exception.
     * Null if this is the original exception itself.
     */
    private final Throwable mCause;

    /**
     * Offset information, or -1 if unknown.
     */
    private final long mOffset;

    /**
     * The original error message.
     */
    private String mMessage;

    /**
     * Creates a new CoderException object.
     *
     * @param message the error text
     */
    public CoderException(String message) {
        this(message, null);
    }

    /**
     * constructor that also keeps track of the throwable at fault.
     *
     * @param message the error text
     * @param cause the original source of the exception
     */
    public CoderException(String message, Throwable cause) {
        this(-1, message, cause);
    }

    /**
     * Constructs from origin information.
     *
     * @param offset the offset from the input stream in bytes, or -1
     * @param message the error text
     * @deprecated use CoderException(OtdLocation,String) instead
     */
    public CoderException(long offset, String message) {
        this(offset, message, null);
    }

    /**
     * Constructs from origin information with a specified cause.
     *
     * @param offset the offset from the input stream in bytes, or -1
     * @param message the error text
     * @param cause the original throwable that is wrapped by this exception.
     * @deprecated use CoderException(OtdLocation,String) instead
     */
    public CoderException(long offset, String message, Throwable cause) {
        super(((offset < 0) ? "" : ("at byte " + offset + ": ")) + message);
        mCause = cause;
        mOffset = offset;
        mMessage = message;
    }

    /**
     * Prints the stack trace to the given stream.
     * Overrides the superclass to show the original cause, if any.
     *
     * @param stream  the output stream
     */
    @Override
    public void printStackTrace(PrintStream stream) {
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
        super.printStackTrace(writer);
        if (mCause != null) {
            writer.println("-- caused by --");
            mCause.printStackTrace(writer);
        }
    }

    /**
     * Gets the original message.
     *
     * @return the original message
     */
    public String getOriginalMessage() {
        return mMessage;
    }

    /**
     * Determines whether byte offset is available.
     *
     * @return flag
     * @deprecated use getLocation() instead
     */
    public boolean hasOffset() {
        return mOffset > -1;
    }

    /**
     * Retrieves the offset.
     *
     * @return non-negative offset if known, else -1
     */
    public long getOffset () {
        return mOffset;
    }
}
