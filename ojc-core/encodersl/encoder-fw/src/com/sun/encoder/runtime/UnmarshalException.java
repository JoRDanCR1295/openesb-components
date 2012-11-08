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
 * @(#)UnmarshalException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * This exception type occurs for an error during the call to unmarshal() in
 * the OtdRoot interface.
 *
 * @author stc
 * @version 
 */
public class UnmarshalException extends RuntimeException {

    /**
     * The original cause, wrapped by this exception.
     * Null if this is the original exception itself.
     */
    private final Throwable mCause;

    /**
     * The location, or null if none.
     */
    private final OtdLocation mLocation;

    /** The location byte offset. */
    private long mOffset = -1;

    /** The location line number. */
    private long mLineNumber = -1;

    /** The location column number. */
    private long mColumnNumber = -1;

    /** The original message. */
    private String mMsg;

    /**
     * Creates a new UnmarshalException object.
     *
     * @param cause the original source of the exception
     */
    public UnmarshalException(Throwable cause) {
        this(cause.getMessage(), cause);
    }

    /**
     * Creates a new UnmarshalException object.
     *
     * @param msg the error text
     */
    public UnmarshalException(String msg) {
        this(msg, null);
    }

    /**
     * constructor that also keeps track of the throwable at fault.
     *
     * @param msg the error text
     * @param cause the original source of the exception
     */
    public UnmarshalException(String msg, Throwable cause) {
        super(msg);
        mLocation = null;
        mCause = cause;
        mMsg = msg;
    }

    /**
     * Constructs from origin information.
     *
     * @param offset the offset from the input stream in bytes, or -1
     * @param msg the error text
     * @deprecated use UnmarshalException(OtdLocation,String) instead
     */
    public UnmarshalException(long offset, String msg) {
        this(offset, msg, null);
    }

    /**
     * Constructs from origin information with a specified cause.
     *
     * @param offset the offset from the input stream in bytes, or -1
     * @param msg the error text
     * @param cause the original throwable that is wrapped by this exception.
     * @deprecated use UnmarshalException(OtdLocation,String) instead
     */
    public UnmarshalException(long offset, String msg, Throwable cause) {
        super(((offset < 0) ? "" : ("at byte " + offset + ": ")) + msg);
        mLocation = null;
        mCause = cause;
        mOffset = offset;
        mMsg = msg;
    }

    /**
     * Constructs from origin information.
     *
     * @param line line number
     * @param column column number
     * @param msg the error text
     * @deprecated use UnmarshalException(OtdLocation,String) instead
     */
    public UnmarshalException(long line, long column, String msg) {
        super(msg);
        mLocation = null;
        mCause = null;
        mLineNumber = line;
        mColumnNumber = column;
    }

    /**
     * Constructs from origin information.
     *
     * @param loc  location information, or null if none
     * @param msg the error text
     */
    public UnmarshalException (OtdLocation loc, String msg) {
        super(msg);
        mLocation = loc;
        mCause = null;
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
     * Gets the original message.
     *
     * @return the original message
     */
    public String getOriginalMessage() {
        return mMsg;
    }

    /**
     * Retrieves the location information.
     *
     * @return the location, or null if none
     */
    public OtdLocation getLocation () { return mLocation; }

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
     * Determines whether line and column numbers are available.
     *
     * @return flag
     * @deprecated use getLocation() instead
     */
    public boolean hasLineAndColumn() {
        return mLineNumber > -1;
    }

    /**
     * Gets the location byte offset.
     *
     * @return the byte offset
     * @deprecated use getLocation() instead
     */
    public long getOffset() {
        return mOffset;
    }

    /**
     * Gets the location line number.
     *
     * @return the line number
     * @deprecated use getLocation() instead
     */
    public long getLineNumber() {
        return mLineNumber;
    }

    /**
     * Gets the location line number.
     *
     * @return the line number
     * @deprecated use getLocation() instead
     */
    public long getColumnNumber() {
        return mColumnNumber;
    }
}
