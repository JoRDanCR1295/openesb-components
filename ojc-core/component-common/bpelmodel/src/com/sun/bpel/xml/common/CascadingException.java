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
 * @(#)CascadingException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common;

/**
 * A Exception that allows recording of nested exceptions.
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public class CascadingException extends Exception
        implements CascadingThrowable {

    /** theThrowable */
    private final Throwable theThrowable;

    /**
     * Construct a new CascadingException instance.
     *
     * @param message The detail message for this exception.
     */
    public CascadingException(final String message) {
        this(message, null);
    }

    /**
     * Construct a new CascadingException instance.
     *
     * @param message The detail message for this exception.
     * @param throwable the root cause of the exception
     */
    public CascadingException(final String message, final Throwable throwable) {
        super(message);
        theThrowable = throwable;
    }

    /**
     * Retrieve root cause of the exception.
     *
     * @return the root cause
     */
    public final Throwable getCause() {
        return theThrowable;
    }
}
