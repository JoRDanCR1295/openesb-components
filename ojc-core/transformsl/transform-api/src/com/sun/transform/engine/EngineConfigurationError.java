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
 * @(#)EngineConfigurationError.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine;

/**
 * 
 * @author Kevan Simpson
 */
public class EngineConfigurationError extends Error {
    /**
     * Wrapped {@link Exception} for the <code>EngineConfigurationError</code>.
     */
    private Throwable mThrowable;

    /**
     * 
     */
    public EngineConfigurationError() {
        mThrowable = null;
    }
    
    /**
     * @param message
     */
    public EngineConfigurationError(String message) {
        this(null, message);
    }

    /**
     * @param cause
     */
    public EngineConfigurationError(Throwable cause) {
        this(null, cause.toString());
    }

    /**
     * @param message
     * @param cause
     */
    public EngineConfigurationError(Throwable cause, String message) {
        super(message);
        mThrowable = cause;
    }

    /**
     * Return the message (if any) for this error . If there is no
     * message for the exception and there is an encapsulated
     * exception then the message of that exception will be returned.
     *
     * @return The error message.
     */
    public String getMessage() {
        String message = super.getMessage();

        return ((message == null) && (mThrowable != null)) 
                ? mThrowable.getMessage() : message;
    }

    /**
     * Return the actual {@link Throwable} (if any) that caused this exception to
     * be raised.
     *
     * @return The encapsulated <code>Throwable</code>, or <code>null</code> if there is none.
     */
    public Throwable getThrowable() {
        return mThrowable;
    }
}
