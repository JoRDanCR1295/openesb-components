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
 * @(#)JXPathException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * Thrown in various situations by JXPath; may contain a nested exception.
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class JXPathException extends RuntimeException {

    /** @serial */
    private Throwable exception;

    /**
     * Create a new <code>JXPathException</code> with no
     * detail mesage.
     */

     public JXPathException() {
         super();
         this.exception = null;
     }

    /**
     * Create a new <code>JXPathException</code> with
     * the <code>String </code> specified as an error message.
     *
     * @param msg The error message for the exception.
     */
    public JXPathException(String msg) {
        super(msg);
        this.exception = null;
    }


    /**
     * Create a new <code>JXPathException</code> with a
     * given <code>Throwable</code> base cause of the error.
     *
     * @param e The exception to be encapsulated in a
     * JXPathException.
     */
    public JXPathException(Throwable e) {
        super(e.toString());
        this.exception = e;
    }

    /**
     * Create a new <code>JXPathException</code> with the
     * given <code>Exception</code> base cause and detail message.
     *
     * @param e The exception to be encapsulated in a
     * JXPathException
     * @param msg The detail message.
     */
    public JXPathException(String msg, Throwable e) {
        super(msg);
        this.exception = e;
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

        if (exception != null) {
            if (message == null) {
                if (exception.getMessage() != null) {
                    return exception.getMessage();
                }
                else {
                    return exception.getClass().getName();
                }
            }
            else {
                if (exception.getMessage() != null) {
                    return message + "; " + exception.getMessage();
                }
                else {
                    return message + "; " + exception.getClass().getName();
                }
            }
        }

        return message;
    }

    /**
     * Return the actual exception (if any) that caused this exception to
     * be raised.
     *
     * @return The encapsulated exception, or null if there is none.
     */
    public Throwable getException() {
        return exception;
    }
}
