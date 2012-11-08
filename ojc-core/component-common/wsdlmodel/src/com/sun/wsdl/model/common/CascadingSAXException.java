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
 * @(#)CascadingSAXException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common;

import java.io.PrintStream;
import java.io.PrintWriter;
import org.xml.sax.SAXException;

/**
 * Wrapper around SAXException but one that does the correct
 * <code>printStackTrace</code>
 *
 * @author Sun Microsystems
 * @version 
 */
public class CascadingSAXException extends SAXException {
    
    /**
     * Create a new CascadingSAXException.
     *
     * @param message The error or warning message.
     * @see org.xml.sax.Parser#setLocale
     */
    public CascadingSAXException(String message) {
        super(message);
    }
    
    
    /**
     * Create a new CascadingSAXException wrapping an existing exception.
     *
     * <p>The existing exception will be embedded in the new
     * one, and its message will become the default message for
     * the SAXException.</p>
     *
     * @param e The exception to be wrapped in a SAXException.
     */
    public CascadingSAXException(Exception e) {
        super(e);
    }
    
    
    /**
     * Create a new CascadingSAXException from an existing exception.
     *
     * <p>The existing exception will be embedded in the new
     * one, but the new exception will have its own message.</p>
     *
     * @param message The detail message.
     * @param e The exception to be wrapped in a SAXException.
     * @see org.xml.sax.Parser#setLocale
     */
    public CascadingSAXException(String message, Exception e) {
        super(message, e);
    }

    /**
     * print the stack trace for this Exception and all nested Exceptions.
     */
    public void printStackTrace() {
        super.printStackTrace();

        if (getException() != null) {
            getException().printStackTrace();
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

        if (getException() != null) {
            getException().printStackTrace(s);
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

        if (getException() != null) {
            getException().printStackTrace(s);
        }
    }
}
