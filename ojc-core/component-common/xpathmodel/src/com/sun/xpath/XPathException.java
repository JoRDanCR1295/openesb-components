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
 * @(#)XPathException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath;


/**
 * XPath exception class.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XPathException
    extends Exception {


    /**
     * Creates an exception wrapping an existing exception.
     *
     * @param ex the existing exception
     */
    public XPathException(Exception ex) {
        super(ex);
    }


    /**
     * Creates an exception with the given message.
     *
     * @param message the message
     */
    public XPathException(String message) {
        super(message);
    }


    /**
     * Creates an exception with the given message and wrapping an existing
     * exception.
     *
     * @param message the message
     * @param ex the existing exception
     */
    public XPathException(String message, Exception ex) {
        super(message, ex);
    }
}
