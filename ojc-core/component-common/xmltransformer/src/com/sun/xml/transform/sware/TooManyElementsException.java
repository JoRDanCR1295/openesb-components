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
 * @(#)TooManyElementsException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware;

import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerException;

/**
 * This exception will be thrown if during sorting more elements than schema
 * allowed are encountered.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class TooManyElementsException extends TransformerException {

    private static final long serialVersionUID = 4205686019044667252L;

    /**
     * Constructs from an error message.
     * 
     * @param message the error message
     */
    public TooManyElementsException(String message) {
        super(message);
    }

    /**
     * Constructs from a throwable.
     * @param e the throwable
     */
    public TooManyElementsException(Throwable e) {
        super(e);
    }

    /**
     * Constructs from a message and a throwable.
     * 
     * @param message the error message
     * @param e the throwable
     */
    public TooManyElementsException(String message, Throwable e) {
        super(message, e);
    }

    /**
     * Constructs from a message and a locator.
     * 
     * @param message the error message
     * @param locator the locator indicating where transformation error occurs
     */
    public TooManyElementsException(String message, SourceLocator locator) {
        super(message, locator);
    }

    /**
     * Constructs from a message, a locator and a throwable.
     * 
     * @param message the error message
     * @param locator the locator indicating where transformation error occurs
     * @param e the throwable
     */
    public TooManyElementsException(String message, SourceLocator locator,
            Throwable e) {
        super(message, locator, e);
    }
}
