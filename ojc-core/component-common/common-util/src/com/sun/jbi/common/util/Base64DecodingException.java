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
 * @(#)Base64DecodingException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util;

import java.io.IOException;


/**
 * Exception that is thrown when an unexpected character is encountered during Base64 decoding.
 * One could catch this exception and use the unexpected character for some other purpose such as
 * including it with data that comes at the end of a Base64 encoded section of an email message.
 *
 * @author Sun Microsystems
 * @version $Version$
 */
public class Base64DecodingException extends IOException {
    private char c;

    /**
     * Construct an new exception.
     *
     * @param message message later to be returned by a getMessage() call.
     * @param c character that caused this error.
     */
    public Base64DecodingException(String message, char c) {
        super(message);
        this.c = c;
    }

    /**
     * Get the character that caused this error.
     *
     * @return the character that caused this error.
     */
    public char getChar() {
        return c;
    }
}
