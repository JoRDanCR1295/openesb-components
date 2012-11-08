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
 * @(#)InvalidConfigurationException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.packaging;

/**
 *
 * Exception class reporting errors related to encoder usage, etc.
 */
public class EncoderException extends Exception {

    /** 
     * Creates a new instance of EncoderException 
     */
    public EncoderException() {
    }

    /** 
     * Creates a new instance of EncoderException 
     *
     * @param str Exception message String.
     */
    public EncoderException (String str) {
        super(str);
    }

    /** 
     * Creates a new instance of EncoderException 
     *
     * @param cause The root cause of the exception
     */
    public EncoderException (Throwable cause) {
        super(cause);
    }
    
    /** 
     * Creates a new instance of EncoderException 
     *
     * @param str Exception message String.
     * @param cause The root cause of the exception.
     */
    public EncoderException (String str, Throwable cause) {
        super(str, cause);
    }
}
