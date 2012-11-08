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
 * @(#)MessageExchangeProcessingException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

/**
 *
 * Exception class for message exchange processing errors.
 */
public class MessageExchangeProcessingException extends Exception {

    /**
     * MessageExchangeProcessingException default constructor
     * @param msg The textual error message
     */
    public MessageExchangeProcessingException() {
        
    }
    
    /**
     * MessageExchangeProcessingException constructor
     * @parm msg String The textual error message
     */
    public MessageExchangeProcessingException (String msg) {
        super(msg);
    }
    
    /**
     * MessageExchangeProcessingException constructor
     * @param cause The cause of the error 
     */
    public MessageExchangeProcessingException (Throwable cause) {
        super(cause);
    }

    /**
     * MessageExchangeProcessingException constructor
     * @param msg The textual error message
     * @param cause The cause of the error for error chaining
     */
    public MessageExchangeProcessingException (String msg, Throwable cause) {
        super(msg,cause);
    }
    
}
