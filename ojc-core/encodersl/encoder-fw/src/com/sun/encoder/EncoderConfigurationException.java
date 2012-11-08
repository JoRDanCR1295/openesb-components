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
 * @(#)EncoderConfigurationException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

/**
 * This class specifies an exception occured during the creation of an
 * EncoderFactory instance or the lookup of an encoder.
 *
 * @author Jun Yang
 */
public class EncoderConfigurationException extends Exception {
    
    private static final long serialVersionUID = 1L;
    
    Throwable mCause = null;
    
    /**
     * Creates a new EncoderConfigurationException.
     *
     */
    public EncoderConfigurationException() {
        super();
    }
    
    /**
     * Creates a new EncoderConfigurationException from an error message.
     *
     * @param msg exception message
     */
    public EncoderConfigurationException(String msg) {
        super(msg);
    }
    
    /**
     * Creates a new EncoderConfigurationException with info and
     * original Exception.
     *
     * @param msg detail Excetpion message
     * @param orig original Excetpion
     */
    public EncoderConfigurationException(
            String    msg,
            Throwable orig) {
        super(msg);
        mCause = orig;
    }
    
    /**
     * Creates a new EncoderConfigurationException with a Throwable.
     *
     * @param cause Throwable instance
     */
    public EncoderConfigurationException(Throwable cause) {
        super();
        mCause = cause;
    }
    
    /**
     * Retrieves the cause of this throwable or null if the cause does not
     * exist.
     * 
     * @return the cause
     */
    @Override
    public Throwable getCause() {
        return mCause;
    }
}
