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
 * @(#)EncoderException.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

/**
 * This class specifies an exception occured during the encoding or decoding
 * process.
 *
 * @author  Jun Yang
 * @version
 */
public class EncoderException
    extends Exception {

    private static final long serialVersionUID = 1L;

    Throwable mCause = null;

    /**
     * Creates new LayerException
     *
     */
    public EncoderException() {
        super();
    }

    /**
     * Creates a new EncoderException
     *
     * @param msg DOCUMENT ME!
     */
    public EncoderException(String msg) {
        super(msg);
    }

    /**
     * Creates a new EncoderException with info and original Exception.
     *
     * @param msg detail Excetpion message
     * @param orig original Excetpion
     */
    public EncoderException(
            String    msg,
            Throwable orig) {
        super(msg);
        mCause = orig;
    }

    /**
     * Creates a new EncoderException with Throwable
     *
     * @param cause Throwable instance
     */
    public EncoderException(Throwable cause) {
        super();
        mCause = cause;
    }

    /**
     * Retrieves the cause of this throwable or null if the cause is not set.
     *
     * @return the cause
     */
    @Override
    public Throwable getCause() {
        return mCause;
    }
}
