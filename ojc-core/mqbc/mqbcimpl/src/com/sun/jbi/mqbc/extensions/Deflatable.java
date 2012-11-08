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
 */

/*
 * @(#)$Id: Deflatable.java,v 1.1 2008/11/12 23:00:19 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.PrintWriter;

/**
 * Deflatable objects know how to serialize themselves in a character encoding.
 *
 * @author Noel.Ang@sun.com
 */
public interface Deflatable {

    /**
     * Serialize.
     *
     * @param pw The serialization outlet.
     * @param context User-defined context.
     *
     * @throws com.sun.jbi.mqbc.extensions.Deflatable.DeflateException if any
     * problems occur during the serialization process.
     */
    void deflate(PrintWriter pw, Deflatable.Context context)
            throws DeflateException;

    /** General mechanism for passing implementation-specific context. */
    static interface Context {
        Object getContext(Object key);
    }

    /** Indicates that a serialization attempt failed. */
    static class DeflateException extends Exception {
        public DeflateException(Throwable cause) {
            super(cause);
        }

        public DeflateException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}
