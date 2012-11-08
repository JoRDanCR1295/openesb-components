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
 * @(#)SimpleOtdOutputStreamImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.OtdOutputStream;

import java.io.IOException;
import java.io.OutputStream;

/**
 * SimpleOtdOutputStream.java Created on October 1, 2002, 5:23 PM
 *
 * @author Hong Lin
 * @version 
 */
public class SimpleOtdOutputStreamImpl
    implements OtdOutputStream {
    /** DOCUMENT ME! */
    private OutputStream mOut = null;

    /**
     * Creates a new instance of SimpleOtdOutputStream
     *
     * @param out DOCUMENT ME!
     */
    public SimpleOtdOutputStreamImpl(OutputStream out) {
        mOut = out;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public OutputStream getOutputStream() {
        return mOut;
    }

    /**
     * DOCUMENT ME!
     *
     * @param out DOCUMENT ME!
     */
    public void setOutputStream(OutputStream out) {
        mOut = out;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void begin()
        throws IOException {
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void end()
        throws IOException {
        mOut.flush();
        mOut.close();

        // dummy...
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(byte value)
        throws IOException {
        mOut.write(value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param buffer DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(byte buffer[])
        throws IOException {
        mOut.write(
            buffer,
            0,
            buffer.length);
    }

    /**
     * DOCUMENT ME!
     *
     * @param buffer DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param size DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(
        byte  buffer[],
        int   offset,
        int   size)
        throws IOException {
        if (buffer.length < (offset + size)) {
            throw new IllegalArgumentException("offset beyond buffer size");
        }

        while (size-- > 0) {
            write(buffer[offset++]);
        }
    }
}
