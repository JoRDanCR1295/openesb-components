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
 * @(#)WrapOtdInputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import com.sun.encoder.runtime.OtdInputStream;

/**
 * Wrapper class to access an OtdInputStream as a byte-array or InputStream.
 * Includes some optimization to avoid double wrapping.
 *
 * @author Michael Libourel
 * @version 
 */
public class WrapOtdInputStream {
    private static final int CHUNK = 0x10000; // input buffer fragment size

    /**
     * Gets the entire contents of the input stream as a byte array.
     * Should be reasonably efficient.
     *
     * @param in  the input stream
     * @param isDirect  use shortcut for InputStream that nukes "in"
     * @return the contents
     * @throws IOException on read problems
     */
    public static byte[] getBytes (OtdInputStream in, boolean isDirect)
        throws IOException {
        if (in == null) {
            throw new NullPointerException("no input");
        }
        if (in instanceof StringOtdInputStreamImpl) {
            return ((StringOtdInputStreamImpl) in).getData();
        }

        int last;
        byte[] data = new byte[CHUNK];
        ByteArrayOutputStream outBytes = new ByteArrayOutputStream();

        if (isDirect && (in instanceof SimpleOtdInputStreamImpl)) {
            // Special case handling to avoid the rewind limit on InputStream;
            // note this implicitly invalidates the SimpleOtdInputStream!
            InputStream ins = ((SimpleOtdInputStreamImpl) in).getInputStream();
            do {
                last = ins.read(data);
                if (last > 0) {
                    outBytes.write(data, 0, last);
                }
            } while (last != -1);
            ins.close();
        } else {
            // Regular rewindable.
            in.begin();
            do {
                last = in.read(data);
                if (last > 0) {
                    outBytes.write(data, 0, last);
                }
            } while (last != -1);
            in.end();
        }
        return outBytes.toByteArray();
    }

    /**
     * Gets the entire contents of the input stream as a byte array.
     *
     * @param in  the input stream
     * @return the contents
     * @throws IOException on read problems
     */
    public static byte[] getBytes (OtdInputStream in)
        throws IOException {
        return getBytes(in, false);
    }

    /**
     * Gets the entire contents of the input stream as an input stream.
     *
     * @param in  the input stream
     * @return the wrapped stream
     * @throws IOException on read problems
     */
    public static InputStream getInputStream (OtdInputStream in)
        throws IOException {
        if (in == null) {
            throw new NullPointerException("no input");
        }
        if (in instanceof SimpleOtdInputStreamImpl) {
            return ((SimpleOtdInputStreamImpl) in).getInputStream();
        }
        return new InStream(in);
    }

    /**
     * Simplistic wrapper class for an OtdInputStream.
     * NYI: Should map more calls to be more efficient, and add mark().
     */
    private static class InStream
        extends InputStream {
        private final OtdInputStream mIn;

        /**
         * Creates on an underlying OTD input stream.
         * We assume the stream is positioned at the beginning, and that
         * begin() has not been called yet.
         *
         * @param in  the non-null OTD input stream
         */
        public InStream (OtdInputStream in)
            throws IOException {
            mIn = in;
            in.begin();
        }

        /**
         * The number of bytes to try an get for available().
         */
        private static final int ASK = 10000;

        /*------------------*\
        |  REQUIRED METHODS  |
        \*------------------*/

        /**
         * Returns the number of bytes that can be read (or skipped over)
         * from this input stream without blocking by the next caller of
         * a method for this input stream. The next caller might be the
         * same thread or or another thread.
         *
         * As OtdInputStream has no size() or available(), we try to guess
         * by skipping some bytes and seeing if that fails. We assume that
         * returning a large but not complete subset of available data is
         * okay.
         *
         * @return a number of bytes, 0 for EOF
         */
        @Override
        public int available()
            throws IOException {
            long at = mIn.mark().offset();
            for (int ask = ASK; ask > 0; ask /= 2) {
                try {
                    mIn.skip(ask);
                    mIn.rewind();
                    mIn.skip(at);
                    return ask;
                } catch (IOException ioe) { }
            }
            return 0;
        }

        /**
         * Reads the next byte of data from the input stream.
         *
         * @return character value
         * @throws IOException on read problems
         */
        public int read ()
            throws IOException {
            return mIn.read();
        }

        /**
         * Reads some number of bytes from the input stream and stores them
         * into the buffer array b.
         *
         * @param b  the buffer
         * @return the number of bytes read
         * @throws IOException on read problems
         */
        @Override
        public int read (byte[] b)
            throws IOException {
            return mIn.read(b);
        }

        /**
         * Releases the resources held by this stream.
         *
         * @throws IOException on input problems
         */
        @Override
        public void close ()
            throws IOException {
            mIn.end();
        }
    }
}
