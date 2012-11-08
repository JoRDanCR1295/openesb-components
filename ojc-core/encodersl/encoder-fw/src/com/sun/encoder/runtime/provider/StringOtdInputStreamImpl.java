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
 * @(#)StringOtdInputStreamImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.OtdInputStream;
import com.sun.encoder.runtime.OtdInputStreamMark;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

/**
 * A simple OtdInputStream, based on a string.
 *
 * @author Michael Libourel
 * @version 
 */
public class StringOtdInputStreamImpl
    implements OtdInputStream {
    private byte[] mData;
    private int mPos = 0;

    /**
     * Creates from given input bytes.
     *
     * @param data  the input data
     */
    public StringOtdInputStreamImpl (byte[] data) {
        if (data == null) {
            throw new NullPointerException("no data");
        }
        mData = data;
    }

    /**
     * Creates from given input string.
     *
     * @param data  the input data
     */
    public StringOtdInputStreamImpl (String data) {
        try {
            mData = data.getBytes("UTF-8");
        } catch (UnsupportedEncodingException ue) {
            throw new RuntimeException("platform sucks: " + ue.getMessage());
        }
    }

    /**
     * Get the complete input data.
     * The result should be treated as an immutable array.
     * This is meant for the WrapOtdInputStream shortcut.
     *
     * @return the data
     */
    protected byte[] getData () { return mData; }

    /*------------------*\
    |  REQUIRED METHODS  |
    \*------------------*/

    /**
     * Announces the start of parsing a message. A rewind should return to this
     * point.
     *
     */
    public void begin () { }

    /**
     * Terminates consumption of the stream by the parser.
     *
     * @return whether there was any data left in the stream
     */
    public boolean end () { return mPos < mData.length; }

    /**
     * Tests if the stream cursor has reached the end of the nput data.
     * The effect of calling this method before begin() or after end()
     * is not defined.
     *
     * @return true if end of data reached, else false.
     * @throws IOException IOException occurred.
     */
    public boolean eof ()
        throws IOException {
        return mPos >= mData.length;
    }

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.
     *
     * @param buffer the array to receive the data
     * @return the number of bytes added to the buffer, or -1 for EOF
     */
    public int read (byte buffer[]) {
        return read(buffer, 0, buffer.length);
    }

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.
     *
     * @param buffer the array to receive the data
     * @param offset stating index in buffer for new data
     * @param len length
     * @return the number of bytes added to the buffer, or -1 for EOF
     */
    public int read (byte buffer[], int offset, int len) {
        if (buffer == null) {
            throw new NullPointerException("no buffer");
        }
        if (mPos >= mData.length) {
            return -1;
        }
        int count = 0;
        while (offset < buffer.length && len-- > 0 && mPos < mData.length) {
            buffer[offset++] = mData[mPos++];
            count++;
        }
        return count;
    }

    /**
     * Returns the next available byte, else -1 for EOF
     *
     * @return a byte (range 0-255) or -1
     */
    public int read() {
        return (mPos < mData.length) ? (mData[mPos++] & 0xFF) : -1;
    }

    /**
     * Class to wrap a marked source position.
     */
    private static class Mark
        implements OtdInputStreamMark {
        public final int mPos;
        public Mark (int pos) { mPos = pos; }
        public long offset () { return (long) mPos; }
    }

    /**
     * Marks the repeatable node.
     *
     * @return cookie to return to position later
     */
    public OtdInputStreamMark mark() {
        return new Mark(mPos);
    }

    /**
     * Return to position marked earlier.
     *
     * @param mark cookie returned by mark()
     *
     * @throws IOException for input problem
     */
    public void seek (OtdInputStreamMark mark)
        throws IOException {
        if (mark == null) {
            throw new NullPointerException("no mark");
        }
        int pos = ((Mark) mark).mPos;
        if (pos < 0 || mData.length < pos) {
            throw new IOException("impossible offset " + pos);
        }
        mPos = pos;
    }

    /**
     * Return to the very beginnig of the stream, which is implicitly marked
     *
     */
    public void rewind () { mPos = 0; }

    /**
     * Writes the offset of a token when calling mark back to stream.
     *
     * @param buffer the array with the offset of the token
     * @param offset stating index in buffer for new data
     * @param len the maximum of bytes to write
     *
     * @throws IOException ioexception
     */
    public void writeMarkOffset(byte buffer[], int offset, int len)
        throws IOException {
    }

    /**
     * Skips the given number of bytes ahead.
     * Trying to skip past the end-of-data will throw an I/O exception.
     *
     * @param count  the number of bytes to skip; non-negative number
     * @throws IOException for input problem
     */
    public void skip (long count)
        throws IOException {
        if (count < 0) {
            throw new IllegalArgumentException("negative count");
        }
        if (mPos + count > mData.length) {
            throw new IOException("tried to skip " + count + ", only "
                + (mData.length - mPos) + " available");
        }
        mPos += count;
    }
}
