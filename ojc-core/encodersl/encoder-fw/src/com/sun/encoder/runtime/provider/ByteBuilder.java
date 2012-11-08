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
 * @(#)ByteBuilder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

/**
 * A convenient class to facilitate byte writing, same as StringBuilder.
 *
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public final class ByteBuilder {

    private byte[] mBytes = new byte[32];
    private int mCount;

    /**
     * Resets the builder.
     */
    public void reset() {
        mCount = 0;
    }

    /**
     * Compacts the builder.
     */
    public void compact() {
        byte[] buf = new byte[mCount < 32 ? 32 : mCount];
        if (mCount > 0) {
            System.arraycopy(mBytes, 0, buf, 0, mCount);
        }
        mBytes = buf;
    }

    /**
     * Gets the size of the content in this builder.
     *
     * @return the size of the content in this builder
     */
    public int size() {
        return mCount;
    }

    /**
     * Gets the internal byte array that backs up this builder. Please use it
     * for read-only purpose.
     *
     * @return the internal buffer that backs up this builder.
     */
    public byte[] getBytes() {
        return mBytes;
    }

    /**
     * Return a copy of byte array contains the content.
     *
     * @return a copy of the content in byte array
     */
    public byte[] toByteArray() {
        if (mCount == 0) {
            return new byte[0];
        }
        byte[] buf = new byte[mCount];
        System.arraycopy(mBytes, 0, buf, 0, mCount);
        return buf;
    }

    /**
     * Writes one byte.
     *
     * @param b the byte
     */
    public void write(final byte b) {
        makeRoom(mCount + 1);
        mBytes[mCount++] = b;
    }

    /**
     * Writes a byte array to this builder. Bytes are copied to the internal
     * buffer that backs up this builder.
     *
     * @param b the byte array to be written
     */
    public void write(final byte[] b) {
        makeRoom(mCount + b.length);
        System.arraycopy(b, 0, mBytes, mCount, b.length);
        mCount += b.length;
    }

    /**
     * Writes a byte array to this builder. Bytes are copied to the internal
     * buffer that backs up this builder.
     *
     * @param b the byte array to be written
     * @param off the offset
     * @param len the length
     */
    public void write(byte[] b, int off, int len) {
        if (len < 0) {
            throw new IllegalArgumentException("invalid length=" + len);
        }
        if (off < 0 || off + len > b.length) {
            throw new ArrayIndexOutOfBoundsException(
                "offset=" + off + ", length=" + len + ", b.length=" + b.length);
        }
        if (len == 0) {
            return;
        }
        makeRoom(mCount + len);
        System.arraycopy(b, off, mBytes, mCount, len);
        mCount += len;
    }

    private void makeRoom(int neededSize) {
        if (neededSize < mBytes.length) {
            return;
        }
        byte[] buf = new byte[mBytes.length * 2];
        for (; neededSize > buf.length; buf = new byte[buf.length * 2]) {
            // do nothing here
        }
        if (mCount > 0) {
            System.arraycopy(mBytes, 0, buf, 0, mCount);
        }
        mBytes = buf;
    }
}
