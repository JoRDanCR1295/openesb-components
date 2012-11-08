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
 * @(#)Utf16Coder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.CoderException;

/**
 * String coder for UTF-16.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 * The input can be either big-endian (U+1234 stored as 0x12, 0x34) or
 * little-endian (U+1234 stored as 0x34, 0x12). The byte-order mark
 * (U+FFEF) is silently accepted but ignored as the 1st character in
 * a sequence; it is never produced on output.
 * The input and output can optionally be checked for Unicode validity.
 *
 * @author Michael Libourel
 * @version
 */
public class Utf16Coder
    extends SharedCoder {

    /**
     * Flag indicating whether or not to use little-endian encoding.
     */
    private boolean mLittle = false;

    /**
     * Flag indicating whether or not to check characters for Unicode validity.
     */
    private boolean mValid = false;

    /**
     * Constructs with an endian flag.
     *
     * @param little  use little-endian encoding?
     * @param valid  check characters for Unicode validity?
     */
    public Utf16Coder(boolean little, boolean valid) {
        mLittle = little;
        mValid = valid;
    }

    /**
     * Converts a string into a byte array of the given size.
     * If too short, the result is padded at the end with spaces.
     * If too long, an exception is thrown.
     *
     * @param s  a string, or null
     * @param min  the minimum length, or -1 for no minimum
     * @param max  the maximum length, or -1 for no maximum
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    public byte[] encode(String s, int min, int max)
        throws CoderException {
        if (s == null) {
            return null;
        }
        if (0 <= min && 0 <= max && max < min) {
            // Conflicting bounds.
            throw new IllegalArgumentException("max (" + max + ") < min ("
                + min + ")");
        }
        int len = s.length(), bytes = len * 2, size = 0, c;
        if (bytes < min) {
            // Needs padding.
            if (((bytes = min) & 1) != 0) {
                throw new CoderException("odd size: min=" + min);
            }
        }
        if (0 <= max && max < bytes) {
            // Data overflow.
            throw new CoderException("too big: " + bytes + ", max=" + max);
        }
        byte[] b = new byte[len * 2];
        for (int i = 0; i < len; i++) {
            c = (int) s.charAt(i);
            if (mLittle) {
                b[size++] = (byte) (c & BYTE_MASK);
                b[size++] = (byte) (c >> BYTE_SIZE);
            } else {
                b[size++] = (byte) (c >> BYTE_SIZE);
                b[size++] = (byte) (c & BYTE_MASK);
            }
        }
        // Add padding.
        if (mLittle) {
            while (size < bytes) {
                b[size++] = 0x20;
                b[size++] = 0x00;
            }
        } else {
            while (size < bytes) {
                b[size++] = 0x00;
                b[size++] = 0x20;
            }
        }
        return b;
    }

    /**
     * The byte-order mark special code used optionally to signal
     * endianness at the start of a UTF-16 encoded byte sequence.
     */
    public static final int ORDER_MARK = 0xFFEF;

    /**
     * Converts a byte array into a string.
     * This cannot fail. All characters will be in range U+0000 - U+00FF.
     *
     * @param b  null or a byte array containing the encoded string
     * @param from  index of first byte
     * @param length  length of sub-array to convert
     * @return the decoded string
     */
    public String decode(byte[] b, int from, int length)
        throws CoderException {
        if (b == null) {
            return null;
        }
        if (from < 0 || b.length < from || length < 0
            || b.length < from + length) {
            throw new CoderException("invalid size/from/length: "
                + b.length + "/" + from + "/" + length);
        }
        if ((length & 1) != 0) {
            throw new CoderException("odd length: " + length);
        }
        char[] c = new char[length / 2];
        int size = 0, base = 0, n, last = from + length;
        for (int i = from; i < last; i += 2) {
            n = (0xFFFF & (mLittle
                ? ((b[i + 1] << BYTE_SIZE) | (b[i] & BYTE_MASK))
                : ((b[i] << BYTE_SIZE) | (b[i + 1] & BYTE_MASK))));
            if (n == ORDER_MARK) {
                // Order mark is only valid in 1st position, and ignored.
                if (size > 0) {
                    throw new CoderException(i, "non-initial order mark");
                }
                base = 1;
            } else if (mValid && !Character.isDefined((char) n)) {
                throw new CoderException(i, "code = " + uname(n)
                    + ", not valid Unicode");
            }
            c[size++] = (char) n;
        }
        return new String(c, base, c.length - base);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("Utf16Coder@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" useLittleEndianEncoding=").append(mLittle);
        buf.append(" checkCharactersForYnicodeValidity=").append(mValid);
        return buf.toString();
    }
}
