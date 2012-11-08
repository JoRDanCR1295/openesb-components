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
 * @(#)Utf8Coder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.CoderException;

/**
 * String coder for UTF-8.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * The input encoding can optionally be checked for strict compliance,
 * i.e. that U+0009 is encoded as 0x09, not as 0xC0 0x89 or 0xE0 0x80 0x89.
 * The input and output can optionally be checked for Unicode validity.
 *
 * @author Michael Libourel
 * @version
 */
public class Utf8Coder
    extends SharedCoder {

    /**
     * Flag indicating whether or not be strict on input encoding.
     */
    private boolean mStrict = false;

    /**
     * Flag indicating whether or not to check characters for Unicode validity.
     */
    private boolean mValid = false;

    /**
     * The padding byte for encoding.
     */
    private static final byte PAD_BYTE = 0x20;

    /**
     * Constructs with a strictness flag.
     *
     * @param strict  be strict on input encoding?
     * @param valid  check characters for Unicode validity?
     */
    public Utf8Coder(boolean strict, boolean valid) {
        mStrict = strict;
        mValid = valid;
    }

    /**
     * Constructs non-strict, non-validating.
     */
    public Utf8Coder() {
        this(false, false);
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
        int len = s.length(), size = 0, c;
        for (int i = 0; i < len; i++) {
            if ((c = (int) s.charAt(i)) < 0x80) {
                // Code 0xxxxxxx.
                size++;
            } else if (c < 0x800) {
                // Code 110xxxxx 10xxxxxx.
                size += 2;
            } else if (c < 0xFFFE) {
                // Code 1110xxxx 10xxxxxx 10xxxxxx.
                size += 3;
            } else {
                // Cannot express in encoding.
                // XXX: should use CoderException
                throw new CoderException("char #" + i + " = " + uname(c)
                    + ", not encodable");
            }
            if (mValid && !Character.isDefined((char) c)) {
                throw new CoderException("char #" + i + " = " + uname(c)
                    + ", not valid Unicode");
            }
        }
        if (size < min) {
            size = min;
        }
        if (0 <= max && max < size) {
            // Data overflow.
            throw new CoderException("too big: " + size + ", max=" + max);
        }
        byte[] b = new byte[size];
        size = 0;
        for (int i = 0; i < len; i++) {
            if ((c = (int) s.charAt(i)) < 0x80) {
                // Code 0xxxxxxx.
                b[size++] = (byte) c;
            } else if (c < 0x800) {
                // Code 110xxxxx 10xxxxxx.
                b[size++] = (byte) (0xC0 | (c >> 6));
                b[size++] = (byte) (0x80 | (c & 0x3F));
            } else if (c < 0xFFFE) {
                // Code 1110xxxx 10xxxxxx 10xxxxxx.
                b[size++] = (byte) (0xE0 | (c >> 12));
                b[size++] = (byte) (0x80 | ((c >> 6) & 0x3F));
                b[size++] = (byte) (0x80 | (c & 0x3F));
            }
        }
        while (size < b.length) {
            b[size++] = PAD_BYTE;
        }
        return b;
    }

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
        int size = 0, n, e1, e2;
        for (int i = from; i < length; i++) {
            if ((b[i] & 0xC0) != 0x80) {
                size++;
            }
        }
        char[] c = new char[size];
        size = 0;
        for (int i = from; i < length; i++) {
            if ((n = (b[i] & BYTE_MASK)) < 0x80) {
                // Code 0xxxxxxx.
            } else if ((n & 0xE0) == 0xC0) {
                // Code 110xxxxx 10xxxxxx.
                if (i + 1 == b.length) {
                    throw new CoderException(i, "needs extra");
                }
                if (((e1 = b[++i]) & 0xC0) != 0x80) {
                    throw new CoderException(i, "byte = "
                        + bname(e1 & BYTE_MASK) + ", invalid extra");
                }
                n = (((n & 0x1F) << 6) | (e1 & 0x3F));
                if (mStrict && n < 0x80) {
                    throw new CoderException(i - 1,
                        "uses 2-byte code for " + n);
                }
            } else if ((n & 0xF0) == 0xE0) {
                // Code 1110xxxx 10xxxxxx 10xxxxxx.
                if (i + 2 >= b.length) {
                    throw new CoderException(i, "needs 2 extra");
                }
                if (((e1 = b[++i]) & 0xC0) != 0x80) {
                    throw new CoderException(i, "byte = "
                        + bname(e1 & BYTE_MASK) + ", invalid 1st extra");
                }
                if (((e2 = b[++i]) & 0xC0) != 0x80) {
                    throw new CoderException(i, "byte = "
                        + bname(e2 & BYTE_MASK) + ", invalid 2nd extra");
                }
                if (mStrict && n < 0x80) {
                    throw new CoderException("byte #" + (i - 1)
                        + " uses 2-byte code for " + n);
                }
                n = (((n & 0x0F) << 12) | ((e1 & 0x3F) << 6) | (e2 & 0x3F));
                if (mStrict && n < 0x800) {
                    throw new CoderException(i - 1,
                        " uses 3-byte code for " + n);
                }
            } else {
                throw new CoderException(i, "byte = " + bname(n & BYTE_MASK)
                    + ", invalid start");
            }
            c[size++] = (char) n;
            if (mValid && !Character.isDefined((char) n)) {
                throw new CoderException(i, "code = " + uname(n)
                    + ", not valid Unicode");
            }
        }
        return new String(c);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("Utf8Coder@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" beStrictOnInputEncoding=").append(mStrict);
        buf.append(" checkCharactersForYnicodeValidity=").append(mValid);
        return buf.toString();
    }
}
