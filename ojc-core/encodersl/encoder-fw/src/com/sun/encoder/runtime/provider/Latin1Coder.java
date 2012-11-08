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
 * @(#)MultiByteCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.CoderException;
import com.sun.encoder.runtime.StringCoder;

/**
 * String coder for ISO-8859-1, which maps the lowest code page
 * of Unicode / ISO-10646 to the same 256 code points as byte values.
 *
 * This class has one extra property, named by CALLS, to record the number
 * of encode() and decode() calls. This is meant for debugging and as
 * example code.
 *
 * @author Michael Libourel
 * @version
 */
public class Latin1Coder
    extends SharedCoder {

    public static final String CALLS = "calls";
    private int mCalls = 0;

    /**
     * Converts a string into a byte array of the given size.
     * If too short, the result is padded at the end with a space.
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
        mCalls++;
        if (s == null) {
            return null;
        }
        if (0 <= min && 0 <= max && max < min) {
            // Conflicting bounds.
            throw new IllegalArgumentException("max (" + max + ") < min ("
                + min + ")");
        }
        int len = s.length(), size = len;
        if (size < min) {
            size = min;
        }
        if (0 <= max && max < size) {
            // Data overflow.
            throw new CoderException("too big: " + size + ", max=" + max);
        }
        byte[] b = new byte[size];
        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);
            if (0xFF < c) {
                // Cannot express in encoding.
                // XXX: should use CoderException
                throw new CoderException("char #" + i + " = " + uname(c)
                    + ", not encodable");
            }
            b[i] = (byte) c;
        }
        // Append the padding.
        while (len < size) {
            b[len++] = 0x20;
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
    public String decode(byte[] b, int from, int length) {
        mCalls++;
        if (b == null) {
            return null;
        }
        if (from < 0 || b.length < from || length < 0
            || b.length < from + length) {
            throw new CoderException("invalid size/from/length: "
                + b.length + "/" + from + "/" + length);
        }
        char[] c = new char[length];
        for (int i = 0; i < length; i++) {
            c[i] = (char) (b[from + i] & 0xFF);
        }
        return new String(c);
    }

    /**
     * Sets the value of the named property.
     * Supported: {@link #CALLS}.
     *
     * @param name  name of property
     * @param value  new value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    @Override
    public void setProperty(String name, Object value) {
        if (CALLS.equals(name)) {
            if (!(value instanceof Integer)) {
                throw new IllegalArgumentException("'" + CALLS
                    + "' requires Integer, not "
                    + ((value == null)
                        ? "null"
                        : value.getClass().getName()));
            }
            mCalls = ((Integer) value).intValue();
        }
        super.setProperty(name, value);
    }

    /**
     * Retrieves the value of the named property.
     * Supported: {@link #CALLS}, {@link StringCoder#SHARE}.
     *
     * @param name  name of property
     * @return value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    @Override
    public Object getProperty(String name) {
        if (CALLS.equals(name)) {
            return new Integer(mCalls);
        }
        return super.getProperty(name);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("Latin1Coder@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" calls=").append(mCalls);
        return buf.toString();
    }
}

