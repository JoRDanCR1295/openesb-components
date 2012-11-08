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
 * @(#)SharedCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.CoderException;
import com.sun.encoder.runtime.StringCoder;

/**
 * Base class for stateless, sharable string coders.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * @author Michael Libourel
 * @version
 */
public abstract class SharedCoder
    implements StringCoder {

    public static final int BYTE_MASK = 0xFF;
    public static final int BYTE_SIZE = 8;

    public abstract byte[] encode(String s, int min, int max)
        throws CoderException;

    public abstract String decode(byte[] b, int from, int length)
        throws CoderException;

    protected static char hex(int n) {
        return "0123456789ABCDEF".charAt(n & 0xF);
    }

    protected static String hex2(int n) {
        return new String(new char[] { hex(n >> 4), hex(n) });
    }

    protected static String hex4(int n) {
        return new String(new char[] { hex(n >> 12), hex(n >> 8),
            hex(n >> 4), hex(n) });
    }

    /**
     * Returns a hexadecimal representation of a byte array slice.
     *
     * @param b  the byte-array
     * @param from  the index of the first byte to show
     * @param to  one past the index of the last byte to show
     * @return a string of hexadecimal digits
     */
    protected static String hex(byte[] b, int from, int to) {
        char[] c = new char[(to - from) * 2];
        for (int pos = 0; from < to; from++) {
            c[pos++] = hex(b[from] >> 4);
            c[pos++] = hex(b[from] >> 0);
        }
        return new String(c);
    }

    protected static String hex(byte[] b) {
        return hex(b, 0, b.length);
    }

    /**
     * Returns Unicode name like "U+1234" for code point.
     *
     * @param c  the code point
     * @return the Unicode numeric name
     */
    public static String uname(int c) {
        if (c < 0 || 0xFFFF < c) {
            return "[" + c + "]";
        }
        return "U+" + hex4(c);
    }

    public static String bname (int b) {
        return "0x" + hex(b >> 4) + hex(b);
    }

    /**
     * Converts a string into a byte array.
     *
     * @param s  a string, or null
     * @return byte array containing the encoded string
     */
    public byte[] encode(String s)
        throws CoderException {
        return encode(s, -1, -1);
    }

    /**
     * Converts a byte array into a string.
     * This cannot fail. All characters will be in range U+0000 - U+00FF.
     *
     * @param b  null or a byte array containing the encoded string
     * @return the decoded string
     */
    public String decode(byte[] b) {
        if (b == null) {
            return null;
        }
        return decode(b, 0, b.length);
    }

    /**
     * Sets the value of the named property. None supported at present.
     *
     * @param name  name of property
     * @param value  new value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    public void setProperty(String name, Object value) {
        if (name == null) {
            throw new NullPointerException("no name");
        }
        throw new IllegalArgumentException("unknown property '"
            + name + "'");
    }

    /**
     * Retrieves the value of the named property.
     * Supported: {@link StringCoder#SHARE}, always true.
     *
     * @param name  name of property
     * @return value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    public Object getProperty(String name) {
        if (name == null) {
            throw new NullPointerException("no name");
        }
        if (name.equals(StringCoder.SHARE)) {
            return new Boolean(true);
        }
        throw new IllegalArgumentException("unknown property '"
            + name + "'");
    }
}
