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
 * @(#)NestedStringCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime;

import com.sun.encoder.runtime.CoderException;
import com.sun.encoder.runtime.StringCoder;

/**
 * Dynamic string coder, wraps around another string coder that may change.
 * This way, the encoding supported by this nested string coder can be
 * changed dynamically.
 *
 * <p>
 * This is useful for switching the encoding in the current architecture,
 * which originally assumed that encoding would be fixed at design time.
 *
 * @author Michael Libourel
 * @version
 */
public class NestedStringCoder
    implements StringCoder {

    private StringCoder mCoder = null;

    /**
     * Constructs with optinally an initial encoding.
     *
     * @param coder  the encoding, or null for none
     */
    public NestedStringCoder(StringCoder coder) {
        mCoder = coder;
    }

    /**
     * Retrieves the current coder.
     *
     * @return the current coder, or null for none
     */
    public StringCoder getCoder() {
        return mCoder;
    }

    /**
     * Sets the current coder.
     *
     * @param coder  the new coder, or null for none
     */
    public void setCoder(StringCoder coder) {
         mCoder = coder;
    }

    // Asserts we have a current coder.
    private void haveCoder() {
        if (mCoder == null) {
           throw new RuntimeException("No current coder.");
        }
    }

    /*------------------*\
    |  REQUIRED METHODS  |
    |   by StringCoder   |
    \*------------------*/

    /**
     * Converts a string into a byte array.
     *
     * @param s  a string, or null
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    public byte[] encode(String s)
        throws CoderException {
        haveCoder();
        return mCoder.encode(s);
    }

    /**
     * Converts a string into a byte array of the given size.
     * If too short, the result is padded at the end with a padding
     * determined by the encoder. If too long, an exception is thrown.
     *
     * @param s  a string, or null
     * @param min  the minimum length, or -1 for no minimum
     * @param max  the maximum length, or -1 for no maximum
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    public byte[] encode(String s, int min, int max)
        throws CoderException {
        haveCoder();
        return mCoder.encode(s, min, max);
    }

    /**
     * Converts a byte array into a string.
     *
     * @param b  null or a byte array containing the encoded string
     * @return the decoded string
     * @throws CoderException for uninterpretable byte sequences
     */
    public String decode(byte[] b)
        throws CoderException {
        haveCoder();
        return mCoder.decode(b);
    }

    /**
     * Converts a byte array slice into a string.
     *
     * @param b  null or a byte array containing the encoded string
     * @param from  index of first byte
     * @param length  length of sub-array to convert
     * @return the decoded string
     * @throws CoderException for uninterpretable byte sequences
     */
    public String decode(byte[] b, int from, int length)
        throws CoderException {
        haveCoder();
        return mCoder.decode(b, from, length);
    }

    /**
     * Sets the value of the named property.
     *
     * @param name  name of property
     * @param value  new value of property
     * @throws IllegalArgumentException for unknown property or wrong value
     */
    public void setProperty(String name, Object value) {
        haveCoder();
        mCoder.setProperty(name, value);
    }

    /**
     * Retrieves the value of the named property.
     *
     * @param name  name of property
     * @return value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    public Object getProperty(String name) {
        haveCoder();
        return mCoder.getProperty(name);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("NestedStringCoder@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" stringCoder=").append(mCoder);
        return buf.toString();
    }
}
