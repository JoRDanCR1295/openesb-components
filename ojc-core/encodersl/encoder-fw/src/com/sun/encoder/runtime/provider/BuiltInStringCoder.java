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
 * @(#)BuiltInStringCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

import com.sun.encoder.runtime.StringCoder;
import com.sun.encoder.runtime.CoderException;

/**
 * An implementation of the string coder interface, that serves as a
 * straight-forward map to the built-in encoding support of the JDK,
 * visible though java.lang.String.
 *
 * @author Michael Libourel
 * @version
 */
public class BuiltInStringCoder
    implements StringCoder {

    /**
     * Property name for the Java encoding name.
     */
    public static final String CODE = "code";

    /**
     * Property name for the padding bytes value.
     */
    public static final String PAD = "pad";

    private final String mEncoding;
    private final byte[] mPadding;
    private final CharsetDecoder mDecoder;
    private final CharsetEncoder mEncoder;

    /**
     * Creates from encoding name.
     * Optionally the Java name can be followed by ",byte=123" to specify
     * a padding sequence of byte 123 (currently limited to 1 byte).
     *
     * @param enc  name of encoding
     * @throws UnsupportedEncodingException encoding not supported
     */
    public BuiltInStringCoder(String enc)
        throws UnsupportedEncodingException {
        if (enc == null) {
            throw new UnsupportedEncodingException("null encoding name");
        }
        int at = enc.indexOf(",pad=");
        byte[] pad = null;
        if (0 <= at) {
            try {
                int value = Integer.parseInt(enc.substring(at + 5));
                pad = new byte[] { (byte) value };
                enc = enc.substring(0, at);
            } catch (NumberFormatException n) {
                throw new IllegalArgumentException("invalid padding: ["
                    + enc.substring(at + 5) + "]");
            }
        }
        this.mEncoding = enc;
        this.mPadding = pad;
        Charset cs = Charset.forName(enc);
        this.mDecoder = cs.newDecoder();
        this.mEncoder = cs.newEncoder();
    }

    /**
     * Creates from encoding name.
     * The encoding name is available through the {@link #CODE} property,
     * the padding array through the {@link #PAD} property.
     * Note that there is no guard again partial padding sequences on output.
     *
     * @param enc  name of encoding
     * @param pad  value for padding; null or empty will default
     * @throws UnsupportedEncodingException encoding not supported
     */
    public BuiltInStringCoder(String enc, byte[] pad)
        throws UnsupportedEncodingException {
        if (enc == null) {
            throw new UnsupportedEncodingException("null encoding name");
        }
        if (pad == null || pad.length == 0) {
            pad = new byte[] { 0x20 };
        }
        this.mEncoding = enc;
        this.mPadding = pad;
        Charset cs = Charset.forName(enc);
        this.mDecoder = cs.newDecoder();
        this.mEncoder = cs.newEncoder();
    }

    /**
     * Converts a string into a byte array.
     *
     * @param s  a string, or null
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    public byte[] encode(String s)
        throws CoderException {
        if (null == s) {
            return null;
        }
        mEncoder.reset();
        float f = s.length() * mEncoder.maxBytesPerChar();
        ByteBuffer bf = ByteBuffer.allocate((int) f);
        CharBuffer cb = CharBuffer.wrap(s);
        boolean endOfInput = true;
        CoderResult cr = mEncoder.encode(cb, bf, endOfInput);
        if (cr.isError()) {
            int at = cr.length();
            throw new CoderException("Unable to convert string, char[" + at
                + "]=" + SharedCoder.uname(s.charAt(at))
                + ". " + cr.toString());
        }
        bf.flip();
        byte[] result = new byte[bf.limit()];
        bf.get(result);
        return result;
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
        if (null == s) {
            return null;
        }
        mEncoder.reset();
        CharBuffer cb = CharBuffer.wrap(s);
        int len = (max < 0
            ? (int) (mEncoder.maxBytesPerChar() * s.length())
            : max);
        ByteBuffer buf = ByteBuffer.allocate(len);
        CoderResult cr = mEncoder.encode(cb, buf, true);
        if (cr.isError()) {
            if (cr.isUnmappable()) {
                int at = cr.length();
                throw new CoderException("String unencodable, char[" + at
                    + "]=" + SharedCoder.uname(s.charAt(at)));
            }
            if (cr.isMalformed()) {
                int at = cr.length();
                throw new CoderException("String malformed, char[" + at
                    + "]=" + SharedCoder.uname(s.charAt(at)));
            }
        }
        if (cr.isOverflow()) {
            int at = cr.length();
            throw new CoderException("String too big, did "
                + at + " of " + s.length()
                + (max < 0 ? "" : (", max bytes=" + max)));
        }
        int to = buf.position();
        if (0 <= min) {
            while (to < min) {
                for (int j = 0; j < mPadding.length && to < min; j++, to++) {
                    buf.put(mPadding[j]);
                }
            }
        }
        buf.flip();
        byte[] copy = new byte[to];
        buf.get(copy);
        return copy;
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
        if (null == b) {
            return null;
        }
        mDecoder.reset();
        try {
            CharBuffer cb = mDecoder.decode(ByteBuffer.wrap(b));
            if (cb.position() > 0) {
                cb.flip();
            }
            return cb.toString();
        } catch (CharacterCodingException e) {
            throw new CoderException("Unable to convert string.", e);
        }
    }

    /**
     * Converts a byte array slice into a string.
     * XXX: Could improve performance by calling ByteToCharConverter's method
     * convert(byte[],int,int,char[],int,int) directly.
     *
     * @param b  null or a byte array containing the encoded string
     * @param from  index of first byte
     * @param length  length of sub-array to convert
     * @return the decoded string
     * @throws CoderException for uninterpretable byte sequences
     */
    public String decode(byte[] b, int from, int length)
        throws CoderException {
        if (from < 0) {
            throw new IllegalArgumentException("negative offset: " + from);
        }
        if (length < 0) {
            throw new IllegalArgumentException("negative length: " + length);
        }
        if (b == null) {
            return null;
        }
        if (b.length < from + length) {
            throw new IllegalArgumentException("offset + length < size: "
                + from + ", " + length + ", " + b.length);
        }
        if (0 < from || length != b.length) {
            byte[] copy = new byte[length];
            System.arraycopy(b, from, copy, 0, length);
            b = copy;
        }
        return decode(b);
    }

    /**
     * Sets the value of the named property.
     *
     * @param name  name of property
     * @param value  new value of property
     * @throws IllegalArgumentException for unknown property or wrong value
     */
    public void setProperty(String name, Object value) {
        throw new IllegalArgumentException(getClass()
            + " has no [" + name + "] property");
    }

    /**
     * Retrieves the value of the named property.
     * Supports {@link #CODE} (string value).
     *
     * @param name  name of property
     * @return value of property, to be treated as immutable
     * @throws IllegalArgumentException if property not supported by encoder
     */
    public Object getProperty(String name) {
        if (CODE.equals(name)) {
            return mEncoding;
        }
        if (PAD.equals(name)) {
            return mPadding;
        }
        throw new IllegalArgumentException(getClass()
            + " has no [" + name + "] property");
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("BuiltInStringCoder@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" encoding=").append(mEncoding == null ? "null" : mEncoding);
        buf.append(" decoder=").append(mDecoder == null ? "null" : mDecoder);
        buf.append(" encoder=").append(mEncoder == null ? "null" : mEncoder);
        if (mPadding != null) {
            buf.append(" padding=").append(Misc.printable(mPadding));
        }
        return buf.toString();
    }
}
