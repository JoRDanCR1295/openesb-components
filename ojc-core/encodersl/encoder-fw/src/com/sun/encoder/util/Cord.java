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
 * @(#)Cord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.util;

/**
 * Class to implement an immutable string of Unicode 4.0+ characters.
 * This includes characters in range U+000000 through U+10FFFF, not all of
 * which are representable as Java "char" values. When converting between
 * code points and "char" values, code points over U+FFFF are represented
 * as a pair of substitution chars in range U+D800 - U+DBFF for the 1st and
 * U+DC00 - U+DFFF for the 2nd char. These surrogate chars will not normally
 * appear as code points in a Cord value.
 *
 * @author Michael Libourel
 * @version 
 */
public class Cord {
    protected final int[] mValue;

    public static final int MAX_CODE = 0x10FFFF;
    public static final int MAX_CHAR = 0xFFFF;

    // Surrogate char range bounds.
    private static final int
        SUR_HI_MIN = 0xD800,
        SUR_HI_MAX = 0xDBFF,
        SUR_LO_MIN = 0xDC00,
        SUR_LO_MAX = 0xDFFF;

    /**
     * Constructs from the given array of character values.
     * The input array should not change after this call, we assume it is
     * immutable.
     *
     * @param value  the array
     */
    public Cord (int[] value) {
        if (value == null) {
            throw new NullPointerException("no value");
        }
        for (int i = 0; i < value.length; i++) {
            int c = value[i];
            if (c < 0 || MAX_CODE < c) {
                throw new IllegalArgumentException("invalid char[" + i + "]:"
                   + c);
            }
        }
        mValue = value;
    }

    /**
     * Creates from a string value, converting surrogate sequences if valid.
     *
     * @param value  the string
     */
    public Cord (String value) {
        if (value == null) {
            throw new NullPointerException("no value");
        }
        int len = value.length(), pos = 0, c, d;
        int[] v = new int[value.length()];
        for (int i = 0; i < len; i++) {
            if (SUR_HI_MIN <= (c = value.charAt(i))
                && c <= SUR_HI_MAX
                && i + 1 < len
                && SUR_LO_MIN <= (d = value.charAt(i + 1))
                && d <= SUR_LO_MAX) {
                // Convert surrogate.
                i++;
                if (MAX_CODE < (c = ((c & 0x3FF) << 10) + (d & 0x3FF))) {
                    throw new IllegalArgumentException(
                        "invalid composite char[" + i + "]:" + c);
                }
            }
            v[pos++] = c;
        }
        if (pos < len) {
            // Compacted surrogates.
            int[] copy = new int[pos];
            while (pos-- > 0) {
                copy[pos] = v[pos];
            }
            v = copy;
        }
        mValue = v;
    }

    /**
     * Gets the code value at the given offset.
     *
     * @param index  the offset
     * @return the value, in range 0 through MAX_CODE
     */
    public int charAt (int index) {
        if (index < 0 || mValue.length <= index) {
            throw new IndexOutOfBoundsException("index " + index
                + " not in range 0..." + (mValue.length - 1));
        }
        return mValue[index];
    }

    public int length () {
        return mValue.length;
    }

    /**
     * Concatenates the specified string to the end of this string.
     * @param cord Cord object.
     * @return concatenated string.
     */
    public Cord concat (Cord cord) {
        if (cord == null || cord.mValue.length == 0) {
            return this;
        }
        if (mValue.length == 0) {
            return cord;
        }
        int[] value = new int[mValue.length + cord.mValue.length];
        System.arraycopy(mValue, 0, value, 0, mValue.length);
        System.arraycopy(cord.mValue, 0, value, mValue.length,
            cord.mValue.length);
        return new Cord(value);
    }

    /**
     * Returns a Cord that represents the character sequence in the array.
     *
     * @param data  the data to copy, or null
     * @return the new Cord, or null for null data
     */
    public static Cord copyValueOf (int[] data) {
        return copyValueOf(data, 0, (data == null ? 0 : data.length));
    }

    /**
     * Returns a Cord that represents the character sequence in the sub-array.
     *
     * @param data  the data to copy, or null
     * @return the new Cord, or null for null data
     */
    public static Cord copyValueOf (int[] data, int offset, int count) {
        if (offset < 0) {
            throw new IllegalArgumentException("negative offset: " + offset);
        }
        if (count < 0) {
            throw new IllegalArgumentException("negative count: " + count);
        }
        if (data == null) {
            return null;
        }
        int[] value = new int[count];
        System.arraycopy(data, offset, value, 0, count);
        return new Cord(value);
    }

    /**
     * Checks if this ends with the given suffix.
     *
     * @param suffix  string to test for
     * @return true if suffix, else false
     */
    public boolean endsWith (Cord suffix) {
        if (suffix == null) {
            return false;
        }
        int off = mValue.length - suffix.mValue.length;
        if (off < 0) {
            return false;
        }
        for (int i = suffix.mValue.length; i-- > 0;) {
            if (suffix.mValue[i] != mValue[off + i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Compares for equality of character sequence.
     *
     * @param cord  another coder, or null
     * @return true if same code sequence, false if different or null
     */
    public boolean equals (Cord cord) {
        if (cord == null || cord.length() != mValue.length) {
            return false;
        }
        for (int i = 0; i < mValue.length; i++) {
            if (mValue[i] != cord.mValue[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns the index of the first occurrence of the given code point,
     * starting at the given offset.
     *
     * @param c  the code point to find
     * @param from  the offset
     * @return the offset where found, or -1 if not found
     */
    public int indexOf (int c, int from) {
        if (from < 0) {
            from = 0;
        }
        for (; from < mValue.length; from++) {
            if (mValue[from] == c) {
                return from;
            }
        }
        return -1;
    }

    /**
     * Returns the index of the first occurrence of the given code point.
     *
     * @param c  the code point to find
     * @return the offset where found, or -1 if not found
     */
    public int indexOf (int c) {
        return indexOf(c, 0);
    }

    /**
     * Returns the index of the last occurrence of the given code point,
     * starting at the given offset.
     *
     * @param c  the code point to find
     * @param from  the offset
     * @return the offset where found, or -1 if not found
     */
    public int lastIndexOf (int c, int from) {
        if (from >= mValue.length) {
            from = mValue.length - 1;
        }
        for (; from >= 0; from--) {
            if (mValue[from] == c) {
                return from;
            }
        }
        return -1;
    }

    /**
     * Returns the index of the last occurrence of the given code point.
     *
     * @param c  the code point to find
     * @return the offset where found, or -1 if not found
     */
    public int lastIndexOf (int c) {
        return lastIndexOf(c, mValue.length - 1);
    }

    /**
     * Checks if this starts with the given prefix.
     *
     * @param prefix  string to test for
     * @return true if prefix, else false
     */
    public boolean startsWith (Cord prefix) {
        if (prefix == null || mValue.length < prefix.mValue.length) {
            return false;
        }
        for (int i = prefix.mValue.length; i-- > 0;) {
            if (prefix.mValue[i] != mValue[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Converts to new string object, escaping code points over U+FFFF
     * as pairs of surrogate chars.
     *
     * @return a string value
     */
    public String toString () {
        int size = mValue.length, c;
        for (int i = 0; i < mValue.length; i++) {
            if (MAX_CHAR < mValue[i]) {
                size++;
            }
        }
        char[] s = new char[size];
        for (int i = size = 0; i < mValue.length; i++) {
            if (MAX_CHAR < (c = mValue[i])) {
                s[size++] = (char) (SUR_HI_MIN + (c >> 10));
                s[size++] = (char) (SUR_LO_MIN + (c & 0x3FF));
            } else {
                s[size++] = (char) c;
            }
        }
        return new String(s);
    }
}
