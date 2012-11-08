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
 * @(#)StringCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

/**
 * Interface for objects that can encode String values as byte-arrays,
 * and decode byte-arrays to String values. This is not meant for the
 * encoding and decoding of OtdInputStream and OtdOutputStream as a
 * whole, because streaming encoding can be stateful.
 *
 * Inspired by eGate 4.5.3 Java/com/stc/jcsre/IStringCoder.java.
 *
 * @author Michael Libourel
 * @version
 */
public interface StringCoder {

    /**
     * If a coder has this property set to non-false, then the factory
     * will assume the coder instances are thread-safe and good to share.
     */
    public static final String SHARE = "share";

    /**
     * Converts a string into a byte array.
     *
     * @param s  a string, or null
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    byte[] encode(String s)
        throws CoderException;

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
    byte[] encode(String s, int min, int max)
        throws CoderException;

    /**
     * Converts a byte array into a string.
     *
     * @param b  null or a byte array containing the encoded string
     * @return the decoded string
     * @throws CoderException for uninterpretable byte sequences
     */
    String decode(byte[] b)
        throws CoderException;

    /**
     * Converts a byte array slice into a string.
     *
     * @param b  null or a byte array containing the encoded string
     * @param from  index of first byte
     * @param length  length of sub-array to convert
     * @return the decoded string
     * @throws CoderException for uninterpretable byte sequences
     */
    String decode(byte[] b, int from, int length)
        throws CoderException;

    /**
     * Sets the value of the named property.
     *
     * @param name  name of property
     * @param value  new value of property
     * @throws IllegalArgumentException for unknown property or wrong value
     */
    void setProperty(String name, Object value);

    /**
     * Retrieves the value of the named property.
     *
     * @param name  name of property
     * @return value of property
     * @throws IllegalArgumentException if property not supported by encoder
     */
    Object getProperty(String name);
}
