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
 * @(#)Base64Utils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


/**
 * Implements additional Base64 encoding and decoding methods.
 *
 * @author Sun Microsystems
 * @version 
 */
public class Base64Utils {
    /**
     * Given a byte array, this method will Base64-encode the byte array data and return the
     * encoded data as a String.
     *
     * @param data The byte array data to be encoded.
     *
     * @return The Base64-encoded String data.
     *
     * @throws IOException if there are IO errors.
     */
    public static String byteToBase64String(byte[] data)
        throws IOException {
        ByteArrayInputStream bin = new ByteArrayInputStream(data);
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        Base64.encode(bin, bout);

        return bout.toString();
    }

    /**
     * Given a String that is in Base64 encoded format, this method will decode the data and return
     * the decoded data as a byte array.
     *
     * @param data The Base64-encoded String data to be decoded.
     *
     * @return The decoded data as a byte array.
     *
     * @throws IOException if there are IO errors.
     */
    public static byte[] base64DecodeToByte(String data)
        throws IOException {
        ByteArrayInputStream bin = new ByteArrayInputStream(data.getBytes());
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        Base64.decode(bin, bout);

        return bout.toByteArray();
    }

    /**
     * Decodes a String using MIME Base64 Encryption.
     *
     * @param str Encoded String.
     *
     * @return Decoded String.
     *
     * @throws Exception Thrown if the string cannot be decoded.
     */
    public static String base64Decode(String str) throws Exception {
        return Base64.decode(str);
    }

    /**
     * Decodes a byte array supplied in Base64-encoded format and returns the decoded data as a
     * byte array.
     *
     * <p></p>
     *
     * @param arr The Base64-encoded byte array to be decoded.
     *
     * @return Decoded byte array.
     *
     * @throws Exception Thrown if the string cannot be decoded.
     */
    public static byte[] base64Decode(byte[] arr) throws Exception {
        return Base64.decode(arr);
    }

    /**
     * Encodes a String using MIME Base64 Encryption.
     *
     * @param str String to be encoded.
     *
     * @return Encoded String.
     *
     * @throws Exception Thrown if the string cannot be encoded.
     */
    public static String string2Base64(String str) throws Exception {
        return Base64.encode(str);
    }

    /**
     * Encodes a String using MIME Base64 Encryption.
     *
     * @param arr Byte array to be encoded.
     *
     * @return Encoded byte array.
     *
     * @throws Exception Thrown if the string cannot be encoded.
     */
    public static byte[] string2Base64(byte[] arr) throws Exception {
        return Base64.encode(arr);
    }
}
