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
 * @(#)UUIDGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.common;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * This class generates unique ids. Currently it only generates Network-Unique
 * IDs (NUIDs).  NUIDs are guaranteed to be unique within a network - where IP
 * addresses must be unique. The reason it is only unique within a network and
 * not globally is because different networks behind different firewalls can
 * have machines with the same IP address. A NUID is basically a byte array.
 * The bytes in the array are arranged so that the most often changing portion
 * occurrs first. That way any comparisons done will detect a difference as
 * soon as possible. The NUID uses the following format: IIIISSSSSSSMMMMV I -
 * an integer that is incremented every time a new NUID is generated the
 * integer is placed into the byte array LSB first so that changes occur as
 * early in the byte. S - a segment identifier.  This is initially set to the
 * least significant 7-bytes returned by
 * <code>System.currentTimeMillis()</code>.  This gets incremented every time
 * the I portion rolls over. M - the 4-bytes making up the IP address of the
 * machine.  V - the version number of the NUID generator. This means that
 * there are 2^120 unique IDs.
 *
 * @author Graj
 *
 */
public class UUIDGenerator implements Serializable {

    /**
     * DOCUMENT ME!
     */
    private static final int   NUID_LENGTH   = 16;

    /**
     * DOCUMENT ME!
     */
    private static final int[] NUID_SEGMENTS = new int[] {4, 7, 4, 1};

    /**
     * DOCUMENT ME!
     */
    private static byte[] nuid = null;

    private static MessageDigest algorithm = null;

    /**
     * DOCUMENT ME!
     */
    private static final int[] HASHUID_SEGMENTS = new int[] {5, 5, 5, 5};
    
    public static final long serialVersionUID = 1;

    /**
     * Cannot be instantiated.
     */
    private UUIDGenerator() {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * Returns an UID from the given value.
     *
     * @param value string to be converted to id
     * @return the generated UID
     *
     * @throws NullPointerException if the input value is null or empty
     * @throws NoSuchAlgorithmException if the hash algoithm is not found
     */
    public static synchronized String getHashUID(String value) throws NullPointerException, NoSuchAlgorithmException {
        if (value == null || value.equals("")) {
            throw new NullPointerException("Input value to convert is null or empty");
        }
        byte[] digest = null;

        if (algorithm == null) {
            algorithm = MessageDigest.getInstance("SHA-1");
        }
        algorithm.reset();
        algorithm.update(value.getBytes());
        digest = algorithm.digest();
        return toString(digest, HASHUID_SEGMENTS);
    }

    /**
     * Returns a Network-Unique ID (NUID).
     *
     * @return a network unique id
     *
     * @since 1.0
     */
    public static synchronized String getNUID() {
        if (null == UUIDGenerator.nuid) {
            initNUID(UUIDGenerator.NUID_SEGMENTS);
        }


        // don't want to increment version accidentally
        int last = UUIDGenerator.nuid.length - 1;

        // increment bytes with carries
        for (int ii = 0; ii < last; ii++) {
            if (++UUIDGenerator.nuid[ii] != 0) {
                break;
            }
        }


        return toString(UUIDGenerator.nuid,
                        UUIDGenerator.NUID_SEGMENTS);
    }


    /**
     * Initializes the NUID.
     *
     * @param segments the segments
     * @throws IllegalStateException if this operation fails
     *
     * @since 1.0
     */
    private static void initNUID(int[] segments) {
        UUIDGenerator.nuid = new byte[NUID_LENGTH];

        // skip first segment
        int pos = segments[0];

        // initialize segment
        long now = System.currentTimeMillis();

        for (int ii = 0; ii < segments[1]; ii++) {
            UUIDGenerator.nuid[pos++] = (byte) (now & 0xFF);
            now >>>= 8;
        }


        // initialize machineIP segment
        byte[] b = null;

        try {
            b = InetAddress.getLocalHost()
                           .getAddress();
        } catch (UnknownHostException ex) {
            throw new IllegalStateException("Unable to get IP address.");
        }


        for (int ii = 0; ii < b.length; ii++) {
            UUIDGenerator.nuid[pos++] = b[ii];
        }



        // set version
        UUIDGenerator.nuid[pos++] = 0x01;
    }

    /**
     * Returns a string representation of the byte array.
     *
     * @param bytes byte array to convert
     * @param segments how the array is to be segmented
     *
     * @return the string representation
     */
    private static String toString(byte[] bytes,
                                   int[]  segments) {
        final String chars = "0123456789ABCDEF";

        StringBuffer buf = new StringBuffer(
                                   (segments.length + 2 + bytes.length) << 1);

        int pos = 0;

        // Commenting out the curly braces '{' since SeeBeyond's BPEL Service Engine cannot handle them
        // buf.append('{');

        for (int ii = 0; ii < segments.length; ii++) {
            for (int jj = segments[ii]; jj > 0; jj--) {
                byte b = bytes[pos++];

                buf.append(chars.charAt(
                                   (b >> 4) & 0xF));
                buf.append(chars.charAt(
                                   b & 0xF));
            }


            if (ii < (segments.length - 1)) {
                buf.append('-');
            }
        }


        // Commenting out the curly braces '}' since SeeBeyond's BPEL Service Engine cannot handle them
        // buf.append('}');

        return buf.toString();
    }


    public static void main(String[] args) {
    	System.out.println(UUIDGenerator.getNUID());
    }
}
