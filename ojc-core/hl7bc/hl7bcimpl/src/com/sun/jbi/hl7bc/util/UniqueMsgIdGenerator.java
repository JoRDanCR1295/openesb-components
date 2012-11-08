/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)UniqueMsgIdGenerator.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.util;

import java.util.Enumeration;

import java.net.NetworkInterface;
import java.net.InetAddress;

import java.security.SecureRandom;

/**
 * 
 * This class is used to generate message exchange identifiers.
 * 
 * This implementation uses time-based UUID's as its basis. Since the UUID itself is not
 * really needed (and it's only in J2SE 5.0) we only return a string representation. The
 * String representation is changed from the standard to be simpler to contruct and easier
 * to compare with other timestamp that might be available.
 *
 * @author Sun Microsystems, Inc
 *
 */

public final class UniqueMsgIdGenerator {
    /**
     * Timestamp of last id generated.
     */
    long mLastTimestamp;

    /**
     * Number of ticks within the same millisecond.
     */
    int mCounter;

    /**
     * Random portion that will be created initially or if the clock goes backwards.
     */
    int mSequence;

    /**
     * Random portion of the id that is based on the machine.
     */
    long mNode;

    /**
     * Offset from 15-oct-1582 to 1-jan-1970 for timestamp correction.
     */
    private final static long CLOCK_OFFSET = 0x01b21dd213814000L;

    /**
     * Scale from currentTimeMillis() to 100 ns of timestamps. This is also used
     * to generate a different timestamp if called within the same millisecond.
     */
    private final static int CLOCK_SCALE = 10000;

    /*
     * The random number generator used by this class to create random
     * based UUIDs.
     */
    private SecureRandom mRandom = new SecureRandom();

    /**
     * Generate the next Id.
     *
     * @return  a randomly generated <tt>String</tt>.
     */
    public String nextId() {
        long now = System.currentTimeMillis();
        StringBuilder sb = new StringBuilder(40);
        int counter;
        long node;
        int sequence;

        synchronized (this) {
            node = mNode;
            sequence = mSequence;
            if (mNode == 0 || now < mLastTimestamp) {
                initialize();
                node = mNode;
                sequence = mSequence;
            } else if (now == mLastTimestamp) {
                //
                // If we have generated the limit of UUID's per tick we have to wait.
                //
                if (mCounter == CLOCK_SCALE) {
                    mCounter &= 0xFF;
                    do {
                        try {
                            Thread.sleep(1L);
                        } catch (InterruptedException ie) {
                        }
                        now = System.currentTimeMillis();
                    } while (now == mLastTimestamp);
                }
            } else {
                mCounter &= 0xff;
            }
            mLastTimestamp = now;
            counter = ++mCounter;
        }

        //
        //  Format up the identifier.
        //
        sb.append(node);
        sb.append('-');
        sb.append(sequence);
        sb.append('-');
        sb.append(now * CLOCK_SCALE + CLOCK_OFFSET + counter);
        return (sb.toString());
    }

    private void initialize() {
        //
        //  For added entropy, add seeding material to the RNG. We use our set of network
        //  interface address as input.
        //
        if (mNode == 0) {
            Enumeration eni;

            //
            //  Let the RNG seed itself.
            //
            mRandom.nextBytes(new byte[16]);

            //
            //  Now add the InetAddresses.
            //
            try {
                eni = NetworkInterface.getNetworkInterfaces();
                while (eni.hasMoreElements()) {
                    NetworkInterface ni = (NetworkInterface) eni.nextElement();
                    Enumeration eia = ni.getInetAddresses();
                    while (eia.hasMoreElements()) {
                        InetAddress ia = (InetAddress) eia.nextElement();
                        mRandom.setSeed(ia.getAddress());
                    }
                }
            } catch (java.net.SocketException sEx) {

            }
            mNode = mRandom.nextLong() & 0xffffffffffffL;

        }

        mCounter = (mRandom.nextInt() & 0xff);
        mSequence = (mRandom.nextInt() & 0xffff);
    }
}
