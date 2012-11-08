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
 * @(#)TrapID.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

import com.sun.management.snmp.SnmpPdu;

import java.net.InetAddress;
import java.util.StringTokenizer;

/**
 * Identifies a trap
 * 
 * @author fkieviet
 */
public class TrapID {
    private InetAddress mAddress;
    private int mPort;

    /**
     * Constructor
     * 
     * @param address ip adress
     * @param port port
     */
    public TrapID(InetAddress address, int port) {
        mAddress = address;
        mPort = port;
    }
    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((mAddress == null) ? 0 : mAddress.hashCode());
        result = prime * result + mPort;
        return result;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final TrapID other = (TrapID) obj;
        if (mAddress == null) {
            if (other.mAddress != null) {
                return false;
            }
        } else if (!mAddress.equals(other.mAddress)) {
            return false;
        }
        if (mPort != other.mPort) {
            return false;
        }
        return true;
    }

    /**
     * @param p pdu
     * @return a new TrapID that can be used to identify the trap
     */
    public static TrapID createID(SnmpPdu p) {
        return new TrapID(p.address, p.port);
    }
    
    /**
     * Unserializes a TrapID from a string
     * 
     * @param replyID serialized TrapID
     * @return TrapID
     * @throws Exception on failure
     */
    public static TrapID unserializeTrapIDFromString(String replyID) throws Exception {
        try {
            // Split on colon
            int colon = replyID.indexOf(':');

            // Parse ipaddress
            String addrstr = replyID.substring(0, colon);
            byte[] buf = new byte[4];
            int nbytes = 0;
            for (StringTokenizer s = new StringTokenizer(addrstr, "."); s.hasMoreTokens();) {
                if (nbytes == buf.length) {
                    byte[] buf2 = new byte[nbytes + 1];
                    System.arraycopy(buf, 0, buf2, 0, nbytes);
                    buf = buf2;
                }
                buf[nbytes++] = Byte.parseByte(s.nextToken());
            }

            // Assemble
            String port = replyID.substring(colon + 1);
            InetAddress inetaddr = InetAddress.getByAddress(buf);
            return new TrapID(inetaddr, Integer.parseInt(port));
        } catch (Exception e) {
            throw new Exception("Could not unserialize TrapID from string [" + replyID + "]: " + e, e);
        }
    }

    /**
     * Serializes the trapid to a string so that it can be recreated
     * 
     * @param id id to serialize
     * @return serialized id
     */
    public static String serializeTrapIDToString(TrapID id) {
        byte[] addr = id.mAddress.getAddress();
        StringBuilder buf = new StringBuilder();
        for (byte b : addr) {
            if (buf.length() != 0) {
                buf.append('.');
            }
            buf.append(b);
        }
        
        buf.append(':').append(id.mPort);
        
        return buf.toString();
    }
    /**
     * Getter for mAddress
     *
     * @return InetAddress
     */
    public InetAddress getAddress() {
        return mAddress;
    }
    /**
     * Getter for mPort
     *
     * @return int
     */
    public int getPort() {
        return mPort;
    }

}
