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
 * @(#)MSMQXIDImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.transaction;

import java.io.Serializable;

import java.net.InetAddress;
import java.net.ServerSocket;

import javax.transaction.xa.Xid;

/**
 * Xid implementation for MSMQ BC
 *
 * @author Sun Microsystems
 */
public final class MSMQXIDImpl implements Xid, Serializable {
    static final long serialVersionUID = 3256727264572813369L;

    static byte[] ipAddress;

    static long counter = 0;

    static ServerSocket sock;
    static {
        try {
            // use local host address + bound ip port as unique
            // host/process identifier
            byte[] hostAddress = InetAddress.getLocalHost().getAddress();
            ipAddress = new byte[hostAddress.length + 2];
            System.arraycopy(hostAddress, 0, ipAddress, 0, hostAddress.length);
            sock = new ServerSocket(0);
            int port = sock.getLocalPort();
            ipAddress[ipAddress.length - 2] = (byte) ((port >> 8) & 0xF);
            ipAddress[ipAddress.length - 1] = (byte) (port & 0xF);
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }

    private int formatId = 987654;

    private byte[] branchQualifier;

    private byte[] globalTransactionId;

    static synchronized long incrCounter() {
        return ++counter;
    }

    /**
     * 
     */
    public MSMQXIDImpl() {
        branchQualifier = new byte[8 + ipAddress.length];
        int i;
        for (i = 0; i < ipAddress.length; i++) {
            branchQualifier[i] = ipAddress[i];
        }
        // add system time and counter to produce global unique id
        long count = incrCounter();
        long uid = (System.currentTimeMillis() << 12) + (count & 0xFFF);
        for (int j = 0; j < 8; j++, i++) {
            branchQualifier[i] = (byte) uid;
            uid >>= 8;
        }
        globalTransactionId = branchQualifier;
    }

    /**
     * Getter for GlobalTransactionId attribute of the MSMQXIDImpl
     * object
     *
     * @return xid
     */
    public byte[] getGlobalTransactionId() {
        return globalTransactionId;
    }

    /**
     * Setter for GlobalTransactionId attribute of the MSMQXIDImpl
     * object
     *
     * @param value global transaction id
     */
    public void setGlobalTransactionId(byte[] value) {
        globalTransactionId = value;
    }

    /**
     * Getter for BranchQualifier attribute of the MSMQXIDImpl
     * object
     *
     * @return binary xid
     */
    public byte[] getBranchQualifier() {
        return branchQualifier;
    }

    /**
     * Setter for BranchQualifier attribute of the MSMQXIDImpl
     * object
     *
     * @param value branch qualifier
     */
    public void setBranchQualifier(byte[] value) {
        branchQualifier = value;
    }

    /**
     * Getter for FormatId attribute of the MSMQXIDImpl object
     *
     * @return format id
     */
    public int getFormatId() {
        return formatId;
    }

    /**
     * Setter for FormatId attribute of the MSMQXIDImpl object
     *
     * @param value format id
     */
    public void setFormatId(int value) {
        formatId = value;
    }

    /**
     *
     * @return hash code
     */
    public int hashCode() {
        int result = 0;
        for (int i = 0; i < branchQualifier.length; i++) {
            result += (result << 3) + branchQualifier[i];
        }
        for (int i = 0; i < globalTransactionId.length; i++) {
            result += (result << 3) + globalTransactionId[i];
        }
        return result;
    }

    /**
     * @param that other object to compare to
     * @return true if objects are equal
     */
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (!(that instanceof MSMQXIDImpl)) {
            return false;
        }
        MSMQXIDImpl thatXid = (MSMQXIDImpl) that;
        for (int i = 0; i < branchQualifier.length; i++) {
            if (branchQualifier[i] != thatXid.branchQualifier[i]) {
                return false;
            }
        }
        for (int i = 0; i < globalTransactionId.length; i++) {
            if (globalTransactionId[i] != thatXid.globalTransactionId[i]) {
                return false;
            }
        }
        return true;
    }

    static final char hexChar(int c) {
        final String hex = "0123456789ABCDEF";
        return hex.charAt(c & 0xF);
    }

    /**
     *
     * @return pretty print string of this object
     */
    public String toString() {
        StringBuffer result = new StringBuffer();
        result.append("xid:");

        result.append(this.formatId);
        // format id
        result.append(":");

        for (int i = 0; i < globalTransactionId.length; i++) {
            byte b = globalTransactionId[i];
            result.append(hexChar(b >> 4));
            result.append(hexChar(b));
        }
        result.append(":");
        for (int i = 0; i < branchQualifier.length; i++) {
            byte b = branchQualifier[i];
            result.append(hexChar(b >> 4));
            result.append(hexChar(b));
        }
        return result.toString();
    }
}
