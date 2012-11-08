/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.util.ParseUtils;

import java.net.InetAddress;
import java.net.UnknownHostException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class IpAddress implements java.io.Serializable {
    // byte0 is MSB
    // Byte 3 is LSB
    private byte[] octets = new byte[4];

    private IpAddress() {
    }

    /**
     * Creates a new IpAddress object.
     *
     * @param ipvalue DOCUMENT ME!
     */
    public IpAddress(int ipvalue) {
        octets[0] = (byte) ((ipvalue >> 24) & 0xFF);
        octets[1] = (byte) ((ipvalue >> 16) & 0xFF);
        octets[2] = (byte) ((ipvalue >> 8) & 0xFF);
        octets[3] = (byte) ((ipvalue) & 0xFF);
    }

    /**
     * Creates a new IpAddress object.
     *
     * @param ipvalue DOCUMENT ME!
     */
    public IpAddress(long ipvalue) {
        this((int) ipvalue);
    }

    /**
     * Creates a new IpAddress object.
     *
     * @param data DOCUMENT ME!
     */
    public IpAddress(byte[] data) {
        if ((data == null) || (data.length != 4)) {
            throw new IllegalArgumentException("Invalid argument for IpAddress constructor");
        }

        System.arraycopy(data, 0, octets, 0, data.length);
    }

    /**
     * Creates a new IpAddress object.
     *
     * @param inetAddr DOCUMENT ME!
     */
    public IpAddress(InetAddress inetAddr) {
        System.arraycopy(inetAddr.getAddress(), 0, octets, 0, octets.length);
    }

    /**
     * DOCUMENT ME!
     *
     * @param out DOCUMENT ME!
     */
    public void copyOctetsTo(byte[] out) {
        System.arraycopy(octets, 0, out, 0, octets.length);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws UnknownHostException DOCUMENT ME!
     */
    public InetAddress getInetAddress() throws UnknownHostException {
        return InetAddress.getByAddress(octets);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isValidNetworkMask() {
        long value = toLong();

        boolean zeroMode = true;

        for (int i = 0; i < 32; i++) {
            if (zeroMode) {
                if ((value & 0x1) == 1) {
                    zeroMode = false;
                }
            } else {
                if ((value & 0x1) == 0) {
                    return false;
                }
            }

            value = value >> 1;
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int countTrailingZeroBits() {
        long value = toLong();

        int count = 0;

        for (int i = 0; i < 32; i++) {
            if ((value & 0x1) == 1) {
                return count;
            }

            count++;

            value = value >> 1;
        }

        return count;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getNetworkPartLength() {
        return (32 - countTrailingZeroBits());
    }

    /**
     * DOCUMENT ME!
     *
     * @param hostAddress DOCUMENT ME!
     * @param netmask DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    public static IpAddress getNetworkAddress(IpAddress hostAddress, IpAddress netmask)
        throws IllegalArgumentException {
        if (!netmask.isValidNetworkMask()) {
            throw new IllegalArgumentException("Invalid NETWORK MASK");
        }

        return new IpAddress(hostAddress.toLong() & netmask.toLong());
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws NumberFormatException DOCUMENT ME!
     */
    public static IpAddress getFromDottedFormat(String s)
        throws NumberFormatException {
        String[] tokens = ParseUtils.split(s, ".", true);

        if (tokens.length != 7) {
            throw new NumberFormatException("Invalid ip address format:" + s);
        }

        if ((!tokens[1].equals(".")) || (!tokens[3].equals(".")) || (!tokens[5].equals("."))) {
            throw new NumberFormatException("Invalid ip address format" + s);
        }

        if (
            (tokens[0].length() > 3) || (tokens[2].length() > 3) || (tokens[4].length() > 3) ||
                (tokens[5].length() > 4)
        ) {
            throw new NumberFormatException("Invalid ip address format" + s);
        }

        IpAddress ip = new IpAddress();

        ip.octets[0] = (byte) Integer.parseInt(tokens[0]);
        ip.octets[1] = (byte) Integer.parseInt(tokens[2]);
        ip.octets[2] = (byte) Integer.parseInt(tokens[4]);
        ip.octets[3] = (byte) Integer.parseInt(tokens[6]);

        return ip;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toDottedString() {
        StringBuffer sb = new StringBuffer();

        sb.append((octets[0] & 0xFF));
        sb.append(".");
        sb.append((octets[1] & 0xFF));
        sb.append(".");
        sb.append((octets[2] & 0xFF));
        sb.append(".");
        sb.append((octets[3] & 0xFF));

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return toDottedString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param o DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }

        if (o instanceof IpAddress) {
            IpAddress ip = (IpAddress) o;

            for (int i = 0; i < octets.length; i++) {
                if (octets[i] != ip.octets[i]) {
                    return false;
                }
            }

            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int hashCode() {
        return (int) toLong();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long toLong() {
        long v = (((octets[0] << 24) & 0xFF000000) | ((octets[1] << 16) & 0xFF0000) |
            ((octets[2] << 8) & 0xFF00) | ((octets[3]) & 0xFF));

        return (v & 0xFFFFFFFFL);
    }
}
