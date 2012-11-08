/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.util.ParseUtils;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class MacAddress implements java.io.Serializable {
    // byte0 is MSB
    // Byte 5 is LSB
    private byte[] octets = new byte[6];

    private MacAddress() {
    }

    /**
     * Creates a new MacAddress object.
     *
     * @param data DOCUMENT ME!
     */
    public MacAddress(byte[] data) {
        if ((data == null) || (data.length != 6)) {
            throw new IllegalArgumentException("Invalid argument for MacAddress constructor");
        }

        System.arraycopy(data, 0, octets, 0, data.length);
    }

    /**
     * Creates a new MacAddress object.
     *
     * @param data DOCUMENT ME!
     */
    public MacAddress(char[] data) {
        if ((data == null) || (data.length != 6)) {
            throw new IllegalArgumentException("Invalid argument for MacAddress constructor");
        }

        octets[0] = (byte) (data[0] & 0xFF);
        octets[1] = (byte) (data[1] & 0xFF);
        octets[2] = (byte) (data[2] & 0xFF);
        octets[3] = (byte) (data[3] & 0xFF);
        octets[4] = (byte) (data[4] & 0xFF);
        octets[5] = (byte) (data[5] & 0xFF);

        //System.arraycopy(data, 0, octets, 0, data.length);
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
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws NumberFormatException DOCUMENT ME!
     */
    public static MacAddress getFromDottedFormat(String s)
        throws NumberFormatException {
        String[] tokens = ParseUtils.split(s, ".", true);

        if (tokens.length != 5) {
            throw new NumberFormatException("Invalid mac address format");
        }

        if ((!tokens[1].equals(".")) || (!tokens[3].equals("."))) {
            throw new NumberFormatException("Invalid mac address format");
        }

        if ((tokens[0].length() > 4) || (tokens[2].length() > 4) || (tokens[4].length() > 4)) {
            throw new NumberFormatException("Invalid mac address format");
        }

        MacAddress mac = new MacAddress();

        int a = Integer.parseInt(tokens[0], 16);
        int b = Integer.parseInt(tokens[2], 16);
        int c = Integer.parseInt(tokens[4], 16);

        mac.octets[0] = (byte) ((a >> 8) & 0xFF);
        mac.octets[1] = (byte) (a & 0xFF);

        mac.octets[2] = (byte) ((b >> 8) & 0xFF);
        mac.octets[3] = (byte) (b & 0xFF);

        mac.octets[4] = (byte) ((c >> 8) & 0xFF);
        mac.octets[5] = (byte) (c & 0xFF);

        return mac;
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
    public static MacAddress getFromHexFormat(String s)
        throws NumberFormatException {
        if (s.length() != 12) {
            throw new NumberFormatException("invalid mac address format");
        }

        MacAddress mac = new MacAddress();

        for (int i = 0; i < 6; i++) {
            String x = s.substring(2 * i, (2 * i) + 2);
            mac.octets[i] = (byte) (Integer.parseInt(x, 16) & 0xFF);
        }

        return mac;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param delim DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws NumberFormatException DOCUMENT ME!
     */
    public static MacAddress getFromHexFormat(String s, char delim)
        throws NumberFormatException {
        String d = "" + delim;

        String[] tokens = ParseUtils.split(s, d, true);

        if (tokens.length != 11) {
            throw new NumberFormatException("invalid mac address format");
        }

        for (int i = 1; i < 11; i = i + 2) {
            if (!tokens[i].equals(d)) {
                throw new NumberFormatException("invalid mac address format");
            }
        }

        for (int i = 0; i < 12; i = i + 2) {
            if (tokens[i].length() > 2) {
                throw new NumberFormatException("invalid mac address format");
            }
        }

        MacAddress mac = new MacAddress();

        for (int i = 0; i < 6; i++) {
            String x = tokens[i * 2];
            mac.octets[i] = (byte) (Integer.parseInt(x, 16) & 0xFF);
        }

        return mac;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toDottedString() {
        StringBuffer sb = new StringBuffer();

        sb.append(
            fixLength(Integer.toHexString(((octets[0] & 0xFF) << 8) | (octets[1] & 0xFF)), 4)
        );
        sb.append(".");
        sb.append(
            fixLength(Integer.toHexString(((octets[2] & 0xFF) << 8) | (octets[3] & 0xFF)), 4)
        );
        sb.append(".");
        sb.append(
            fixLength(Integer.toHexString(((octets[4] & 0xFF) << 8) | (octets[5] & 0xFF)), 4)
        );

        return sb.toString();
    }

    private String fixLength(String s, int len) {
        while (s.length() < len)
            s = "0" + s;

        return s;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toHexString() {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < octets.length; i++) {
            sb.append(fixLength(Integer.toHexString(octets[i] & 0xFF), 2));
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param delim DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toHexString(char delim) {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < octets.length; i++) {
            if (i != 0) {
                sb.append(delim);
            }

            sb.append(fixLength(Integer.toHexString(octets[i] & 0xFF), 2));
        }

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
     * @param mac DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object mac) {
        if (mac == this) {
            return true;
        }

        if (mac instanceof MacAddress) {
            MacAddress m = (MacAddress) mac;

            for (int i = 0; i < octets.length; i++) {
                if (octets[i] != m.octets[i]) {
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
        return (((octets[0] | octets[1] | octets[2]) << 24) + (octets[3] << 16) + (octets[4] << 8) +
        octets[5]);
    }

    /**
     * DOCUMENT ME!
     *
     * @param args DOCUMENT ME!
     */
    public static void main(String[] args) {
        try {
            MacAddress m1 = MacAddress.getFromDottedFormat("11.BB22.0033");
            System.out.println("m1 = " + m1);
            System.out.println("m1 = " + m1.toHexString());
            System.out.println("m1 = " + m1.toHexString('-'));
            System.out.println("");

            m1 = MacAddress.getFromHexFormat("0011223344AA");
            System.out.println("m1 = " + m1);
            System.out.println("m1 = " + m1.toHexString());
            System.out.println("m1 = " + m1.toHexString('-'));
            System.out.println("");

            m1 = MacAddress.getFromHexFormat("00:11:22:33:44:BB", ':');
            System.out.println("m1 = " + m1);
            System.out.println("m1 = " + m1.toHexString());
            System.out.println("m1 = " + m1.toHexString(':'));
            System.out.println("");

            MacAddress m2 = MacAddress.getFromDottedFormat(m1.toDottedString());
            System.out.println("m2 = " + m2.toString());
            System.out.println("m2.equals(m1) = " + m2.equals(m1));
            System.out.println("m2 hashcode = " + m2.hashCode());
            System.out.println("m1 hashcode = " + m1.hashCode());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
