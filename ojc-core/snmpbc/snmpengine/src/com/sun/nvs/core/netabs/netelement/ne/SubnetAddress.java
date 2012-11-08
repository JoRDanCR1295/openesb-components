/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class SubnetAddress implements java.io.Serializable {
    private IpAddress networkAddress;
    private IpAddress networkMask;

    /**
     * Creates a new SubnetAddress object.
     *
     * @param hostPart DOCUMENT ME!
     * @param mask DOCUMENT ME!
     *
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    public SubnetAddress(IpAddress hostPart, IpAddress mask)
        throws IllegalArgumentException {
        this.networkAddress = IpAddress.getNetworkAddress(hostPart, mask);
        this.networkMask = mask;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getNetworkAddress() {
        return networkAddress;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getNetworkMask() {
        return networkMask;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getBroadcastAddress() {
        long mask = networkMask.toLong();
        long mask1 = mask ^ 0xFFFFFFFF;

        return new IpAddress(networkAddress.toLong() | mask1);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getPossibleAddresses() {
        long low = networkAddress.toLong();
        long high = getBroadcastAddress().toLong();

        return (high - low);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getPossibleHostAddresses() {
        long range = getPossibleAddresses();

        if (range <= 2) {
            return 0;
        } else {
            return range - 2;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param host DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isInSubnet(IpAddress host) {
        long net = networkAddress.toLong();
        long hnet = host.toLong() & networkMask.toLong();

        return (net == hnet);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getNetworkPartLength() {
        return networkMask.getNetworkPartLength();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int hashCode() {
        return networkAddress.hashCode();
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

        if (o instanceof SubnetAddress) {
            SubnetAddress other = (SubnetAddress) o;

            if (!other.networkAddress.equals(networkAddress)) {
                return false;
            }

            if (!other.networkMask.equals(networkMask)) {
                return false;
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
    public String toString() {
        return networkAddress + "/" + getNetworkPartLength();
    }
}
