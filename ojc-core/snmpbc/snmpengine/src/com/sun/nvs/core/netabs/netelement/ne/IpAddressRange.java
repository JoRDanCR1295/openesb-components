/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.netabs.netelement.ne.IpAddress;

import java.util.Comparator;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class IpAddressRange {
    private IpAddress ip1;
    private IpAddress ip2;
    private long ip1Code;
    private long ip2Code;
    private int index;
    private String setName = null;
    private boolean autoManage = false;

    /**
     * Creates a new IpAddressRange object.
     *
     * @param ip1 DOCUMENT ME!
     * @param ip2 DOCUMENT ME!
     */
    public IpAddressRange(IpAddress ip1, IpAddress ip2) {
        setIpRange(ip1, ip2);
    }

    /**
     * Creates a new IpAddressRange object.
     *
     * @param low DOCUMENT ME!
     * @param high DOCUMENT ME!
     */
    public IpAddressRange(long low, long high) {
        this(new IpAddress(low), new IpAddress(high));
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip1 DOCUMENT ME!
     * @param ip2 DOCUMENT ME!
     */
    public void setIpRange(IpAddress ip1, IpAddress ip2) {
        if (isValidRange(ip1, ip2)) {
            this.ip1 = ip1;
            this.ip2 = ip2;
            ip1Code = ip1.toLong();
            ip2Code = ip2.toLong();

            return;
        }

        throw new IllegalArgumentException("Invalid IP range");
    }

    /**
     * DOCUMENT ME!
     *
     * @param r DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddressRange union(IpAddressRange r) {
        if (isAdjacent(r) || intersects(r)) {
            long low = Math.min(ip1Code, r.ip1Code);
            long high = Math.max(ip2Code, r.ip2Code);

            return new IpAddressRange(low, high);
        }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param r2 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean intersects(IpAddressRange r2) {
        return (intersect(r2) != null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param r2 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddressRange intersect(IpAddressRange r2) {
        long low = Math.max(ip1Code, r2.ip1Code);
        long high = Math.min(ip2Code, r2.ip2Code);

        if (high < low) {
            return null;
        }

        return new IpAddressRange(low, high);
    }

    /**
     * DOCUMENT ME!
     *
     * @param exr DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddressRange[] excludeRange(IpAddressRange exr) {
        IpAddressRange r1 = null;
        IpAddressRange r2 = null;

        if (!intersects(exr)) {
            return null;
        }

        long high1 = exr.ip1Code - 1;

        if (high1 >= ip1Code) {
            r1 = new IpAddressRange(ip1Code, high1);
        }

        long low2 = exr.ip2Code + 1;

        if (low2 <= ip2Code) {
            r2 = new IpAddressRange(low2, ip2Code);
        }

        if ((r1 == null) && (r2 == null)) {
            return new IpAddressRange[0];
        }

        if ((r1 != null) && (r2 != null)) {
            IpAddressRange[] result = new IpAddressRange[2];
            result[0] = r1;
            result[1] = r2;

            return result;
        }

        if (r1 != null) {
            IpAddressRange[] result = new IpAddressRange[1];
            result[0] = r1;

            return result;
        } else {
            IpAddressRange[] result = new IpAddressRange[1];
            result[0] = r2;

            return result;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param r DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isAdjacent(IpAddressRange r) {
        if (
            (ip2Code == r.ip1Code) || (ip1Code == r.ip2Code) || (ip2Code == (r.ip1Code - 1)) ||
                (ip1Code == (r.ip2Code + 1))
        ) {
            return true;
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getLowValue() {
        return this.ip1;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getHighValue() {
        return this.ip2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip1 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isInRange(IpAddress ip1) {
        long newIp = ip1.toLong();

        if ((newIp >= ip1Code) && (newIp <= ip2Code)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip1 DOCUMENT ME!
     * @param ip2 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isValidRange(IpAddress ip1, IpAddress ip2) {
        if (ip1.toLong() <= ip2.toLong()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param o DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object o) {
        if (o instanceof IpAddressRange) {
            IpAddressRange r = (IpAddressRange) o;

            return (r.index == index);
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ind DOCUMENT ME!
     */
    public void setIndex(int ind) {
        this.index = ind;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getIndex() {
        return index;
    }

    /**
     * DOCUMENT ME!
     *
     * @param setName DOCUMENT ME!
     */
    public void setSetName(String setName) {
        this.setName = setName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSetName() {
        return setName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isManageable() {
        return autoManage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param autoManage DOCUMENT ME!
     */
    public void setManageable(boolean autoManage) {
        this.autoManage = autoManage;
    }
}
