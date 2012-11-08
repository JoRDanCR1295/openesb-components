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
public abstract class AbstractDeviceInterface implements DeviceInterface {
    /**
     * DOCUMENT ME!
     */
    protected int ifIndex = -1;

    /**
     * DOCUMENT ME!
     */
    protected String name = null;

    /**
     * DOCUMENT ME!
     */
    protected MacAddress macAddr;

    /**
     * DOCUMENT ME!
     */
    protected IpAddress ip = null;

    /**
     * DOCUMENT ME!
     */
    protected IpAddress netMask = null;

    /**
     * DOCUMENT ME!
     */
    protected Device device = null;

    /**
     * DOCUMENT ME!
     */
    protected int adminStatus = STATUS_DOWN;

    /**
     * DOCUMENT ME!
     */
    protected int operStatus = STATUS_DOWN;

    /**
     * DOCUMENT ME!
     */
    protected String typeStr = null;

    /**
     * DOCUMENT ME!
     */
    protected EntityId entId = null;

    /**
     * Creates a new AbstractDeviceInterface object.
     */
    protected AbstractDeviceInterface() {
    }

    /**
     * Creates a new AbstractDeviceInterface object.
     *
     * @param dev DOCUMENT ME!
     * @param ifidx DOCUMENT ME!
     */
    protected AbstractDeviceInterface(Device dev, int ifidx) {
        device = dev;
        ifIndex = ifidx;
    }

    /**
     * DOCUMENT ME!
     *
     * @param idx DOCUMENT ME!
     */
    public void setIfIndex(int idx) {
        ifIndex = idx;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getIfIndex() {
        return ifIndex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ma DOCUMENT ME!
     */
    public void setMacAddress(MacAddress ma) {
        macAddr = ma;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public MacAddress getMacAddress() {
        return macAddr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param n DOCUMENT ME!
     */
    public void setName(String n) {
        name = n;
        updateEntId();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName() {
        return name;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ipaddr DOCUMENT ME!
     */
    public void setIpAddress(IpAddress ipaddr) {
        ip = ipaddr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getIpAddress() {
        return ip;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mask DOCUMENT ME!
     */
    public void setNetworkMask(IpAddress mask) {
        netMask = mask;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getNetworkMask() {
        return netMask;
    }

    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     */
    public void setDevice(Device d) {
        device = d;
        updateEntId();
    }

    /**
     * DOCUMENT ME!
     */
    protected void updateEntId() {
        if ((device != null) && (name != null)) {
            entId = new EntityId(device.getEntityId(), name);
        } else {
            entId = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EntityId getEntityId() {
        return entId;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getDevice() {
        return device;
    }

    /**
     * DOCUMENT ME!
     *
     * @param st DOCUMENT ME!
     */
    public void setAdminStatus(int st) {
        adminStatus = st;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getAdminStatus() {
        return adminStatus;
    }

    /**
     * DOCUMENT ME!
     *
     * @param st DOCUMENT ME!
     */
    public void setOperStatus(int st) {
        operStatus = st;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getOperStatus() {
        return operStatus;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     */
    public void setTypeStr(String s) {
        typeStr = s;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getTypeStr() {
        return typeStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return name;
    }
}
