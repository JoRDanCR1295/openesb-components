/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.netabs.devicedata.Obj2DataRecordIf;
import com.sun.nvs.core.netabs.netelement.image.SoftwareImage;

import java.net.InetAddress;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface Device extends GroupMemberIf, Obj2DataRecordIf {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getId();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getLastInventoried();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getNameShort();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isContainer();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getIpAddress();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public InetAddress getInetAddress();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getVendorName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getProductFamily();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getTypeStr();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getVendorId();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSysObjectID();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSysDescr();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceCapability getDeviceCapability();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getOSName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getVersion();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isManaged();

    /**
     * DOCUMENT ME!
     *
     * @param manag DOCUMENT ME!
     */
    public void setIsManaged(boolean manag);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isOffline();

    /**
     * DOCUMENT ME!
     *
     * @param offlineStatus DOCUMENT ME!
     */
    public void setIsOffline(boolean offlineStatus);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getUniqueId();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceInterface[] getAllInterfaces();

    /**
     * DOCUMENT ME!
     *
     * @param macAddress DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceInterface[] getInterfaceByMac(MacAddress macAddress);

    /**
     * DOCUMENT ME!
     *
     * @param ifIndex DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceInterface getInterfaceByIfIndex(int ifIndex);

    /**
     * DOCUMENT ME!
     *
     * @param intfIp DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceInterface getInterfaceByIpAddress(IpAddress intfIp);

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceInterface getInterfaceByName(String name);

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasIpAddress(IpAddress ip);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceFactoryIf getFactory();

    /**
     * DOCUMENT ME!
     *
     * @param image DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean acceptsSoftwareImage(SoftwareImage image);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getStartupConfigLocation();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getRunningConfigLocation();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getFileSystems();

    /**
     * DOCUMENT ME!
     *
     * @param memName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceFlashMemory[] getFlashMemoryByName(String memName);

    /**
     * DOCUMENT ME!
     *
     * @param memName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceMemory getMemoryByName(String memName);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceMemory[] getAllMemoryObjs();
}
