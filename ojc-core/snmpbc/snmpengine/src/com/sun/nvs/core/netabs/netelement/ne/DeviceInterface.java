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
public interface DeviceInterface extends EntityIf {
    /**
     * DOCUMENT ME!
     */
    public static final int STATUS_UP = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int STATUS_DOWN = 2;

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getIfIndex();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public MacAddress getMacAddress();

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
    public InterfaceCapability getCapability();

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
    public IpAddress getNetworkMask();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getDevice();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getAdminStatus();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getOperStatus();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getTypeStr();
}
