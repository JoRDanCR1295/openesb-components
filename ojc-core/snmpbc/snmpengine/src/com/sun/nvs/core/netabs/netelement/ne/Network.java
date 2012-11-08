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
public interface Network extends EntityIf {
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
    public Device[] getAllDevices();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllManagedDevices();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllUnManagedDevices();

    /**
     * DOCUMENT ME!
     *
     * @param id DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getDeviceById(int id);

    /**
     * DOCUMENT ME!
     *
     * @param mac DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getByMacAddress(MacAddress mac);

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getByIpAddress(IpAddress ip);

    /**
     * DOCUMENT ME!
     *
     * @param rule DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getDevices(GroupingRule rule);
}
