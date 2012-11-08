/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import java.net.InetAddress;

import java.util.Vector;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface DeviceGroup extends GroupMemberIf {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getImmediateLeafMembers();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllLeafMembers();

    /**
     * DOCUMENT ME!
     *
     * @param network DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllLeafMembers(Network network);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceGroup[] getChildGroups();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isDynamic();

    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isMember(Device d);

    /**
     * DOCUMENT ME!
     *
     * @param g DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean containsGroup(DeviceGroup g);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isSystemGroup();
}
