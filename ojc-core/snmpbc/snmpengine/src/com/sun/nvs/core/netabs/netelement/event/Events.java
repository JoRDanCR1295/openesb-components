/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public interface Events {
    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_SOURCE_TYPE_NETOBJECT = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_SOURCE_TYPE_LOCAL = 2;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_SYSLOG = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_TRAP = 3;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_DEVICE_MANAGER = 10;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_INTERNAL = 4;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_SERVER = 5;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_TFTP_SERVER = 200;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_FTP_SERVER = 201;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_BOOT_MANAGER = 202;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_CONFIG_MANAGER = 203;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_CLUSTER_MANAGER = 204;

    /**
     * DOCUMENT ME!
     */
    public static final int EVENT_TYPE_DEVICE_COMMANDER = 300;
}
