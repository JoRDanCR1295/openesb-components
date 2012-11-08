/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.protocol.snmp;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface SnmpConstants {
    /**
     * DOCUMENT ME!
     */
    public static int VERSION_V1 = 1;

    /**
     * DOCUMENT ME!
     */
    public static int VERSION_V2C = 2;

    /**
     * DOCUMENT ME!
     */
    public static int VERSION_V3 = 3;

    /**
     * DOCUMENT ME!
     */
    public static int NO_ERROR = 0;

    /**
     * DOCUMENT ME!
     */
    public static int DEFAULT_RETRIES = 2;

    /**
     * DOCUMENT ME!
     */
    public static int DEFAULT_TIMEOUT = 10;

    /**
     * DOCUMENT ME!
     */
    public static String DEFAULT_READCOMMUNITY = "public";

    /**
     * DOCUMENT ME!
     */
    public static String DEFAULT_WRITECOMMUNITY = "public";
}
