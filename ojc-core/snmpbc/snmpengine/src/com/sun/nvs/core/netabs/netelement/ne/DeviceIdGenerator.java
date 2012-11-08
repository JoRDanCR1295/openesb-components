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
public final class DeviceIdGenerator {
    private static final DeviceIdGenerator _instance = new DeviceIdGenerator();
    private int count = 0;

    private DeviceIdGenerator() {
    }

    private synchronized int getNextId() {
        return count++;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final int nextId() {
        return _instance.getNextId();
    }
}
