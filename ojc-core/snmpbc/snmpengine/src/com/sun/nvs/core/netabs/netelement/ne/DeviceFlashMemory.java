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
public interface DeviceFlashMemory {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getIndex();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getMaxSize();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getFreeSize();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isRemovable();

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
    public String getDescription();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getInitTime();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getDevice();
}
