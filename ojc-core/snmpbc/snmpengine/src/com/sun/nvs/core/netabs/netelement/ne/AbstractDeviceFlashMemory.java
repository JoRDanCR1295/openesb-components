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
public abstract class AbstractDeviceFlashMemory implements DeviceFlashMemory {
    /**
     * DOCUMENT ME!
     */
    protected int index = -1;

    /**
     * DOCUMENT ME!
     */
    protected String name = null;

    /**
     * DOCUMENT ME!
     */
    protected String description = null;

    /**
     * DOCUMENT ME!
     */
    protected long maxSize = 0;

    /**
     * DOCUMENT ME!
     */
    protected long freeSpace = 0;

    /**
     * DOCUMENT ME!
     */
    protected boolean isRemovable = false;

    /**
     * DOCUMENT ME!
     */
    protected long initializedAt = 0L;

    /**
     * DOCUMENT ME!
     */
    protected Device d = null;

    /**
     * Creates a new AbstractDeviceFlashMemory object.
     *
     * @param index DOCUMENT ME!
     * @param name DOCUMENT ME!
     */
    protected AbstractDeviceFlashMemory(int index, String name) {
        this.index = index;
        this.name = name;
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
     * @return DOCUMENT ME!
     */
    public String getName() {
        return name;
    }

    /**
     * DOCUMENT ME!
     *
     * @param descr DOCUMENT ME!
     */
    public void setDescrption(String descr) {
        this.description = descr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     */
    public void setDevice(Device d) {
        this.d = d;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device getDevice() {
        return d;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDescription() {
        return description;
    }

    /**
     * DOCUMENT ME!
     *
     * @param removable DOCUMENT ME!
     */
    public void setRemovable(boolean removable) {
        this.isRemovable = removable;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isRemovable() {
        return isRemovable;
    }

    /**
     * DOCUMENT ME!
     *
     * @param size DOCUMENT ME!
     */
    public void setSize(long size) {
        this.maxSize = size;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getMaxSize() {
        return this.maxSize;
    }

    /**
     * DOCUMENT ME!
     *
     * @param size DOCUMENT ME!
     */
    public void setAvailableSpace(long size) {
        this.freeSpace = size;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getFreeSize() {
        return this.freeSpace;
    }

    /**
     * DOCUMENT ME!
     *
     * @param time DOCUMENT ME!
     */
    public void setInitTime(long time) {
        this.initializedAt = time;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getInitTime() {
        return this.initializedAt;
    }
}
