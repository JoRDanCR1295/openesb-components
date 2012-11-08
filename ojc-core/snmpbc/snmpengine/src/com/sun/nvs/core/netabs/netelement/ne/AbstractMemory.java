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
public abstract class AbstractMemory implements DeviceMemory {
    /**
     * DOCUMENT ME!
     */
    protected String memoryName = null;

    /**
     * DOCUMENT ME!
     */
    protected String memoryPoolType = null;

    /**
     * DOCUMENT ME!
     */
    protected int memoryPoolTypeInt = -1;

    /**
     * DOCUMENT ME!
     */
    protected boolean hasAlternate = false;

    /**
     * DOCUMENT ME!
     */
    protected long used = 0L;

    /**
     * DOCUMENT ME!
     */
    protected long free = 0L;

    /**
     * DOCUMENT ME!
     */
    protected long largestContiguousFree = 0L;

    /**
     * DOCUMENT ME!
     */
    protected long total = 0L;

    /**
     * DOCUMENT ME!
     */
    protected Device d = null;

    /**
     * DOCUMENT ME!
     */
    protected EntityId entId = null;

    /**
     * Creates a new AbstractMemory object.
     *
     * @param name DOCUMENT ME!
     * @param poolType DOCUMENT ME!
     */
    protected AbstractMemory(String name, int poolType) {
        this.memoryName = name;
        this.memoryPoolTypeInt = poolType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param free DOCUMENT ME!
     */
    public void setFree(long free) {
        this.free = free;
    }

    /**
     * DOCUMENT ME!
     *
     * @param used DOCUMENT ME!
     */
    public void setUsed(long used) {
        this.used = used;
    }

    /**
     * DOCUMENT ME!
     */
    public void setTotal() {
        total = free + used;
    }

    /**
     * DOCUMENT ME!
     *
     * @param largestFree DOCUMENT ME!
     */
    public void setLargestFree(long largestFree) {
        this.largestContiguousFree = largestFree;
    }

    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     */
    public void setDevice(Device d) {
        this.d = d;
        updateEntId();
    }

    /**
     * DOCUMENT ME!
     *
     * @param hasAlternate DOCUMENT ME!
     */
    public void setAlternate(boolean hasAlternate) {
        this.hasAlternate = hasAlternate;
    }

    /**
     * DOCUMENT ME!
     */
    protected void updateEntId() {
        if ((d != null) && (memoryName != null)) {
            entId = new EntityId(d.getEntityId(), memoryName);
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
    public long getUsed() {
        return used;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getFree() {
        return free;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getLargestContiguousFree() {
        return largestContiguousFree;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getTotal() {
        return total;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName() {
        return memoryName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getType() {
        return memoryPoolTypeInt;
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
    public boolean hasAlternate() {
        return hasAlternate;
    }
}
