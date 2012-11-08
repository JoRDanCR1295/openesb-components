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
public interface DeviceMemory extends EntityIf {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EntityId getEntityId();

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
    public long getUsed();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getFree();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getLargestContiguousFree();

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
    public long getTotal();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasAlternate();
}
