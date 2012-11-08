/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.devicedata;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface AttributeMetaData {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getAttributeName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Class getAttributeType();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isRequired();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isIndex();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getDisplayWidth();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDisplayName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isAutoIncrement();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getMaxDbSize();
}
