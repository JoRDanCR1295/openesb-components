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
public interface RecordMetaData {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getRecordTypeName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getRecordTypeId();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getAttributeNames();

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AttributeMetaData getAttributeMetaData(String attrName);
}
