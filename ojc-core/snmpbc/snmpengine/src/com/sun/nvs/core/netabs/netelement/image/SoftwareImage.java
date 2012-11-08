/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.image;

import java.util.Iterator;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface SoftwareImage {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getVendorName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Iterator getImageAttribues();

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasAttribute(String attrName);

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getImageAttributeValue(String name);

    /**
     * DOCUMENT ME!
     *
     * @param platformType DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isPlatformCompatible(String platformType);

    /**
     * DOCUMENT ME!
     *
     * @param fileSize DOCUMENT ME!
     */
    public void setFileSize(long fileSize);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXMLString();
}
