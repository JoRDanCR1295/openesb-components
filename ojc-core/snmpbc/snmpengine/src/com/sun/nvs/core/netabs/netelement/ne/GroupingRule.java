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
public interface GroupingRule {
    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean includeInGroup(Device d);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXmlString();
}
