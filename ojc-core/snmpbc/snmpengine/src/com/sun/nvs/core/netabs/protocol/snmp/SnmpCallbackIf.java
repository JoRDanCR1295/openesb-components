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
public interface SnmpCallbackIf {
    /**
     * DOCUMENT ME!
     *
     * @param handle DOCUMENT ME!
     * @param varsNames DOCUMENT ME!
     * @param values DOCUMENT ME!
     */
    public void handleResponse(Object handle, String[] varsNames, Object[] values);
}
