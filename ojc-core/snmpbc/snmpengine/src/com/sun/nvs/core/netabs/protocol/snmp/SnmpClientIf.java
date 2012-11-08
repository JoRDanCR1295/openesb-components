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
public interface SnmpClientIf {
    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param argv DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws SnmpException DOCUMENT ME!
     */
    public SnmpResult snmpGet(SnmpSessionInfo s, String[] argv)
        throws SnmpException;

    /**
     * DOCUMENT ME!
     *
     * @param MibName DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public void loadMibs(String MibName) throws Exception;

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param argv DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws SnmpException DOCUMENT ME!
     */
    public SnmpWalkResult snmpWalk(SnmpSessionInfo s, String[] argv)
        throws SnmpException;
}
