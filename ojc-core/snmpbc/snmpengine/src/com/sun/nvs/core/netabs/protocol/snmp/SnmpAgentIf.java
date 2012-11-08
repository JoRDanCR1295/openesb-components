/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.protocol.snmp;

import java.net.*;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface SnmpAgentIf {
    /**
     * DOCUMENT ME!
     *
     * @param dest DOCUMENT ME!
     * @param data DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public void sendTrap(InetAddress dest, SnmpTrapData data)
        throws Exception;
}
