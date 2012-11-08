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
public class SnmpTrapData {
    private InetAddress agentAddress = null;
    private String trapCommunity = "public";
    private int genericType = 6;
    private int specificType = 1;

    /**
     * Creates a new SnmpTrapData object.
     */
    public SnmpTrapData() {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public InetAddress getAgentAddress() {
        return agentAddress;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getTrapCommunity() {
        return trapCommunity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getGenericType() {
        return genericType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getSpecificType() {
        return specificType;
    }
}
