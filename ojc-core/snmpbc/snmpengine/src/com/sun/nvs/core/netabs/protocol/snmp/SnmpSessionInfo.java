/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.protocol.snmp;

import com.sun.nvs.core.netabs.netelement.ne.CredentialSet;
import com.sun.nvs.core.netabs.netelement.ne.IpAddress;


//import com.sun.nvs.core.servicemodules.security.aaa.*;
/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class SnmpSessionInfo implements SnmpConstants {
    private String host;
    private int port = 161;
    private int timeout = DEFAULT_TIMEOUT;
    private int retries = DEFAULT_RETRIES;
    private String readCommunity = DEFAULT_READCOMMUNITY;
    private String writeCommunity = DEFAULT_WRITECOMMUNITY;

    /**
     * Creates a new SnmpSessionInfo object.
     *
     * @param hostName DOCUMENT ME!
     * @param snmpPort DOCUMENT ME!
     * @param community DOCUMENT ME!
     * @param to DOCUMENT ME!
     * @param re DOCUMENT ME!
     */
    public SnmpSessionInfo(String hostName, int snmpPort, String community, int to, int re) {
        this(hostName, snmpPort, community, community, to, re);
    }

    /**
     * Creates a new SnmpSessionInfo object.
     *
     * @param hostName DOCUMENT ME!
     * @param readComm DOCUMENT ME!
     * @param writeComm DOCUMENT ME!
     */
    public SnmpSessionInfo(String hostName, String readComm, String writeComm) {
        this(hostName, 161, readComm, writeComm, DEFAULT_TIMEOUT, DEFAULT_RETRIES);
    }

    /**
     * Creates a new SnmpSessionInfo object.
     *
     * @param hostName DOCUMENT ME!
     * @param communityR DOCUMENT ME!
     * @param communityW DOCUMENT ME!
     * @param vlanId DOCUMENT ME!
     * @param useVlanId DOCUMENT ME!
     */
    public SnmpSessionInfo(
        String hostName, String communityR, String communityW, String vlanId, boolean useVlanId
    ) {
        this(hostName, communityR + "@" + vlanId, communityW + "@" + vlanId);
    }

    /**
     * Creates a new SnmpSessionInfo object.
     *
     * @param hostName DOCUMENT ME!
     * @param community DOCUMENT ME!
     */
    public SnmpSessionInfo(String hostName, String community) {
        this(hostName, community, community);
    }

    /**
     * Creates a new SnmpSessionInfo object.
     *
     * @param hostName DOCUMENT ME!
     * @param snmpPort DOCUMENT ME!
     * @param readComm DOCUMENT ME!
     * @param writeComm DOCUMENT ME!
     * @param to DOCUMENT ME!
     * @param re DOCUMENT ME!
     */
    public SnmpSessionInfo(
        String hostName, int snmpPort, String readComm, String writeComm, int to, int re
    ) {
        host = hostName;
        port = snmpPort;
        timeout = to;
        retries = re;
        readCommunity = readComm;
        writeCommunity = writeComm;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static SnmpSessionInfo getSession(IpAddress ip) {
        return getSession(ip, null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     * @param vlanId DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static SnmpSessionInfo getSession(IpAddress ip, String vlanId) {
        CredentialSet cset = null; //CredentialCache.instance().getCredentialSetForDevice(ip);

        if (vlanId != null) {
            return new SnmpSessionInfo(
                ip.toString(), cset.getReadCommunity(), cset.getWriteCommunity(), vlanId, true
            );
        } else {
            return new SnmpSessionInfo(
                ip.toString(), cset.getReadCommunity(), cset.getWriteCommunity()
            );
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getHost() {
        return host;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getPort() {
        return port;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getReadCommunity() {
        return readCommunity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getWriteCommunity() {
        return writeCommunity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getTimeOut() {
        return timeout;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getRetries() {
        return retries;
    }
}
