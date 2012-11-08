/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.netabs.protocol.snmp.SnmpClientIf;
import com.sun.nvs.core.netabs.protocol.snmp.SnmpSessionInfo;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface DeviceFactoryIf {
    /**
     * DOCUMENT ME!
     *
     * @param oid DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDeviceType(String oid);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] supportedOids();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] supportedOidPrefixes();

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     * @param deviceId DOCUMENT ME!
     * @param sysOid DOCUMENT ME!
     * @param session DOCUMENT ME!
     * @param client DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device createDevice(
        IpAddress ip, int deviceId, String sysOid, SnmpSessionInfo session, SnmpClientIf client
    );

    /**
     * DOCUMENT ME!
     *
     * @param oldReference DOCUMENT ME!
     * @param s DOCUMENT ME!
     * @param session DOCUMENT ME!
     * @param client DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device updateDevice(
        Device oldReference, IpAddress s, SnmpSessionInfo session, SnmpClientIf client
    );
}
