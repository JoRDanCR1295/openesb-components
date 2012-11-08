/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ChangedSnmpEventReportDispatcher.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

import com.sun.jdmk.tasks.DaemonTaskServer;
import com.sun.jdmk.tasks.TaskServer;
import com.sun.management.snmp.SnmpEngine;
import com.sun.management.snmp.SnmpStatusException;
import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

import java.net.DatagramPacket;

/**
 * @author fkieviet
 */
public class ChangedSnmpEventReportDispatcher extends CopiedSnmpEventReportDispatcher2 {

    /**
     * Special packet for testing: when encountered, it will send a sync packet back to
     * the originating socket
     */
    public static final String SYNCPOINT_B64 
    = "MEICAQAECVNZTkNQT0lOVKQyBggrBgEEAQMBAUAEfwAAAQIBAwIBAEMBADAXMBUGCCsGAQIBAQEABAlTWU5DUE9JTlQ=";

    /**
     * Special packet for testing: when encountered, it will send a sync packet back to
     * the originating socket
     */
    public static final byte[] SYNCPOINTBUF = Base64.decode(SYNCPOINT_B64);
    
    /**
     * @see CopiedSnmpEventReportDispatcher2#CopiedSnmpEventReportDispatcher2(SnmpEngine, 
     *     int, java.net.InetAddress, TaskServer, TaskServer)
     */
    public ChangedSnmpEventReportDispatcher(SnmpEngine engine, int port, 
        DaemonTaskServer taskServer, TaskServer callbackTaskServer) throws Exception {
        super(engine, port, taskServer, callbackTaskServer);
    }

    /**
     * @see com.sun.jbi.snmpengine.impl.CopiedSnmpEventReportDispatcher#handlePacket(
     *     java.net.DatagramPacket)
     */
    protected void handlePacket(DatagramPacket packet) throws SnmpStatusException {
        // Check for syncpoint
        boolean syncpoint = packet.getLength() == SYNCPOINTBUF.length;
        if (syncpoint) {
            int n = SYNCPOINTBUF.length;
            byte[] buf = packet.getData();
            for (int i = 0; i < n; i++) {
                if (SYNCPOINTBUF[i] != buf[i]) {
                    syncpoint = false;
                    break;
                }
            }
        }
        
        if (syncpoint) {
            sendPacket(packet);
        } else {
            // Ordinary trap
            super.handlePacket(packet);
        }
    }

    /**
     * @see com.sun.jbi.snmpengine.impl.CopiedSnmpEventReportDispatcher#doHandlePacket(int, 
     *     java.net.DatagramPacket)
     */
    protected void doHandlePacket(final int version, final DatagramPacket packet) {
        super.doHandlePacket(version, packet);
    }
}
