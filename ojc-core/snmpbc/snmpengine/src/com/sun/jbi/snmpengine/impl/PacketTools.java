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
 * @(#)PacketTools.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

import com.sun.management.snmp.SnmpPduTrap;
import com.sun.management.snmp.SnmpScopedPduRequest;
import com.sun.management.snmp.SnmpVarBind;

import java.net.InetAddress;

/**
 * Listens to traps and forwards them for processing
 * 
 * @author fkieviet
 */
public class PacketTools {
    /**
     * Dumps a trap
     * 
     * @param trap to dump
     * @return dump
     */
    public static String toString(SnmpPduTrap trap) {
        StringBuilder buf = new StringBuilder();
        buf.append("Source: ").append(trap.address).append(":").append(trap.port).append("; \r");
        buf.append("Generic: ").append(trap.genericTrap).append("; \r");
        buf.append("Specific: ").append(trap.specificTrap).append("; \r");
        buf.append("Timestamp: ").append(trap.timeStamp).append("; \r");
        buf.append("Agent address: ").append(trap.agentAddr).append("; \r");
        buf.append("Community: ").append(trap.community).append("; \r");
        buf.append("Enterprise: ").append(trap.enterprise).append("; \r");
        for (int i = 0; i < trap.varBindList.length; i++) {
            SnmpVarBind b = trap.varBindList[i];
            buf.append("\tvar ").append(i).append(": OID=").append(b.getOid());
            buf.append(", value=").append(b.getStringValue()).append("; \r");
        } 
        return buf.toString();
    }

    /**
     * @param trap to dump
     * @return dump
     */
    public static String toString(SnmpScopedPduRequest trap) {
        StringBuilder buf = new StringBuilder();
        buf.append("Source: ").append(trap.address).append(":").append(trap.port).append("; \r");
        // TODO
        return buf.toString();
    }

    /**
     * Converts an ip address to a numeric form, e.g. 192.168.0.1
     * 
     * @param address to convert
     * @return string
     */
    public static String addressToString(InetAddress address) {
        byte[] addr = address.getAddress();
        StringBuilder buf = new StringBuilder();
        for (byte b : addr) {
            if (buf.length() != 0) {
                buf.append('.');
            }
            buf.append(b);
        }
        
        return buf.toString();
    }
    
}
