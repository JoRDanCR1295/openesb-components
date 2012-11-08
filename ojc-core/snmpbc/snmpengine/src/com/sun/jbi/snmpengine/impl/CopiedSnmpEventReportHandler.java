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
 * @(#)CopiedSnmpEventReportHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;



import com.sun.management.snmp.SnmpPduTrap;
import com.sun.management.snmp.SnmpPduPacket;
import com.sun.management.snmp.SnmpPduRequest;
import com.sun.management.snmp.SnmpScopedPduRequest;
import com.sun.management.snmp.SnmpDefinitions;
import com.sun.management.snmp.SnmpPdu;
import com.sun.management.snmp.manager.SnmpInformListener;
import com.sun.management.snmp.manager.SnmpTrapListener;

/**
 * A runnable which simply executes the processSnmp*
 * method of an SnmpEventReportListener.
 *
 * @since Java DMK 5.1
 */

class CopiedSnmpEventReportHandler implements Runnable {
    
    public CopiedSnmpEventReportHandler(SnmpTrapListener l, SnmpPdu p) {
        trapListener = l;
        pdu = p;
    }
    
    public CopiedSnmpEventReportHandler(SnmpInformListener l, SnmpPdu p) {
        informListener = l;
        pdu = p;
    }
    
    public synchronized void run() {
        switch (pdu.type) {
        case SnmpPduPacket.pduV1TrapPdu: 
	    trapListener.processSnmpTrapV1((SnmpPduTrap)pdu);
            break;
        case SnmpPduPacket.pduV2TrapPdu:
	    if(pdu.version == SnmpDefinitions.snmpVersionTwo)
		trapListener.processSnmpTrapV2((SnmpPduRequest) pdu);
	    else
		if(pdu.version == SnmpDefinitions.snmpVersionThree) {
		    // Try/catch is there for compatibility with 4.2
		    try {
			trapListener.processSnmpTrapV3((SnmpScopedPduRequest) pdu);
		    }catch(Exception e) {}	  
		}
	    break;
	case SnmpPduPacket.pduInformRequestPdu:
	    if(pdu.version == SnmpDefinitions.snmpVersionTwo)
		informListener.processSnmpInform((SnmpPduRequest)pdu);
	    else
		if(pdu.version == SnmpDefinitions.snmpVersionThree) {
		    // Try/catch is there for compatibility with 4.2
		    try {
			informListener.processSnmpInformV3((SnmpScopedPduRequest)pdu);
		    }catch(Exception e) {}
		}
            break;	  
	default:
	    break;	  
	}   
    }
    
    // PRIVATE VARIABLES
    //------------------
    
    private SnmpTrapListener trapListener = null;
    private SnmpInformListener informListener = null;
    private SnmpPdu pdu = null;
}
