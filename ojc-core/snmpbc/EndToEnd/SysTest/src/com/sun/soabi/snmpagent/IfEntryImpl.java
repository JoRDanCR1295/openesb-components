/*
 * @(#)file      IfEntryImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.33
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// java import
//
import java.util.Vector;
import java.net.InetAddress;

// RI import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import com.sun.management.snmp.SnmpOid;
import com.sun.management.snmp.SnmpInt;
import com.sun.management.snmp.SnmpValue;
import com.sun.management.snmp.SnmpVarBind;
import com.sun.management.snmp.SnmpVarBindList;
import com.sun.management.snmp.SnmpDefinitions;
// jdmk import
//
import com.sun.management.comm.SnmpAdaptorServer;
import com.sun.management.comm.SnmpV3AdaptorServer;

import com.sun.management.snmp.agent.SnmpMib;


/**
 * The IfEntryImpl class provides a simple implementation of the "IfEntry".
 * It is used by the "LinkTrapGenerator" for sending SNMP v1 traps. 
 */

public class IfEntryImpl extends IfEntry {

    // MBean properties.
    //
    private String prefix = null;
    private SnmpAdaptorServer snmpAdaptor = null;
    private int securityLevel = SnmpDefinitions.noAuthNoPriv;

    /**
     * Constructor.
     */
    public IfEntryImpl(SnmpMib myMib) {
        super(myMib);
        prefix = myMib.getMibName() + "/ifTable:ifEntry.ifIndex=";
        initialize();
    }

    /**
     * Register this entry in the MBean server.
     */
    public ObjectName createIfEntryObjectName(MBeanServer server) {
        try {
            if (server == null) return null;
            return new ObjectName(prefix + IfIndex);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public void switchifOperStatus() {

        int status = -1;

        switch(IfOperStatus.intValue()) {
                case 1: // "up
                        status = 2;
                        break;
                case 2: // "down
                        status = 3;
                        break;
                case 3: // "testing"
                        status = 1;
                        break;
        }

        switchifOperStatus(new EnumIfOperStatus(status));
    }

    public void switchifOperStatus(EnumIfOperStatus x) {

        int generic = -1;
        IfOperStatus = x;

        switch(x.intValue()) {
                case 1: // "up
                        generic = 3;
                        break;
                case 2: // "down
                case 3: // "testing"
                        generic = 2;
                        break;
        }

        sendTrap(generic);
    }

    /**
     * Try to find the adaptor created by one of the entry-point
     * classes for this example.
     * Obviously this is a quick hack. 
     **/
    private void initialize() {

        boolean encryption=false;
        SnmpAdaptorServer snmpAdaptor = null;
        
        // We use a hack to determine wheter we are within the
        // the scope of Agent.java, StandAloneSnmpAgent.java, 
        // AgentV3.java, AgentEncryptV3.java
        try {
            snmpAdaptor = Agent.getSnmpAdaptor();
            if (snmpAdaptor != null) return;

            snmpAdaptor = StandAloneSnmpAgent.getSnmpAdaptor();
            if (snmpAdaptor != null) return;

            snmpAdaptor = AgentV3.getSnmpAdaptor();
            if (snmpAdaptor != null) return;

            snmpAdaptor = AgentEncryptV3.getSnmpAdaptor();
            if (snmpAdaptor != null) {
                encryption=true;
                return;
            }

        } finally {
            // Finally we set the value of this.snmpAdaptor and 
            // this.securityLevel
            // 
            this.snmpAdaptor=snmpAdaptor;
            this.securityLevel = SnmpDefinitions.noAuthNoPriv;
            if (snmpAdaptor == null) return;
            if (snmpAdaptor instanceof SnmpV3AdaptorServer) {
                this.securityLevel = (encryption?SnmpDefinitions.authPriv:
                                      SnmpDefinitions.authNoPriv);
            } 
        }
    }

    /**
     * Send SNMP v1 traps with the generic number of the trap
     * according to the "IfOperStatus" value.
     */
    public void sendTrap(int generic) {

        if (snmpAdaptor == null) {
            java.lang.System.err.println("BUG: IfEntryImpl.sendTrap(): " +
                                         "snmpAdaptor is null");
            return;
        }

        String generic_string= null;

        switch (generic) {
                case 3:
                        generic_string = "linkUp";
                        break;
                case 2:
                        generic_string = "linkDown";
                        break;
                default:
                        java.lang.System.err.println(
                                             "BUG: IfEntryImpl.sendTrap(): " +
                                             "bad generic: " + generic);
                        return;
        }

        SnmpVarBindList varBindList = new SnmpVarBindList();

        SnmpOid oid1 = new SnmpOid("1.3.6.1.2.1.2.2.1.1." + IfIndex);
        SnmpInt value1 = new SnmpInt(IfIndex);
        SnmpVarBind varBind1 = new SnmpVarBind(oid1, (SnmpValue) value1);

        varBindList.addVarBind(varBind1);

        java.lang.System.out.print("NOTE: Sending a " + generic_string +
                                   " SNMP trap for the Interface " + 
                                   IfIndex +
                                   " to each destination defined" +
                                   " in the ACL file...");
        try {
            //SNMP V3 trap if V3 agent only.
            if(snmpAdaptor instanceof SnmpV3AdaptorServer) {
                final SnmpV3AdaptorServer adaptorV3=
                    (SnmpV3AdaptorServer)snmpAdaptor;
                adaptorV3.snmpV3UsmTrap("defaultUser",
                                        securityLevel,
                                        "TEST-CONTEXT",
                                        new SnmpOid("1.2.3.4.5.6.7.8.9.0"), 
                                        varBindList);
            }
            // Send V1 trap in every case.
            snmpAdaptor.snmpV1Trap(generic, 0, varBindList);
        } catch (Exception e) {
            e.printStackTrace();
        }
        java.lang.System.out.println("Done.");
    }
}

