/*
 * @(#)file      InterfacesImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.28
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// java import
//

// RI import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;

// jdmk import
//
import com.sun.management.snmp.agent.SnmpMib;


/**
 * The InterfacesImpl class provides an simple implementation of the
 * "Interfaces" group.
 * It creates two entries in the "IfTable" at instantiation time and
 * add them into the MBean server. 
 */

public class InterfacesImpl extends Interfaces {

    // MBean properties.
    //
    private static IfEntryImpl le0 = null;
    private static IfEntryImpl lo0 = null;

    /**
     * Constructors.
     */
    public InterfacesImpl(SnmpMib myMib) {
        super(myMib);
        init(myMib,null);
    }

    public InterfacesImpl(SnmpMib myMib, MBeanServer server) {
        super(myMib, server);
        init(myMib, server);
    }

    private void init(SnmpMib myMib, MBeanServer server) {

        // Add TableEntryListenerImpl as notification listener on the table.
        //
        IfTable.addNotificationListener(new TableEntryListenerImpl(), null, 
					null);
        
        IfNumber = new Integer(2);

        le0 = new IfEntryImpl(myMib);
        le0.IfIndex = new Integer(1);
        le0.IfMtu = new Integer(1500);
        le0.IfType = new EnumIfType(6); // "ethernet-csmacd"
        le0.IfDescr = "le0";
        le0.IfOperStatus = new EnumIfOperStatus(1); // "up"

        lo0 = new IfEntryImpl(myMib);
        lo0.IfIndex = new Integer(2);
        lo0.IfMtu = new Integer(8232);
        lo0.IfType = new EnumIfType(24); // "softwareLoopback"
        lo0.IfDescr = "lo0";
        lo0.IfOperStatus = new EnumIfOperStatus(1); // "up"

	final ObjectName le0Name = le0.createIfEntryObjectName(server);
	final ObjectName lo0Name = lo0.createIfEntryObjectName(server);

        // Register le0 and lo0 into the ifTable.
        //
        try {
            IfTable.addEntry(le0,le0Name);
            IfTable.addEntry(lo0,lo0Name);
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Register le0 and lo0 into the MBeanServer if needed.
        //
	try {
	    if (server != null && le0Name != null) 
		server.registerMBean(le0,le0Name);
	    if (server != null && lo0Name != null) 
		server.registerMBean(lo0,lo0Name);
	} catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static IfEntryImpl find(int ifIndex) {
        if (ifIndex == le0.IfIndex.intValue()) {
            return le0;
        } else if (ifIndex == lo0.IfIndex.intValue()) {
            return lo0;
        } else {
            return null;
        }
    }
}



