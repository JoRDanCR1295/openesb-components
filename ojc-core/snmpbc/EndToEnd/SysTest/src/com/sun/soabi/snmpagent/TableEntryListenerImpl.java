/*
 * @(#)file      TableEntryListenerImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.25
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// JMX imports
//
import javax.management.Notification;
import javax.management.NotificationListener;

// JDMK imports
//
import com.sun.management.snmp.agent.SnmpTableEntryNotification;
import com.sun.management.snmp.agent.SnmpMibTable;

/**
 * This class receives SnmpTableEntryNotifications when an entry 
 * is added to or removed from the "IfTable".  
 */

public class TableEntryListenerImpl implements NotificationListener {

    public  void handleNotification(Notification notification,
                                    Object handback) {
        
        SnmpTableEntryNotification notif =
            (SnmpTableEntryNotification) notification;
        SnmpMibTable table = (SnmpMibTable) notif.getSource();
        String type = notif.getType();
        
        try {
            if (type.equals(SnmpTableEntryNotification.SNMP_ENTRY_ADDED)) {
                java.lang.System.out.println("NOTE: TableEntryListenerImpl " +
                                             "received event \"Entry added\":");
                IfEntryImpl added = (IfEntryImpl) notif.getEntry();
                java.lang.System.out.println("\tIfIndex = " +
                                             added.getIfIndex());
                java.lang.System.out.println("\tIfMtu = " +
                                             added.getIfMtu());
                java.lang.System.out.println("\tIfType = " +
                                             added.getIfType());
                java.lang.System.out.println("\tIfDescr = " +
                                             added.getIfDescr());
                java.lang.System.out.println("\tIfOperStatus = " +
                                             added.getIfOperStatus());
            } else if (type.equals(
                            SnmpTableEntryNotification.SNMP_ENTRY_REMOVED)) {
                java.lang.System.out.println("NOTE: TableEntryListenerImpl " +
                                             "received event " +
                                             "\"Entry removed\":");
            } else {
                java.lang.System.out.println("\n\t>> Unknown event type (?)\n");
            }
        } catch (Exception e) {
            e.printStackTrace();
            java.lang.System.exit(1);
        }
    }
}
