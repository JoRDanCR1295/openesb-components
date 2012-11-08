
//
// Generated by mibgen version 5.1 (05/20/05) when compiling RFC1213-MIB.
//

package com.sun.soabi.snmpagent;

// java imports
//
import java.io.Serializable;

// jmx imports
//
import javax.management.MBeanServer;
import com.sun.management.snmp.SnmpString;
import com.sun.management.snmp.SnmpStatusException;

// jdmk imports
//
import com.sun.management.snmp.agent.SnmpMib;

/**
 * The class is used for implementing the "Interfaces" group.
 * The group is defined with the following oid: 1.3.6.1.2.1.2.
 */
public class Interfaces implements InterfacesMBean, Serializable {

    /**
     * Variable for storing the value of "IfTable".
     * The variable is identified by: "1.3.6.1.2.1.2.2".
     */
    protected TableIfTable IfTable;

    /**
     * Variable for storing the value of "IfNumber".
     * The variable is identified by: "1.3.6.1.2.1.2.1".
     */
    protected Integer IfNumber = new Integer(1);


    /**
     * Constructor for the "Interfaces" group.
     * If the group contains a table, the entries created through an SNMP SET will not be registered in Java DMK.
     */
    public Interfaces(SnmpMib myMib) {
        IfTable = new TableIfTable (myMib);
    }


    /**
     * Constructor for the "Interfaces" group.
     * If the group contains a table, the entries created through an SNMP SET will be AUTOMATICALLY REGISTERED in Java DMK.
     */
    public Interfaces(SnmpMib myMib, MBeanServer server) {
        IfTable = new TableIfTable (myMib, server);
    }

    /**
     * Access the "IfTable" variable.
     */
    public TableIfTable accessIfTable() throws SnmpStatusException {
        return IfTable;
    }

    /**
     * Access the "IfTable" variable as a bean indexed property.
     */
    public IfEntryMBean[] getIfTable() throws SnmpStatusException {
        return IfTable.getEntries();
    }

    /**
     * Getter for the "IfNumber" variable.
     */
    public Integer getIfNumber() throws SnmpStatusException {
        return IfNumber;
    }

}