
//
// Generated by mibgen version 5.1 (05/20/05) when compiling CORE-MIB.
//

package com.sun.soabi.snmpagent;

// java imports
//
import java.io.Serializable;

// jmx imports
//
import com.sun.management.snmp.SnmpOidRecord;

// jdmk imports
//
import com.sun.management.snmp.SnmpOidTableSupport;

/**
 * The class contains metadata definitions for "CORE-MIB".
 * Call SnmpOid.setSnmpOidTable(new CORE_MIBOidTable()) to load the metadata in the SnmpOidTable.
 */
public class CORE_MIBOidTable extends SnmpOidTableSupport implements Serializable {

    /**
     * Default constructor. Initialize the Mib tree.
     */
    public CORE_MIBOidTable() {
        super("CORE_MIB");
        loadMib(varList);
    }

    static SnmpOidRecord varList [] = {
    };
}
