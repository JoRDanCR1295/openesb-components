
//
// Generated by mibgen version 5.1 (05/20/05) when compiling RFC1213-MIB.
//

package com.sun.soabi.snmpagent;

// java imports
//
import java.io.Serializable;
import java.util.Hashtable;

// RI imports
//
import com.sun.jdmk.Enumerated;

/**
 * The class is used for representing "SnmpEnableAuthenTraps".
 */
public class EnumSnmpEnableAuthenTraps extends Enumerated implements Serializable {

    protected static Hashtable intTable = new Hashtable();
    protected static Hashtable stringTable = new Hashtable();
    static  {
        intTable.put(new Integer(1), "enabled");
        intTable.put(new Integer(2), "disabled");
        stringTable.put("enabled", new Integer(1));
        stringTable.put("disabled", new Integer(2));
    }

    public EnumSnmpEnableAuthenTraps(int valueIndex) throws IllegalArgumentException {
        super(valueIndex);
    }

    public EnumSnmpEnableAuthenTraps(Integer valueIndex) throws IllegalArgumentException {
        super(valueIndex);
    }

    public EnumSnmpEnableAuthenTraps() throws IllegalArgumentException {
        super();
    }

    public EnumSnmpEnableAuthenTraps(String x) throws IllegalArgumentException {
        super(x);
    }

    protected Hashtable getIntTable() {
        return intTable ;
    }

    protected Hashtable getStringTable() {
        return stringTable ;
    }

}
