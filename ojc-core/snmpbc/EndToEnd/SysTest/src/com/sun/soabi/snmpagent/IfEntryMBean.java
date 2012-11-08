
//
// Generated by mibgen version 5.1 (05/20/05) when compiling RFC1213-MIB in standard metadata mode.
//

package com.sun.soabi.snmpagent;

// jmx imports
//
import com.sun.management.snmp.SnmpStatusException;

/**
 * This interface is used for representing the remote management interface for the "IfEntry" MBean.
 */
public interface IfEntryMBean {

    /**
     * Getter for the "IfOutDiscards" variable.
     */
    public Long getIfOutDiscards() throws SnmpStatusException;

    /**
     * Getter for the "IfOutNUcastPkts" variable.
     */
    public Long getIfOutNUcastPkts() throws SnmpStatusException;

    /**
     * Getter for the "IfOutUcastPkts" variable.
     */
    public Long getIfOutUcastPkts() throws SnmpStatusException;

    /**
     * Getter for the "IfOutOctets" variable.
     */
    public Long getIfOutOctets() throws SnmpStatusException;

    /**
     * Getter for the "IfInUnknownProtos" variable.
     */
    public Long getIfInUnknownProtos() throws SnmpStatusException;

    /**
     * Getter for the "IfInErrors" variable.
     */
    public Long getIfInErrors() throws SnmpStatusException;

    /**
     * Getter for the "IfInDiscards" variable.
     */
    public Long getIfInDiscards() throws SnmpStatusException;

    /**
     * Getter for the "IfInNUcastPkts" variable.
     */
    public Long getIfInNUcastPkts() throws SnmpStatusException;

    /**
     * Getter for the "IfInUcastPkts" variable.
     */
    public Long getIfInUcastPkts() throws SnmpStatusException;

    /**
     * Getter for the "IfInOctets" variable.
     */
    public Long getIfInOctets() throws SnmpStatusException;

    /**
     * Getter for the "IfLastChange" variable.
     */
    public Long getIfLastChange() throws SnmpStatusException;

    /**
     * Getter for the "IfOperStatus" variable.
     */
    public EnumIfOperStatus getIfOperStatus() throws SnmpStatusException;

    /**
     * Getter for the "IfAdminStatus" variable.
     */
    public EnumIfAdminStatus getIfAdminStatus() throws SnmpStatusException;

    /**
     * Setter for the "IfAdminStatus" variable.
     */
    public void setIfAdminStatus(EnumIfAdminStatus x) throws SnmpStatusException;

    /**
     * Checker for the "IfAdminStatus" variable.
     */
    public void checkIfAdminStatus(EnumIfAdminStatus x) throws SnmpStatusException;

    /**
     * Getter for the "IfPhysAddress" variable.
     */
    public Byte[] getIfPhysAddress() throws SnmpStatusException;

    /**
     * Getter for the "IfSpeed" variable.
     */
    public Long getIfSpeed() throws SnmpStatusException;

    /**
     * Getter for the "IfMtu" variable.
     */
    public Integer getIfMtu() throws SnmpStatusException;

    /**
     * Getter for the "IfType" variable.
     */
    public EnumIfType getIfType() throws SnmpStatusException;

    /**
     * Getter for the "IfDescr" variable.
     */
    public String getIfDescr() throws SnmpStatusException;

    /**
     * Getter for the "IfIndex" variable.
     */
    public Integer getIfIndex() throws SnmpStatusException;

    /**
     * Getter for the "IfSpecific" variable.
     */
    public String getIfSpecific() throws SnmpStatusException;

    /**
     * Getter for the "IfOutQLen" variable.
     */
    public Long getIfOutQLen() throws SnmpStatusException;

    /**
     * Getter for the "IfOutErrors" variable.
     */
    public Long getIfOutErrors() throws SnmpStatusException;

}
