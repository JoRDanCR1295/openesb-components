
//
// Generated by mibgen version 5.1 (05/20/05) when compiling RFC1213-MIB in standard metadata mode.
//

package com.sun.soabi.snmpagent;

// jmx imports
//
import com.sun.management.snmp.SnmpStatusException;

/**
 * This interface is used for representing the remote management interface for the "System" MBean.
 */
public interface SystemMBean {

    /**
     * Getter for the "SysLocation" variable.
     */
    public String getSysLocation() throws SnmpStatusException;

    /**
     * Setter for the "SysLocation" variable.
     */
    public void setSysLocation(String x) throws SnmpStatusException;

    /**
     * Checker for the "SysLocation" variable.
     */
    public void checkSysLocation(String x) throws SnmpStatusException;

    /**
     * Getter for the "SysName" variable.
     */
    public String getSysName() throws SnmpStatusException;

    /**
     * Setter for the "SysName" variable.
     */
    public void setSysName(String x) throws SnmpStatusException;

    /**
     * Checker for the "SysName" variable.
     */
    public void checkSysName(String x) throws SnmpStatusException;

    /**
     * Getter for the "SysContact" variable.
     */
    public String getSysContact() throws SnmpStatusException;

    /**
     * Setter for the "SysContact" variable.
     */
    public void setSysContact(String x) throws SnmpStatusException;

    /**
     * Checker for the "SysContact" variable.
     */
    public void checkSysContact(String x) throws SnmpStatusException;

    /**
     * Getter for the "SysUpTime" variable.
     */
    public Long getSysUpTime() throws SnmpStatusException;

    /**
     * Getter for the "SysObjectID" variable.
     */
    public String getSysObjectID() throws SnmpStatusException;

    /**
     * Getter for the "SysDescr" variable.
     */
    public String getSysDescr() throws SnmpStatusException;

    /**
     * Getter for the "SysServices" variable.
     */
    public Integer getSysServices() throws SnmpStatusException;
    
    public String getSysTestGetter() throws SnmpStatusException;
    public void setSysTestGetter(String x) throws SnmpStatusException;
    public void checkSysTestGetter(String x) throws SnmpStatusException;

}
