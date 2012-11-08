
//
// Generated by mibgen version 5.1 (05/20/05) when compiling RFC1213-MIB in standard metadata mode.
//

package com.sun.soabi.snmpagent;


// jmx imports
//
import com.sun.management.snmp.SnmpStatusException;

/**
 * This interface is used for representing the remote management interface for the "Snmp" MBean.
 */
public interface SnmpMBean {

    /**
     * Getter for the "SnmpEnableAuthenTraps" variable.
     */
    public EnumSnmpEnableAuthenTraps getSnmpEnableAuthenTraps() throws SnmpStatusException;

    /**
     * Setter for the "SnmpEnableAuthenTraps" variable.
     */
    public void setSnmpEnableAuthenTraps(EnumSnmpEnableAuthenTraps x) throws SnmpStatusException;

    /**
     * Checker for the "SnmpEnableAuthenTraps" variable.
     */
    public void checkSnmpEnableAuthenTraps(EnumSnmpEnableAuthenTraps x) throws SnmpStatusException;

    /**
     * Getter for the "SnmpInTraps" variable.
     */
    public Long getSnmpInTraps() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInGetResponses" variable.
     */
    public Long getSnmpInGetResponses() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInSetRequests" variable.
     */
    public Long getSnmpInSetRequests() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInGetNexts" variable.
     */
    public Long getSnmpInGetNexts() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInGetRequests" variable.
     */
    public Long getSnmpInGetRequests() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInTotalSetVars" variable.
     */
    public Long getSnmpInTotalSetVars() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInTotalReqVars" variable.
     */
    public Long getSnmpInTotalReqVars() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInGenErrs" variable.
     */
    public Long getSnmpInGenErrs() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInReadOnlys" variable.
     */
    public Long getSnmpInReadOnlys() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInBadValues" variable.
     */
    public Long getSnmpInBadValues() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInNoSuchNames" variable.
     */
    public Long getSnmpInNoSuchNames() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInTooBigs" variable.
     */
    public Long getSnmpInTooBigs() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutTraps" variable.
     */
    public Long getSnmpOutTraps() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutGetResponses" variable.
     */
    public Long getSnmpOutGetResponses() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInASNParseErrs" variable.
     */
    public Long getSnmpInASNParseErrs() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutSetRequests" variable.
     */
    public Long getSnmpOutSetRequests() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInBadCommunityUses" variable.
     */
    public Long getSnmpInBadCommunityUses() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutGetNexts" variable.
     */
    public Long getSnmpOutGetNexts() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutGetRequests" variable.
     */
    public Long getSnmpOutGetRequests() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInBadCommunityNames" variable.
     */
    public Long getSnmpInBadCommunityNames() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutGenErrs" variable.
     */
    public Long getSnmpOutGenErrs() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInBadVersions" variable.
     */
    public Long getSnmpInBadVersions() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutPkts" variable.
     */
    public Long getSnmpOutPkts() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutBadValues" variable.
     */
    public Long getSnmpOutBadValues() throws SnmpStatusException;

    /**
     * Getter for the "SnmpInPkts" variable.
     */
    public Long getSnmpInPkts() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutNoSuchNames" variable.
     */
    public Long getSnmpOutNoSuchNames() throws SnmpStatusException;

    /**
     * Getter for the "SnmpOutTooBigs" variable.
     */
    public Long getSnmpOutTooBigs() throws SnmpStatusException;

}