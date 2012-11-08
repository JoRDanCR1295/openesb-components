/*
 * @(#)file      SnmpImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.25
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// java import
//
import java.io.Serializable;

// RI import
//
import javax.management.MBeanServer;
import com.sun.management.snmp.SnmpStatusException;

// jdmk import
//
import com.sun.management.comm.SnmpAdaptorServer;
import com.sun.management.snmp.agent.SnmpMib;


public class SnmpImpl extends Snmp implements Serializable {

    // MBean properties.
    //
    transient private SnmpMib myMib;

    /**
     * Constructors.
     */
    public SnmpImpl(SnmpMib myMib) {
        
        super(myMib);
        init(myMib);
    }
    
    public SnmpImpl(SnmpMib myMib, MBeanServer server) {
        
        super(myMib, server);
        init(myMib);
    }
    
    private void init(SnmpMib myMib) {
        
        SnmpOutSetRequests = new Long((long)0);
        SnmpOutGetNexts = new Long((long)0);
        SnmpOutGetRequests = new Long((long)0);
        SnmpInNoSuchNames = new Long((long)0);
        SnmpInTooBigs = new Long((long)0);
        SnmpInBadCommunityUses = new Long((long)0);
        SnmpInTraps = new Long((long)0);
        SnmpInGetResponses = new Long((long)0);
        SnmpInGenErrs = new Long((long)0);
        SnmpInReadOnlys = new Long((long)0);
        SnmpInBadValues = new Long((long)0);
        
        // Most of the information are going to come from the SNMP adaptor.
        // Access it.
        //
        this.myMib= myMib;
    }
    
    /**
     * Getter for the "SnmpOutTraps" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutTraps() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutTraps();
    }

    /**
     * Getter for the "SnmpOutGetResponses" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutGetResponses() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutGetResponses();
    }

    /**
     * Getter for the "SnmpOutGenErrs" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutGenErrs() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutGenErrs();
    }

    /**
     * Getter for the "SnmpOutBadValues" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutBadValues() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutBadValues();
    }

    /**
     * Getter for the "SnmpOutNoSuchNames" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutNoSuchNames() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutNoSuchNames();
    }

    /**
     * Getter for the "SnmpOutTooBigs" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutTooBigs() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutTooBigs();
    }

    /**
     * Getter for the "SnmpInASNParseErrs" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInASNParseErrs() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInASNParseErrs();
    }

    /**
     * Getter for the "SnmpInBadCommunityNames" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInBadCommunityNames() throws SnmpStatusException {
        return  accessSnmpStack().getSnmpInBadCommunityNames();
    }

    /**
     * Getter for the "SnmpInBadVersions" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInBadVersions() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInBadVersions();
    }

    /**
     * Getter for the "SnmpInSetRequests" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInSetRequests() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInSetRequests();
    }

    /**
     * Getter for the "SnmpOutPkts" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpOutPkts() throws SnmpStatusException {
        return accessSnmpStack().getSnmpOutPkts();
    }

    /**
     * Getter for the "SnmpInGetNexts" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInGetNexts() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInGetNexts();
    }

    /**
     * Getter for the "SnmpInPkts" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInPkts() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInPkts();
    }

    /**
     * Getter for the "SnmpInGetRequests" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInGetRequests() throws SnmpStatusException {
        return accessSnmpStack().getSnmpInGetRequests();
    }

    /**
     * Getter for the "SnmpInTotalSetVars" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInTotalSetVars() throws SnmpStatusException {
        return  accessSnmpStack().getSnmpInTotalSetVars();
    }

    /**
     * Getter for the "SnmpInTotalReqVars" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public Long getSnmpInTotalReqVars() throws SnmpStatusException {
        return  accessSnmpStack().getSnmpInTotalReqVars();
    }

    /**
     * Getter for the "SnmpEnableAuthenTraps" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public EnumSnmpEnableAuthenTraps getSnmpEnableAuthenTraps()
        throws SnmpStatusException {
    
        if (accessSnmpStack().getAuthTrapEnabled()) {
            return new EnumSnmpEnableAuthenTraps("enabled");
        } 
        return new EnumSnmpEnableAuthenTraps("disabled");
    }

    /**
     * Setter for the "SnmpEnableAuthenTraps" variable.
     * @exception SnmpStatusException An error occurred while accessing a MIB
     * node.
     */
    public  void setSnmpEnableAuthenTraps(EnumSnmpEnableAuthenTraps x)
        throws SnmpStatusException {
        if (x.intValue() == 2) {
            accessSnmpStack().setAuthTrapEnabled(false);
        } else {
            accessSnmpStack().setAuthTrapEnabled(true);
        }
    }

    private SnmpAdaptorServer accessSnmpStack() {
        return (SnmpAdaptorServer)myMib.getSnmpAdaptor();
    }
}
