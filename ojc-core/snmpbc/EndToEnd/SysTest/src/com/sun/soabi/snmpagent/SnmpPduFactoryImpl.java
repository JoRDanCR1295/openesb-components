/*
 * @(#)file      SnmpPduFactoryImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.31
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// JMX imports
//
import com.sun.management.snmp.SnmpPduFactory;
import com.sun.management.snmp.SnmpPduPacket;
import com.sun.management.snmp.SnmpMessage;
import com.sun.management.snmp.SnmpStatusException;
import com.sun.management.snmp.SnmpTooBigException;
import com.sun.management.snmp.SnmpMsg;
import com.sun.management.snmp.SnmpPdu;

// jdmk import
//
import com.sun.management.snmp.SnmpPduFactoryBER;

/**
 * This class implements the SnmpPduFactory interface in order to filter 
 * the received messages according to the host that sent them.  
 */

public class SnmpPduFactoryImpl implements SnmpPduFactory {

    // MBean properties.
    //
    private String[] hostNames;
    private final SnmpPduFactory berFactory;

    /**
     * Constructor.
     * @param hostNames The array of the host names whose requests will 
     * be refused by the agent.
     */
    public SnmpPduFactoryImpl(String[] hostNames) {
        this.hostNames = hostNames;
        this.berFactory = new SnmpPduFactoryBER();
    }

    /**
     * This method is called when a PDU is sent.
     */
    public SnmpMsg encodeSnmpPdu(SnmpPdu pdu, int maxPktSize) 
        throws SnmpStatusException, SnmpTooBigException {
    
        return berFactory.encodeSnmpPdu(pdu,maxPktSize);    
    }

    /**
     * This method is called when a PDU is received. If the sender's name 
     * figures in the list of "denied" hosts then the method returns null 
     * (PDU rejected).
     */
    public SnmpPdu decodeSnmpPdu(SnmpMsg msg) throws SnmpStatusException {
    
        String from = msg.address.getHostName();
        java.lang.System.out.println("NOTE : SnmpPduFactoryImpl received " +
                                     "PDU from host " + from);
        for (int i = 0; i < hostNames.length; i++) {
            if (from.equals(hostNames[i])) {
                java.lang.System.out.println("NOTE : Pdu rejected ...");
                return null;
            }
        }    
        return berFactory.decodeSnmpPdu(msg);    
    }
    
    /**
     * This method is deprecated in JDMK 5.1 but we have to implement
     * it because it is still in the interface, though never called.
     * @deprecated see {@link com.sun.management.snmp.SnmpPduFactory}
     */
    public SnmpPduPacket decodePdu(SnmpMessage msg) 
        throws SnmpStatusException {
        return (SnmpPduPacket)decodeSnmpPdu(msg);
    }

    /**
     * This method is deprecated in JDMK 5.1 but we have to implement
     * it because it is still in the interface, though never called.
     * @deprecated see {@link com.sun.management.snmp.SnmpPduFactory}
     */
    public SnmpMessage encodePdu(SnmpPduPacket pdu, int maxPktSize) 
        throws SnmpStatusException, SnmpTooBigException {
        return (SnmpMessage)encodeSnmpPdu(pdu, maxPktSize);
    }
}
