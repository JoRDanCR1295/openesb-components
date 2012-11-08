/*
 * @(#)file      SystemImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.26
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// Java imports
//
import java.net.InetAddress;
import java.net.UnknownHostException;

// JMX imports
//
import javax.management.MBeanServer;
import com.sun.management.snmp.SnmpStatusException;

// JDMK imports
//
import com.sun.management.snmp.agent.SnmpMib;


/**
 * The SystemImpl class provides an simple implementation of the "System" group.
 */

public class SystemImpl extends System {

    // MBean properties.
    //
    
    // Start up time of the agent.
    //
    private long startUpTime = 0;

    /**
     * Constructors.
     */
    public SystemImpl(SnmpMib myMib) {
    
        super(myMib);
        init();
    }
    
    public SystemImpl(SnmpMib myMib, MBeanServer server) {
    
        super(myMib, server);
        init();
    }
    
    private void init() {
        
        // Initialize the system description using some system properties.
        //
        try {
            SysDescr = java.lang.System.getProperty("os.name") + " " +
                java.lang.System.getProperty("os.arch") + " " +
                java.lang.System.getProperty("os.version");
        } catch(SecurityException e) {
            // Do not process the exception
        }
    
        // Initialize the system name using the hostname.
        //
        try {
            SysName = (InetAddress.getLocalHost()).getHostName();
        } catch(UnknownHostException e) {
            // Do not process the exception
        }
    
        // Initialize the system contact using some system properties.
        //
        try {
            SysContact = java.lang.System.getProperty("user.name");
        } catch(SecurityException e) {
            // Do not process the exception
        }
    
        // Initialize the set of services. Assume the agent only deals
        // with application layer.
        //
        SysServices = new Integer(72);

        // Initialize the location with a dummy string.
        //
        SysLocation = "Sample implementation of system group.";
    
        // For the sysUpTime, use the time the agent started...
        //
        startUpTime = java.lang.System.currentTimeMillis();
        
        SysTestGetter = "test getter 2";
    }
    
    public Long getSysUpTime() throws SnmpStatusException {
    
        long diff = java.lang.System.currentTimeMillis() - startUpTime;
        return new Long(diff);
    }
}
