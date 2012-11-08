/*
 * @(#)file      SecureAgent.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// JMX imports
//
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;

// JDMK imports
//
import com.sun.jdmk.comm.HtmlAdaptorServer;
import com.sun.management.comm.SnmpAdaptorServer;

/**
 * The SecureAgent class provides a simple example on how to use the
 * SnmpPduFactory interface for filtering incoming PDUs according to
 * the host that sends them. This agent uses the same MIB (RFC1213)
 * and the same SNMP port as the Agent.
 */

public class SecureAgent {

    static SnmpAdaptorServer snmpAdaptor = null;

    /**
     * The main method allows you to specify the following command-line
     * parameters:  
     *  1) the names of the hosts whose PDUs the agent will reject upon
     *     reception.
     */
    public static void main(String args[]) {

        MBeanServer server;
        ObjectName htmlObjName;
        ObjectName snmpObjName;
        ObjectName mibObjName;
        int htmlPort = 8082;
        int snmpPort = 161;

        try {
            server = MBeanServerFactory.createMBeanServer();
            String domain = server.getDefaultDomain();

            // Create and start the HTML adaptor.
            //
            htmlObjName =
                new ObjectName(domain +
                               ":class=HtmlAdaptorServer,protocol=html,port=" +
                               htmlPort);
            println("Adding HTML adaptor to MBean server with name \n\t" +
                    htmlObjName);
            println("NOTE: HTML Adaptor is bound on TCP port " + htmlPort);
            HtmlAdaptorServer htmlAdaptor = new HtmlAdaptorServer(htmlPort);
            server.registerMBean(htmlAdaptor, htmlObjName);
            htmlAdaptor.start();

            //
            // SNMP specific code:
            //

            // Create and start the SNMP adaptor.
            // Specify the port to use in the object name. 
            // If you want to use the standard port (161) comment out the
            // following line:
            // snmpPort = 8085;
            //
            snmpPort= 8085;
            snmpObjName =
                new ObjectName(domain +
                               ":class=SnmpAdaptorServer,protocol=snmp,port=" +
                               snmpPort);
            println("Adding SNMP adaptor to MBean server with name \n\t" +
                    snmpObjName);
            println("NOTE: SNMP Adaptor is bound on UDP port " + snmpPort);
            snmpAdaptor = new SnmpAdaptorServer(snmpPort);
            server.registerMBean(snmpAdaptor, snmpObjName);
            snmpAdaptor.start();

            // Use SnmpPduFactoryImpl class for SnmpPduFactory to filter
            // requests.
            // The agent will reject requests coming from the specified hosts.
            //
            String[] refusedHosts = new String[args.length];
            refusedHosts = args;
            snmpAdaptor.setPduFactory(new SnmpPduFactoryImpl(refusedHosts));

            // Create the MIB II (RFC 1213) and add it to the MBean server.
            //
            mibObjName = new ObjectName("snmp:class=RFC1213_MIB");
            println("Adding RFC1213-MIB to MBean server with name \n\t" +
                    mibObjName);

            // Create an instance of the customized MIB
            //
            RFC1213_MIB mib2 = new RFC1213_MIB_IMPL();
            server.registerMBean(mib2, mibObjName);

            // Bind the SNMP adaptor to the MIB in order to make the MIB
            // accessible through the SNMP protocol adaptor.
            // If this step is not performed, the MIB will still live in
            // the Java DMK agent:
            // its objects will be addressable through HTML but not SNMP.
            //
	    mib2.setSnmpAdaptor(snmpAdaptor);

            println("\n>> Press <Ctrl-C> if you want to stop this agent.");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static public SnmpAdaptorServer getSnmpAdaptor() {
        return snmpAdaptor;
    }

    /**
     * print/println stuff...
     */
    private final static void println(String msg) {
        java.lang.System.out.println(msg);
    }
    private final static void print(String msg) {
        java.lang.System.out.print(msg);
    }
}
