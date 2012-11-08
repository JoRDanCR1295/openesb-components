/*
 * @(#)file      StandAloneSnmpAgent.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.26
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// JDMK imports
//
import com.sun.management.comm.SnmpAdaptorServer;

/**
 * The example shows how to write a standalone SNMP agent using 
 * Java Dynamic Management Kit.
 * 
 * When calling the program, you can specify:
 *      - nb_traps: number of traps the SNMP agent will send.
 * If not specified, the agent will send traps continuously.
 *
 * In this example, the SNMP adaptor is started on port 8085, 
 * and the traps are sent to the port 8086, i.e a non standard ports
 * for SNMP. 
 * As such you do not need to be root to start the agent.
 */

public class StandAloneSnmpAgent {

    static SnmpAdaptorServer snmpAdaptor = null;

    /**
     * This variable defines the number of traps this agent has to send.
     * If not specified in the command line arguments, the traps will be
     * sent continuously.
     */
    private static int nbTraps = -1;
    
    /**
     * Main entry point.
     * When calling the program, you can specify:
     *  1) nb_traps: number of traps the SNMP agent will send.
     * If not specified, the agent will send traps continuously.
     */
    public static void main(String args[]) {
  
        // Parse the number of traps to be sent.
        //
        if ((args.length != 0) && (args.length != 1)) {
            usage();
            java.lang.System.exit(1);
        }
        else if (args.length == 1) {
            try {
                nbTraps = (new Integer(args[0])).intValue();
                if (nbTraps < 0) {
                    usage();
                    java.lang.System.exit(1);
                }
            } catch (java.lang.NumberFormatException e) {
                usage();
                java.lang.System.exit(1);
            }
        }

        try {

            // Bind the adaptor. By default port 161 is used.
            // For the example, we are going to use a on standard port.
            //
            // If you wish to use the standard SNMP ports, just comment 
            // out the following line:
            // port = 8085;
            //
            int port = 161;
            port = 8085;
            snmpAdaptor = new SnmpAdaptorServer(port);
            println("NOTE: SNMP Adaptor is bound on port " + port);

            // Start the adaptor.
            //
            snmpAdaptor.start();

            // Send a coldStart SNMP Trap.
            //
            print("NOTE: Sending a coldStart SNMP trap to" + 
                  " each destination defined in the ACL file...");
            snmpAdaptor.setTrapPort(new Integer(port+1));
            snmpAdaptor.snmpV1Trap(0, 0, null);
            println("Done.");

            // Create an instance of the customized  MIB.
            // The name of the generated class depends on the name of 
            // the module in the mib.
            //
            RFC1213_MIB mib2 = new RFC1213_MIB_IMPL();

            // Initialize the MIB and its associated metadata.
            //
            mib2.init();

            // Bind the MIB to the previously created adaptor.
            //
	    mib2.setSnmpAdaptor(snmpAdaptor);
	    
            // Create a LinkTrapGenerator.
            //
            int ifIndex = 1;
            LinkTrapGenerator trapGenerator = 
                new LinkTrapGenerator(ifIndex, nbTraps);
            
            println("\n>> Press <Enter>" +
                    " if you want to start sending traps.");
            println("   -or-");
            println(">> Press <Ctrl-C> if you want to stop this agent.");
            java.lang.System.in.read();
            
            trapGenerator.start();

            //
            // That's it : we have a SNMP agent running...
            //

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Return a reference to the SNMP adaptor server.
     */
    public static SnmpAdaptorServer getSnmpAdaptor() {
        return snmpAdaptor;
    }

    /**
     * Return usage of the program.
     */
    public static void  usage() {
        println("java StandAloneSnmpAgent <nb_traps>");
        println("where");
        println("\t-nb_traps: number of traps the SNMP agent" +
                " will send.");
        println("\t          " +
                "If not specified, the agent will send traps continuously.");
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
