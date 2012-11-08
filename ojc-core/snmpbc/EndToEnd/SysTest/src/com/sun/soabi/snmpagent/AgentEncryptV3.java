/*
 * @(#)file      AgentEncryptV3.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.13
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

import com.sun.management.snmp.SnmpEngine;
import com.sun.management.snmp.SnmpEngineId;
import com.sun.management.snmp.SnmpEngineParameters;

// JDMK imports
//
import com.sun.jdmk.comm.HtmlAdaptorServer;
import com.sun.management.comm.SnmpV3AdaptorServer;
import com.sun.management.comm.SnmpAdaptorServer;


/**
 * The Agent class provides a simple example on how to use the SNMP V3 
 * protocol adaptor, encryption being activated.
 * A subset of MIB II (RFC1213) is implemented. The MIB is loaded 
 * and initialized. 
 * As such you can now see the MIB using your favorite SNMP manager, 
 * or you can use a web browser and see the MIB through the HTML adaptor.
 * 
 * When calling the program, you can specify:
 *      - nb_traps: number of traps the SNMP agent will send.
 * If not specified, the agent will send traps continuously.
 *
 * In this example, the SNMP adaptor is started on port 8085, and the
 * traps are sent to the port 8086, i.e non standard ports for SNMP.
 * As such you do not need to be root to start the agent.
 */

public class AgentEncryptV3 {

    static SnmpV3AdaptorServer snmpAdaptor = null;
    
    /**
     * This variable defines the number of traps this agent has to send.
     * If not specified in the command line arguments, the traps will 
     * be sent continuously.
     */
    private static int nbTraps = -1;

    /**
     * Main entry point.
     * When calling the program, you can specify:
     *  1) nb_traps: number of traps the SNMP agent will send.
     * If not specified, the agent will send traps continuously.
     */
    public static void main(String args[]) {
        
        final MBeanServer server;
        final ObjectName htmlObjName;
        final ObjectName snmpObjName;
        final ObjectName mibObjName;
        final ObjectName trapGeneratorObjName;
        int htmlPort = 8082;
        int snmpPort = 161;

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
            server = MBeanServerFactory.createMBeanServer();
            String domain = server.getDefaultDomain();

            // Create and start the HTML adaptor.
            //
            htmlObjName = new ObjectName(domain + 
                 ":class=HtmlAdaptorServer,protocol=html,port=" + htmlPort);
            println("Adding HTML adaptor to MBean server with name \n\t" +
                    htmlObjName);
            println("NOTE: HTML adaptor is bound on TCP port " + htmlPort);
            HtmlAdaptorServer htmlAdaptor = new HtmlAdaptorServer(htmlPort);
            server.registerMBean(htmlAdaptor, htmlObjName);
            htmlAdaptor.start();
                  
            //
            // SNMP specific code:
            //
      
            // Create and start the SNMP adaptor.
            // Specify the port to use in the constructor. 
            // If you want to use the standard port (161) comment out 
            // the following line:
            //     snmpPort = 8085;
            //
            snmpPort = 8085;
            snmpObjName = new ObjectName(domain + 
                 ":class=SnmpAdaptorServer,protocol=snmp,port=" + snmpPort);
            println("Adding SNMP adaptor to MBean server with name \n\t" +
                    snmpObjName);
            println("NOTE: SNMP Adaptor is bound on UDP port " + snmpPort);


            // Handling the encryption activation.
            // By default, Java DMK 5.1 Agent API handles unsecured as well as 
            // authenticated SNMP requests. 
            // Privacy can also be optionally activated.
            // Some parameters need to be set in order to activate this level 
            // of security.  
            // This is done thanks to a new Java DMK class: SnmpEngineParemeters

            // Parameters are passed to the engine. The engine is configured
            // according to these parameters (eg : if encryption is activated,
            // DES algorithm will be loaded in the User Security Model and
            // privacy will be supported.
            
            // Usually, if no specific parameters have to be provided, 
            // you do not need to instantiate this SnmpEngineParameters 
            // class.
            // In this example, we show how to use the SnmpEngineParameters
            // in order to activate encryption.

            //First create parameters.
            SnmpEngineParameters parameters = new SnmpEngineParameters();

            //Then activate encryption
            parameters.activateEncryption();

            //Finaly create the adaptor V3 and pass the parameters.
            snmpAdaptor = new SnmpV3AdaptorServer(parameters,
                                                  null,
                                                  null,
                                                  snmpPort,
                                                  null);
            
            // Register the SNMP Adaptor in the MBean Server. This will
            // make it possible to manage the adaptor through HTTP/HTML
            //
            server.registerMBean(snmpAdaptor, snmpObjName);

            // Register the USM MIB. This makes it possible to view the
            // USM MIB through SNMP, and also through the MBeanServer
            // (server parameter != null).
            snmpAdaptor.registerUsmMib(server, null);

            // Start the adaptor.
            snmpAdaptor.start();

            // Send a coldStart SNMP Trap. 
            // Use port = snmpPort+1.
            //
            print("NOTE: Sending a coldStart SNMP trap to each " +
                  "destination defined in the ACL file...");

            snmpAdaptor.setTrapPort(new Integer(snmpPort+1));
            snmpAdaptor.snmpV1Trap(0, 0, null);
            println("Done.");
      
            // Create the MIB II (RFC 1213) and add it to the MBean server.
            //
            mibObjName= new ObjectName("snmp:class=RFC1213_MIB");
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
            // In SNMP V3 you can register MIBS under the scope of a 
            // context.
            // 
            // With Java DMK 4.2 you had two means to register a MIB with the
            // SNMP v1/v2 adaptor: 
            // 1) call myMib.setSnmpAdaptor(myAdaptor);
            // or 2) call myMib.setSnmpAdaptorName(myAdaptorName); - only 
            //       works for MBeanServer registered adaptors.
            //
            // Calling one of these two methods with an SnmpAdaptorServerV3
            // results in registering the MIB in the scope of the default
            // context (= no scope).
            // 
            // If you need to register a MIB in a specific context, you 
            // now have to call one of the two methods:
            //
            // 1) call myMib.setSnmpAdaptor(myAdaptor,"MyContext");
            // or 2) call myMib.setSnmpAdaptorName(myAdaptorName, 
            //                                     "MyContext"); - only 
            //       works for MBeanServer registered adaptors.
            //
            // For the sake of the example, the MIB is registered within 
            // the scope of "TEST-CONTEXT", using the first method.
            //
            mib2.setSnmpAdaptor(snmpAdaptor, "TEST-CONTEXT");

            // Create a LinkTrapGenerator.
            // Specify the ifIndex to use in the object name.
            //
            String trapGeneratorClass = "LinkTrapGenerator";
            int ifIndex = 1;
            trapGeneratorObjName = new ObjectName("trapGenerator" + 
                            ":class=LinkTrapGenerator,ifIndex=" + ifIndex);
            println("Adding LinkTrapGenerator to MBean server with name \n\t" +
                    trapGeneratorObjName);
            LinkTrapGenerator trapGenerator = new LinkTrapGenerator(nbTraps);
            server.registerMBean(trapGenerator, trapGeneratorObjName);

            println("\n>> Press <Enter> if you want to start sending traps." +
                    " SNMP V1 and SNMP V3 traps will be sent.");
            println("   -or-");
            println(">> Press <Ctrl-C> if you want to stop this agent.");
            java.lang.System.in.read();
            
            trapGenerator.start();
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Return a reference to the SNMP adaptor server.
     */
    static public SnmpAdaptorServer getSnmpAdaptor() {
        return snmpAdaptor;
    }
    
    /**
     * Return usage of the program.
     */
    public static void  usage() {
        println("java -Djdmk.security.file=jdmkencrypt.security AgentV3 " +
                "<nb_traps>");
        println("where");
        println("\t-nb_traps: number of traps the SNMP agent " +
                "will send.");
        println("\t          If not specified, the agent will send traps " +
                " continuously.");
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
