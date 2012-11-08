/*
 * @(#)file      MultipleAgentV3.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.9
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
import com.sun.management.snmp.SnmpEngineParameters;

// JDMK imports
//
import com.sun.jdmk.comm.HtmlAdaptorServer;
import com.sun.management.comm.SnmpV3AdaptorServer;
import com.sun.management.comm.SnmpAdaptorServer;

/**
 * The MultipleAgentV3 class provides a simple example on how to use the SNMP V3
 * protocol adaptor.
 * It highlights the fact that multiple SnmpAdaptorServer can be 
 * instantiated in an SNMP agent leading to a multiple SNMP V3 engine agent.
 *
 * <p>Such an agent simulate multiple SNMP agent.
 *
 * <p>Each adaptor has his own security configuration (jdmk.security file). 
 *
 * <p>A subset of MIB II (RFC1213) is implemented. The MIB is loaded and 
 * initialized. 
 * An instance of the mib is registered in both adaptors.
 * As such you can now see the MIB using your favorite SNMP manager, 
 * or you can use a web browser and see the MIB through the HTML adaptor.
 * 
 * When calling the program, you can specify:
 *      - nb_traps: number of traps the SNMP agent will send.
 * If not specified, the agent will send traps continuously.
 *
 * In this example, the SNMP adaptor is started on port 8085, 
 * and the traps are sent to the port 8086, i.e a non standard ports for
 * SNMP. Thus you do not need to be root to start the agent.
 */

public class MultipleAgentV3 {

    static SnmpV3AdaptorServer snmpAdaptor1 = null;
    static SnmpV3AdaptorServer snmpAdaptor2 = null;
    
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
        
        MBeanServer server;
        ObjectName htmlObjName;
        ObjectName snmpObjName1;
        ObjectName snmpObjName2;
        ObjectName mibObjName1;
        ObjectName mibObjName2;
        ObjectName trapGeneratorObjName;
        int htmlPort = 8082;

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
      
            // Create and start the first SNMP adaptor.
            // Specify the port to use in the constructor. 
            // If you want to use the standard port (161) comment out the 
            // following line:
            // snmpPort = 8085;
            //
            int snmpPort = 8085;
            snmpObjName1 = new ObjectName(domain + 
                   ":class=SnmpAdaptorServer,protocol=snmp,port=" + snmpPort);
            println("Adding SNMP adaptor to MBean server with name \n\t" +
                    snmpObjName1);
            println("NOTE: SNMP Adaptor is bound on UDP port " + snmpPort);
            snmpAdaptor1 = new SnmpV3AdaptorServer(snmpPort);
            server.registerMBean(snmpAdaptor1, snmpObjName1);
            snmpAdaptor1.registerUsmMib(server, null);
            snmpAdaptor1.start();

            // Send a coldStart SNMP Trap. 
            // Use port = snmpPort+1.
            //
            print("NOTE: Sending a coldStart SNMP trap"+
                  " to each destination defined in the ACL file...");
            snmpAdaptor1.setTrapPort(new Integer(snmpPort+1));
            snmpAdaptor1.snmpV1Trap(0, 0, null);
            snmpAdaptor1.enableSnmpV1V2SetRequest();
            println("Done.");
      
            
            // Create and start the second SNMP adaptor.
            // Specify the port to use in the constructor. 
            // If you want to use the standard port (161) comment out 
            // the following line:
            // snmpPort = 8087;
            //
            snmpPort = 8087;
            snmpObjName2 = new ObjectName(domain + 
                 ":class=SnmpAdaptorServer,protocol=snmp,port=" + snmpPort);
            println("Adding SNMP adaptor to MBean server with name \n\t" +
                    snmpObjName2);
            println("NOTE: SNMP Adaptor is bound on UDP port " + snmpPort);
            

            // Second adaptor instantiation.
            // The java property -Djdmk.security.file=jdmk.security is 
            // used for the first adaptor.
            // Another security file (if both adaptor are not sharing the 
            // same file) can be passed to the SnmpV3AdaptorServer passing 
            // it some SnmpEngineParameters.
            SnmpEngineParameters params = new SnmpEngineParameters();
            params.setSecurityFile("jdmk2.security");

            //Finaly create the adaptor passing it the parameters.
            snmpAdaptor2 = new SnmpV3AdaptorServer(params,
                                                   null,
                                                   null,
                                                   snmpPort,
                                                   null);
            server.registerMBean(snmpAdaptor2, snmpObjName2);
            snmpAdaptor2.registerUsmMib(server, null);
            snmpAdaptor2.start();

            // Send a coldStart SNMP Trap.
            // Use port = snmpPort+1.
            //
            print("NOTE: Sending a coldStart SNMP trap"+
                  " to each destination defined in the ACL file...");
            snmpAdaptor2.setTrapPort(new Integer(snmpPort+1));
            snmpAdaptor2.snmpV1Trap(0, 0, null);
            println("Done.");


            // Create the MIB II (RFC 1213) first instance and add it to 
            // the MBean server.
            //
            mibObjName1 = new ObjectName("snmp:class=RFC1213_MIB");
            println("Adding RFC1213-MIB to MBean server with name \n\t" +
                    mibObjName1);
            
            // Create the MIB II (RFC 1213) second instance and add it to 
            // the MBean server.
            //
            mibObjName2 = new ObjectName("snmp:class=RFC1213_MIB_2");
            println("Adding RFC1213-MIB to MBean server with name \n\t" +
                    mibObjName2);

            // Create 2 instances of the customized MIB
            //
            RFC1213_MIB mib2 = new RFC1213_MIB_IMPL();

            // The second mib name must be different. If not, a error 
            // is raised by the MBeanServer when registering the second MIB.
            // The 2 MBeans must have diffrent names.
            RFC1213_MIB mib2_2 = new RFC1213_MIB_IMPL("RFC1213_MIB_2");
            server.registerMBean(mib2, mibObjName1);
            server.registerMBean(mib2_2, mibObjName2);
            
            // Bind the SNMP adaptor to the MIB in order to make the MIB 
            // accessible through the SNMP protocol adaptor.
            // If this step is not performed, the MIB will still live in 
            // the Java DMK agent:
            // its objects will be addressable through HTML but not SNMP.
            // 
            // In SNMP V3 you can register MIBS under the scope of a context.
            // You register a mib under the scope of a context using 
            // setSnmpAdaptorName(snmpAdaptorName, context). For the sake 
            // of the example, the MIB is registered within the scope 
            // of TEST-CONTEXT.
            
	    mib2.setSnmpAdaptor(snmpAdaptor1, "TEST-CONTEXT");
	    mib2_2.setSnmpAdaptor(snmpAdaptor2, "TEST-CONTEXT");
            
            //
            // Multiple agent is ready to answer SNMP requests.
            //

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Return usage of the program.
     */
    public static void  usage() {
        println("java -Djdmk.security.file=jdmk.security MultipleAgentV3");
    }

    private final static void print(String msg) {
        java.lang.System.out.print(msg);
    }

    private final static void println(String msg) {
        java.lang.System.out.println(msg);
    }
}
