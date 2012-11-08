/*
 * JUnitAgent.java
 * 
 * Created on Apr 19, 2007, 6:49:59 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
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
 * This class is used by JUnit test framework to control the lifecycle of SNMP agent.
 * This class duplicates the functionality of Agent.java
 * 
 * @author echou
 */
public class JUnitAgent {

    private MBeanServer server;
    private ObjectName snmpObjName;
    private ObjectName mibObjName;
    
    private SnmpAdaptorServer snmpAdaptor = null;
    private int snmpPort;
    
    public JUnitAgent(int snmpPort) {
        this.snmpPort = snmpPort;
    }
    
    public void init() throws Exception {
    
        server = MBeanServerFactory.createMBeanServer();
        String domain = server.getDefaultDomain();

        // Create and start the HTML adaptor.
        //
        /*
        htmlObjName = new ObjectName(domain +
               ":class=HtmlAdaptorServer,protocol=html,port=" + htmlPort);
        println("Adding HTML adaptor to MBean server with name \n\t" +
                htmlObjName);
        println("NOTE: HTML adaptor is bound on TCP port " + htmlPort);
        HtmlAdaptorServer htmlAdaptor = new HtmlAdaptorServer(htmlPort);
        server.registerMBean(htmlAdaptor, htmlObjName);
        htmlAdaptor.start();
         */

        //
        // SNMP specific code:
        //

        // Create and start the SNMP adaptor.
        snmpObjName = new ObjectName(domain + 
              ":class=SnmpAdaptorServer,protocol=snmp,port=" + snmpPort);
        println("Adding SNMP adaptor to MBean server with name \n\t" +
                snmpObjName);
        println("NOTE: SNMP Adaptor is bound on UDP port " + snmpPort);
        snmpAdaptor = new SnmpAdaptorServer(snmpPort);
        server.registerMBean(snmpAdaptor, snmpObjName);
        snmpAdaptor.start();

        // Send a coldStart SNMP Trap. 
        // Use port = snmpPort+1.
        //
        /*
        print("NOTE: Sending a coldStart SNMP trap" + 
              " to each destination defined in the ACL file...");
        snmpAdaptor.setTrapPort(new Integer(snmpPort+1));
        snmpAdaptor.snmpV1Trap(0, 0, null);
        println("Done.");
         */

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
        mib2.setSnmpAdaptor(snmpAdaptor);

        // Create a LinkTrapGenerator.
        // Specify the ifIndex to use in the object name.
        //
        /*
        String trapGeneratorClass = "LinkTrapGenerator";
        int ifIndex = 1;
        trapGeneratorObjName = new ObjectName("trapGenerator" + 
                          ":class=LinkTrapGenerator,ifIndex=" + ifIndex);
        println("Adding LinkTrapGenerator to MBean server with name \n\t" +
                trapGeneratorObjName);
        LinkTrapGenerator trapGenerator = new LinkTrapGenerator(nbTraps);
        server.registerMBean(trapGenerator, trapGeneratorObjName);

        println("\n>> Press <Enter> if you want to start sending traps.");
        println("   -or-");
        println(">> Press <Ctrl-C> if you want to stop this agent.");
        java.lang.System.in.read();

        trapGenerator.start();
         */
            
    }
    
    
    public void stop() throws Exception {
        snmpAdaptor.stop();
    }

    public static void main(String[] args) {
        try {
            JUnitAgent agent = new JUnitAgent(8085);
            agent.init();
        } catch (Exception e) {
            e.printStackTrace();
        }
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
