/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testproviderenum;

import it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum;
import it.imolinfo.jbi4corba.test.testproviderenum.foo;
import it.imolinfo.jbi4corba.test.testproviderenum.fooHelper;
import it.imolinfo.jbi4corba.test.testproviderenum.fooPOA;
import java.io.InputStream;
import java.util.Properties;

import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoEnumImpl extends fooPOA {

    private static Logger log = Logger.getLogger(EchoEnumImpl.class.getName());

    /**
     * main first argument is the port (host is supposed to be localhost) second argument is
     * daemon=true/false optional, default false.
     * If daemon is true the servan starts as daemon, useful for integration tests
     */
    public static void main(String args[]) {
        String propertyFile = args[0];

        boolean daemon = args.length > 1 ? "daemon=true".equals(args[1]) : false;
        startCorbaServant(daemon, propertyFile);
    }

    private static void startOrbd(final String port) {
        Thread orbdThread = new Thread(new Runnable() {
            public void run() {
                log.info("starting orbd on port: " + port);
                com.sun.corba.se.impl.activation.ORBD.main(new String[]{"-ORBInitialPort", port, "-ORBInitialHost", "localhost"});
            }
        });
        orbdThread.setDaemon(true);
        orbdThread.start();
        log.info("orbd launched");
    }

    protected static void startCorbaServant(final boolean daemon, final String orbPropertyFile) {
        Thread servantThread = new Thread(new Runnable() {
            public void run() {
                try {
                    // create and initialize the ORB                    
                    log.info("loading orb.properties: " + orbPropertyFile);

                    InputStream is = this.getClass().getResourceAsStream("/" + orbPropertyFile);
                    log.info("input stream: " + is);
                    Properties props = new Properties();
                    props.load(is);

                    log.info("launching orb with properties: " + props);

                    ORB orb = ORB.init((String[]) null, props);

                    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
                    rootPOA.the_POAManager().activate();        
                    
                    // Instantiate the Service Object that needs to be published
                    // and associate it with RootPOA.
                    EchoEnumImpl servant = new EchoEnumImpl();                   
                    
                                        // get object reference from the servant
                    org.omg.CORBA.Object ref = rootPOA.servant_to_reference(servant);
                    foo href = fooHelper.narrow(ref);                    
                    
                    // get the root naming context
                    // NameService invokes the name service
                    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

                    // Use NamingContextExt which is part of the Interoperable
                    // Naming Service (INS) specification.
                    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
                    
                                        // bind the Object Reference in Naming
                    String name = "EchoEnumImpl";
                    NameComponent[] path = ncRef.to_name(name);
                    ncRef.rebind(path, href);
                    log.info("EchoEnumImpl - echoref rebindato: " + ncRef);
/*
                    if (daemon) {
                        startOrbd(props.getProperty("orbd.port"));
                        Thread.currentThread().sleep(2000);
                    }
*/
                    // Publish the INS Service using 
                    // orb.register_initial_reference( <ObjectKey>, <ObjectReference> 
                    // NOTE: Sun private internal API, not part of CORBA 2.3.1.
                    // May move as our compliance with OMG standards evolves.
                    //((com.sun.corba.se.internal.Interceptors.PIORB) orb).
//                    if(orb instanceof org.jacorb.orb.ORB ){
//                        org.omg.CosNaming.NamingContextExt ncRef =  ((org.jacorb.orb.ORB) orb).(
//                            "EchoEnumImpl", rootPOA.servant_to_reference(servant));
//                    }else{
//                                                            
//                        ((com.sun.corba.se.impl.orb.ORBImpl) orb).register_initial_reference(
//                            "EchoEnumImpl", rootPOA.servant_to_reference(servant));
//                    }
//                    
//                                        // bind the Object Reference in Naming
//                    String name = "EchoArray";
//                    NameComponent[] path = ncRef.to_name(name);
//                    ncRef.rebind(path, href);
                    log.info("EchoEnumImpl - echoref rebindato: " + ncRef);

                    System.out.println("EchoEnumImpl Server is Ready...");

                    // We are ready to receive requests
                    orb.run();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.err.println("Error in setup : " + e);
                }
                log.info("EchoServer Exiting ...");
            }
        });
        servantThread.setDaemon(daemon);
        servantThread.start();
    }

    // ==========================================
    //                  The operations in the IDL
    // ==========================================
    public EchoComplexEnum echoEnum(EchoComplexEnum arg0) {
        log.info("EchoEnumImpl - message received: " + arg0);
        return arg0;
    }
}
