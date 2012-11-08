/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercorbaname;

import it.imolinfo.jbi4corba.test.testprovidercorbaname.Echo;
import it.imolinfo.jbi4corba.test.testprovidercorbaname.EchoHelper;
import it.imolinfo.jbi4corba.test.testprovidercorbaname.EchoPOA;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoCorbanameImpl extends EchoPOA {

  /**
   * Logger
   */
  //private static Log log = LogFactory.getLog(CorbanameImpl.class);
  private static Logger log = Logger.getLogger(EchoCorbanameImpl.class.getName());

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

  protected static void startCorbaServant(/*String[] args*/final boolean daemon, final String orbPropertyFile) {
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

          // get reference to rootpoa & activate the POAManager
          POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
          rootpoa.the_POAManager().activate();

          // create servant and register it with the ORB
          EchoCorbanameImpl helloImpl = new EchoCorbanameImpl();
          log.info("EchoCorbanameImpl ..." + helloImpl);

          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
          Echo href = EchoHelper.narrow(ref);

          if (daemon) {
            startOrbd(props.getProperty("orbd.port"));
            Thread.currentThread().sleep(2000);
          }

          // get the root naming context
          // NameService invokes the name service
          org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

          // Use NamingContextExt which is part of the Interoperable
          // Naming Service (INS) specification.
          NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

          // bind the Object Reference in Naming
          String name = "EchoCorbanameImpl";
          NameComponent path[] = ncRef.to_name(name);
          ncRef.rebind(path, href);
          log.info("EchoCorbanameImpl rebindato: " + ncRef);

          log.info("EchoCorbanameServer ready and waiting ...");

          // wait for invocations from clients
          orb.run();

        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
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
  public String echo(String arg0) {
    log.info("CorbanameImpl - message received: " + arg0);
    return arg0;
  }
}
