/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercorbaloc;

import it.imolinfo.jbi4corba.test.testprovidercorbaloc.EchoPOA;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoCorbalocImpl extends EchoPOA {

  private static Logger log = Logger.getLogger(EchoCorbalocImpl.class.getName());

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

          // Instantiate the Service Object that needs to be published
          // and associate it with RootPOA.
          EchoCorbalocImpl servant = new EchoCorbalocImpl();
          POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));

          rootPOA.the_POAManager().activate();
          rootPOA.activate_object(servant);
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
          ((com.sun.corba.se.impl.orb.ORBImpl) orb).register_initial_reference(
                  "EchoCorbalocImpl", rootPOA.servant_to_reference(servant));

          System.out.println("EchoCorbalocImpl Server is Ready...");

          // We are ready to receive requests
          orb.run();
        } catch (Exception e) {
          System.err.println("Error in setup : " + e);
        }
        log.info("EchoCorbaloc Server Exiting ...");
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }

  // ==========================================
  //                  The operations in the IDL
  // ==========================================
  public String echo(String arg0) {
    log.info("CorbalocImpl - message received: " + arg0);
    return arg0;
  }
}
