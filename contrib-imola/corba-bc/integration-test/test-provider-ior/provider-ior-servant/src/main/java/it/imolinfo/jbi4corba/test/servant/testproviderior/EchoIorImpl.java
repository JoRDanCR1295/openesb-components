/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testproviderior;

import it.imolinfo.jbi4corba.test.testproviderior.EchoPOA;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoIorImpl extends EchoPOA {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoIorImpl.class.getName());

  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the servan starts as daemon, useful for integration tests
   */
  public static void main(String[] args) throws Exception {
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

  private static void startCorbaServant(final boolean daemon, final String orbPropertyFile) {
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
          EchoIorImpl helloImpl = new EchoIorImpl();
          log.info("EchoIorImpl ..." + helloImpl);

          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
          //Echo echo = EchoHelper.narrow(ref);
/*
          if (daemon) {
            startOrbd(props.getProperty("orbd.port"));
            Thread.currentThread().sleep(2000);
          }
*/
          String ior = orb.object_to_string(ref);
          log.info("The servant IOR is:" + ref);
          
          /*
           * TODO da gestire path relativo nel Servant e nel WSDL del provider                        
          java.io.File test = java.io.File.createTempFile("test", null);
          String tempPath = (String)System.getProperties().get("java.io.tmpdir");
          String iorFileName = tempPath + java.io.File.separator + "EchoIorXBeanImpl.txt";
          System.err.println("***********************");
          System.err.println(iorFileName);
          System.err.println("***********************");
           */
          PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(
                                                   "c:/EchoIorXBeanImpl.txt")));
          pw.println(ior);
          pw.flush();
          pw.close();

          log.info("Servant EchoIorImpl ready and waiting ...");

          // wait for invocations from clients
          orb.run();
        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
        }
        log.info("EchoIor Server Exiting ...");
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }
  // ==========================================
  //                  The operations in the IDL
  // ==========================================  
  public String echo(String arg0) {
    log.info("ServantTestSimple - message received: " + arg0);
    return arg0;
  }
}
