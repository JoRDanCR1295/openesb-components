/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.testprovidersimplemult;


import it.imolinfo.jbi4corba.test.testprovidersimplemult.Echo;
import it.imolinfo.jbi4corba.test.testprovidersimplemult.Echo2;
import it.imolinfo.jbi4corba.test.testprovidersimplemult.Echo2Helper;
import it.imolinfo.jbi4corba.test.testprovidersimplemult.EchoHelper;

import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;


/**
 *
 * @author Luca
 */
public class StartMultipleServant {

    /**
   * logger.
   */
  private static Logger log = Logger.getLogger(Echo.class.getName());
  //LogFactory.getLog(EchoImpl.class);
    //LogFactory.getLog(EchoImpl.class);
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
          is.close();
          log.info("launching orb with properties: " + props);

          ORB orb = ORB.init((String[]) null, props);

          // get reference to rootpoa & activate the POAManager
          POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
          rootpoa.the_POAManager().activate();

          //Fine parte comune   
          // create servant and register it with the ORB
          EchoImpl  echoImpl = new EchoImpl();
          EchoImpl2 echoImpl2 =new EchoImpl2();  
         
          log.info("EchoImpl ..." + echoImpl);
          log.info("EchoImpl2 ..." + echoImpl);

          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(echoImpl);
          org.omg.CORBA.Object ref2 = rootpoa.servant_to_reference(echoImpl2);
          Echo href = EchoHelper.narrow(ref);
          Echo2 href2 = Echo2Helper.narrow(ref2);
          

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
          String name = "Echo";
          NameComponent[] path = ncRef.to_name(name);
          ncRef.rebind(path, href);
          
          log.info("Echo - echoref rebindato: " + ncRef);

          log.info("EchoServer1 ready and waiting ...");
          
          String name2 = "Echo2";
          NameComponent[] path2 = ncRef.to_name(name2);
          ncRef.rebind(path2, href2);
          
          log.info("Echo2 - echoref rebindato: " + ncRef);

          log.info("EchoServer2 ready and waiting ...");

          // wait for invocations from clients
          orb.run();
        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
        }
        log.info("EchoServer1 Exiting ...");
        log.info("EchoServer2 Exiting ...");
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }
 
 
  
  
}
