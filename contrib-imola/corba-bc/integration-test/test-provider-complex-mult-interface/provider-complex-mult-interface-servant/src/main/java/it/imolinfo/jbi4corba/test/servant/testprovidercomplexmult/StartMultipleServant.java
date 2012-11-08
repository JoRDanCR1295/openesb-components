/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.testprovidercomplexmult;



import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2Helper;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex3;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex3Helper;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplexHelper;
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
  private static Logger log = null;
 
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
          log = Logger.getLogger(EchoComplex.class.getName());
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

          // create servant and register it with the ORB
          EchoImpl echoImpl = new EchoImpl();
          log.info("EchoImpl ..." + echoImpl);
          EchoImpl2 echoImpl2 = new EchoImpl2();
          log.info("EchoImpl2 ..." + echoImpl2);
          EchoImpl3 echoImpl3 = new EchoImpl3();
          log.info("EchoImpl3 ..." + echoImpl3);
          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(echoImpl);
          EchoComplex href = EchoComplexHelper.narrow(ref);
          ref = rootpoa.servant_to_reference(echoImpl2);
          EchoComplex2 href2 = EchoComplex2Helper.narrow(ref);
          ref = rootpoa.servant_to_reference(echoImpl3);
          EchoComplex3 href3 = EchoComplex3Helper.narrow(ref);
          
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
          String name = "EchoComplex";
          NameComponent[] path = ncRef.to_name(name);
          ncRef.rebind(path, href);
          log.info("EchoComplex - echoref rebindato: " + ncRef);
          log.info("EchoComplexServer1 ready and waiting ...");
          
          // bind the Object Reference in Naming
          name = "EchoComplex2";
          path = ncRef.to_name(name);
          ncRef.rebind(path, href2);
          log.info("EchoComplex - echoref rebindato: " + ncRef);
          log.info("EchoComplexServer2 ready and waiting ...");
          
          
          // bind the Object Reference in Naming
           name = "EchoComplex3";
           path = ncRef.to_name(name);
           ncRef.rebind(path, href3);
           log.info("EchoComplex - echoref rebindato: " + ncRef );
           log.info("EchoComplexServer3 ready and waiting ...");
          

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
 
 
 
 
}
