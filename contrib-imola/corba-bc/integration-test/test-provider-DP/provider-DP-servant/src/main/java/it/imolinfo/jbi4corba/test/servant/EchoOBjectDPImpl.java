

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package it.imolinfo.jbi4corba.test.servant;

import it.imolinfo.jbi4corba.test.ObjRef;
import it.imolinfo.jbi4corba.test.ObjRefHelper;
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
public class EchoOBjectDPImpl {

    /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoOBjectDPImpl.class.getName());
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

     
          ObjRefImpl echoImpl3 =new ObjRefImpl();


          log.info("ObjRefFactory ..." + echoImpl3);

          org.omg.CORBA.Object ref3 = rootpoa.servant_to_reference(echoImpl3);

          ObjRef href3 = ObjRefHelper.narrow(ref3);


          if (daemon) {
            startOrbd(props.getProperty("orbd.port"));
            Thread.currentThread().sleep(2000);
            
          }

          org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

          NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

          String name3 = "ObjRef";
          NameComponent[] path3 = ncRef.to_name(name3);
          ncRef.rebind(path3, href3);
          
          log.info("ObjRef - echoref rebindato: " + ncRef);

          log.info("ObjRef Server ready and waiting ...");
          
         
          orb.run();
        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
        }

        log.info("ObjRef Exiting ...");

        
        
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }
}