

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo2;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo2Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo3;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo3Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo4;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo4Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo5;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo5Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo6;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo6Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo7;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo7Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo8;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo8Helper;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoHelper;
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
public class EchoIntTypeImpl {

    /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoIntTypeImpl.class.getName());
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
          EchoImpl3 echoImpl3 =new EchoImpl3();
          EchoImpl4 echoImpl4 =new EchoImpl4();
          EchoImpl5 echoImpl5 =new EchoImpl5();
          EchoImpl6 echoImpl6 =new EchoImpl6();
          EchoImpl7 echoImpl7 =new EchoImpl7(); 
          EchoImpl8 echoImpl8 =new EchoImpl8();
         
          
         
          log.info("EchoImpl ..." + echoImpl);
          log.info("EchoImpl2 ..." + echoImpl);
          log.info("EchoImpl3 ..." + echoImpl);
          log.info("EchoImpl4 ..." + echoImpl);
          
          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(echoImpl);
          org.omg.CORBA.Object ref2 = rootpoa.servant_to_reference(echoImpl2);
          org.omg.CORBA.Object ref3 = rootpoa.servant_to_reference(echoImpl3);
          org.omg.CORBA.Object ref4 = rootpoa.servant_to_reference(echoImpl4);
          org.omg.CORBA.Object ref5 = rootpoa.servant_to_reference(echoImpl5);
          org.omg.CORBA.Object ref6 = rootpoa.servant_to_reference(echoImpl6);
          org.omg.CORBA.Object ref7 = rootpoa.servant_to_reference(echoImpl7);
          //org.omg.CORBA.Object ref8 = rootpoa.servant_to_reference(echoImpl8);
         
          Echo href = EchoHelper.narrow(ref);
          Echo2 href2 = Echo2Helper.narrow(ref2);
          Echo3 href3 = Echo3Helper.narrow(ref3);
          Echo4 href4 = Echo4Helper.narrow(ref4);
          Echo5 href5 = Echo5Helper.narrow(ref5);
          Echo6 href6 = Echo6Helper.narrow(ref6);
          Echo7 href7 = Echo7Helper.narrow(ref7);
          //Echo8 href8 = Echo8Helper.narrow(ref8);

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
          
          String name3 = "Echo3";
          NameComponent[] path3 = ncRef.to_name(name3);
          ncRef.rebind(path3, href3);
          
          log.info("Echo3 - echoref rebindato: " + ncRef);

          log.info("EchoServer3 ready and waiting ...");
          
          
          String name4 = "Echo4";
          NameComponent[] path4 = ncRef.to_name(name4);
          ncRef.rebind(path4, href4);
          
          log.info("Echo4 - echoref rebindato: " + ncRef);

          log.info("EchoServer4 ready and waiting ...");
          
          String name5 = "Echo5";
          NameComponent[] path5 = ncRef.to_name(name5);
          ncRef.rebind(path5, href5);
          
          log.info("Echo5 - echoref rebindato: " + ncRef);

          log.info("EchoServer5 ready and waiting ...");
          
          
          String name6 = "Echo6";
          NameComponent[] path6 = ncRef.to_name(name6);
          ncRef.rebind(path6, href6);
          
          log.info("Echo6 - echoref rebindato: " + ncRef);

          log.info("EchoServer6 ready and waiting ...");
          
           String name7 = "Echo7";
          NameComponent[] path7 = ncRef.to_name(name7);
          ncRef.rebind(path7, href7);
          
          log.info("Echo7 - echoref rebindato: " + ncRef);

          log.info("EchoServer7 ready and waiting ...");
           
          //String name8 = "Echo8";
          //NameComponent[] path8 = ncRef.to_name(name8);
          //ncRef.rebind(path8, href8);
          
          //log.info("Echo8 - echoref rebindato: " + ncRef);

          //log.info("EchoServer8 ready and waiting ...");

          // wait for invocations from clients
          orb.run();
        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
        }
        log.info("EchoServer1 Exiting ...");
        log.info("EchoServer2 Exiting ...");
        log.info("EchoServer3 Exiting ..."); 
        log.info("EchoServer4 Exiting ...");
        log.info("EchoServer5 Exiting ...");
        log.info("EchoServer6 Exiting ...");
        log.info("EchoServer7 Exiting ...");
        log.info("EchoServer8 Exiting ...");
        
        
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }
}