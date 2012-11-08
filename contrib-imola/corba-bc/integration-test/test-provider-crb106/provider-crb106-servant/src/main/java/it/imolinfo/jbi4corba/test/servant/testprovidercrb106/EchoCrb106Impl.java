package it.imolinfo.jbi4corba.test.servant.testprovidercrb106;

import it.imolinfo.jbi4corba.test.testprovidercrb106.doubleparam.InterfaceDoubleParam;
import it.imolinfo.jbi4corba.test.testprovidercrb106.doubleparam.InterfaceDoubleParamHelper;
import it.imolinfo.jbi4corba.test.testprovidercrb106.singleparam.InterfaceSingleParam;
import it.imolinfo.jbi4corba.test.testprovidercrb106.singleparam.InterfaceSingleParamHelper;

import java.io.InputStream;
import java.util.Properties;

import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoCrb106Impl {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoCrb106Impl.class.getName());
                             

    /**
     * main first argument is the port (host is supposed to be localhost) second argument is
     * daemon=true/false optional, default false.
     * If daemon is true the servan starts as daemon, useful for integration tests
     */
    public static void main(String[] args) throws Exception {
        String propertyFile = args[0];
        
        boolean daemon=args.length>1?"daemon=true".equals(args[1]):false;               
        startCorbaServant(daemon,propertyFile);
        
    }

    private static void startOrbd(final String port) {
        Thread orbdThread = new Thread(new Runnable() {
            public void run() {
                log.info("starting orbd on port: " + port);
                com.sun.corba.se.impl.activation.ORBD.main(new String[]{"-ORBInitialPort",port,"-ORBInitialHost","localhost"});
            }
        });
        orbdThread.setDaemon(true);
        orbdThread.start();
        log.info("orbd launched");
    }

    private static void startCorbaServant(final boolean daemon,final String orbPropertyFile) {
        Thread servantThread = new Thread(new Runnable() {
            public void run() {
                try {
                    // create and initialize the ORB                    
                    log.info("loading orb.properties: " +orbPropertyFile);

                    InputStream is=this.getClass().getResourceAsStream("/"+orbPropertyFile);
                    log.info("input stream: "+is);
                    Properties props = new Properties();
                    props.load(is);

                    log.info("launching orb with properties: "+props);

                    ORB orb = ORB.init((String[])null, props);

                    if (daemon) {
                      startOrbd(props.getProperty("orbd.port"));
                      Thread.currentThread().sleep(2000);
                    }
               
                    bindEchoCrb106Single(orb);
                    //System.out.println("EchoCrb106Single ready and waiting ...");
                    log.info("EchoCrb106Single ready and waiting ...");                        

                    bindEchoCrb106Double(orb);
                    //System.out.println("EchoCrb106Double ready and waiting ...");
                    log.info("EchoCrb106Double ready and waiting ...");    

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

    private static void bindEchoCrb106Single(/*String [] args,*/ ORB orb)
      throws Exception {

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();

        // create servant and register it with the ORB
        InterfaceSingleParamImpl servantImpl = new InterfaceSingleParamImpl();

        // get object reference from the servant
        org.omg.CORBA.Object ref = rootpoa.servant_to_reference(servantImpl);
        InterfaceSingleParam href = InterfaceSingleParamHelper.narrow(ref);

        // get the root naming context
        // NameService invokes the name service
        org.omg.CORBA.Object objRef
            = orb.resolve_initial_references("NameService");

        // Use NamingContextExt which is part of the Interoperable
        // Naming Service (INS) specification.
        NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

        // bind the Object Reference in Naming
        String name = "EchoCrb106Single";
        NameComponent path[] = ncRef.to_name(name);
        ncRef.rebind(path, href);
        log.info("EchoCrb106Single - echoref rebindato: " + ncRef);
    }

    private static void bindEchoCrb106Double(/*String [] args,*/ ORB orb)
      throws Exception {

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();

        // create servant and register it with the ORB
        InterfaceDoubleParamImpl servantImpl = new InterfaceDoubleParamImpl();

        // get object reference from the servant
        org.omg.CORBA.Object ref = rootpoa.servant_to_reference(servantImpl);
        InterfaceDoubleParam href = InterfaceDoubleParamHelper.narrow(ref);

        // get the root naming context
        // NameService invokes the name service
        org.omg.CORBA.Object objRef
            = orb.resolve_initial_references("NameService");

        // Use NamingContextExt which is part of the Interoperable
        // Naming Service (INS) specification.
        NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

        // bind the Object Reference in Naming
        String name = "EchoCrb106Double";
        NameComponent path[] = ncRef.to_name(name);
        ncRef.rebind(path, href);
        log.info("EchoCrb106Double - echoref rebindato: " + ncRef);
    }
}
