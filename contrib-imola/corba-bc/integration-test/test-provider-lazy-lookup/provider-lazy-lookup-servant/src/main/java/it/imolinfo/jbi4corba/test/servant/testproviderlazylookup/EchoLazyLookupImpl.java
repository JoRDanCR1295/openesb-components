
package it.imolinfo.jbi4corba.test.servant.testproviderlazylookup;

import java.io.InputStream;
import java.util.Properties;
import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupNs;
import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupNsHelper;
import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaname;
import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbanameHelper;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;

import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

public class EchoLazyLookupImpl {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoLazyLookupImpl.class.getName());

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

                    if (daemon) {
                        startOrbd(props.getProperty("orbd.port"));
                        Thread.currentThread().sleep(2000);
                    }

                    bindEchoLazyLookupNs(orb);
                    log.info("EchoLazyLookupNs ready and waiting ...");

                    bindEchoLazyLookupCorbaname(orb);
                    log.info("EchoLazyLookupCorbaname ready and waiting ...");
                    
                    bindEchoLazyLookupCorbaloc(orb);
                    log.info("EchoLazyLookupCorbaloc ready and waiting ...");

                    bindEchoLazyLookupIOR(orb);
                    log.info("EchoLazyLookupIOR ready and waiting ...");
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
    private static void bindEchoLazyLookupNs(ORB orb)
            throws Exception {

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();

        // create servant and register it with the ORB
        InterfaceEchoLazyLookupNsImpl servantImpl = new InterfaceEchoLazyLookupNsImpl();

        // get object reference from the servant
        org.omg.CORBA.Object ref = rootpoa.servant_to_reference(servantImpl);
        InterfaceEchoLazyLookupNs href = InterfaceEchoLazyLookupNsHelper.narrow(ref);

        // get the root naming context
        // NameService invokes the name service
        org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

        // Use NamingContextExt which is part of the Interoperable
        // Naming Service (INS) specification.
        NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

        // bind the Object Reference in Naming
        String name = "EchoLazyLookupNs";
        NameComponent path[] = ncRef.to_name(name);
        ncRef.rebind(path, href);
        log.info("EchoLazyLookupNs - echoref rebindato: " + ncRef);
    }

    // DA VERIFICARE
    private static void bindEchoLazyLookupCorbaname(ORB orb)
            throws Exception {

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();

        // create servant and register it with the ORB
        InterfaceEchoLazyLookupCorbanameImpl servantImpl = new InterfaceEchoLazyLookupCorbanameImpl();

        // get object reference from the servant
        org.omg.CORBA.Object ref = rootpoa.servant_to_reference(servantImpl);
        InterfaceEchoLazyLookupCorbaname href = InterfaceEchoLazyLookupCorbanameHelper.narrow(ref);

        // get the root naming context
        // NameService invokes the name service
        org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

        // Use NamingContextExt which is part of the Interoperable
        // Naming Service (INS) specification.
        NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

        // bind the Object Reference in Naming
        String name = "EchoLazyLookupCorbaname";
        NameComponent path[] = ncRef.to_name(name);
        ncRef.rebind(path, href);
        log.info("EchoLazyLookupCorbaname - echoref rebindato: " + ncRef);
    }
    
    private static void bindEchoLazyLookupCorbaloc(ORB orb)
            throws Exception {

        // create servant and register it with the ORB
        InterfaceEchoLazyLookupCorbalocImpl servantImpl = new InterfaceEchoLazyLookupCorbalocImpl();

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();
        rootpoa.activate_object(servantImpl);

        // Publish the INS Service using 
        // orb.register_initial_reference( <ObjectKey>, <ObjectReference> 
        // NOTE: Sun private internal API, not part of CORBA 2.3.1.
        // May move as our compliance with OMG standards evolves.
        //((com.sun.corba.se.internal.Interceptors.PIORB) orb).
        ((com.sun.corba.se.impl.orb.ORBImpl) orb).register_initial_reference(
                "EchoLazyLookupCorbaloc", rootpoa.servant_to_reference(servantImpl));

        log.info("EchoLazyLookupCorbaloc Server is Ready...");


    }

    private static void bindEchoLazyLookupIOR(ORB orb)
            throws Exception {

        // get reference to rootpoa & activate the POAManager
        POA rootpoa = POAHelper.narrow(
                orb.resolve_initial_references("RootPOA"));
        rootpoa.the_POAManager().activate();

        // create servant and register it with the ORB
        InterfaceEchoLazyLookupIORImpl servantImpl = new InterfaceEchoLazyLookupIORImpl();

        // get object reference from the servant
        org.omg.CORBA.Object ref = rootpoa.servant_to_reference(servantImpl);
        //InterfaceEchoLazyLookupIOR href = InterfaceEchoLazyLookupIORHelper.narrow(ref);

        String ior = orb.object_to_string(ref);
          log.info("The servant EchoLazyLookupIOR is:" + ref);
          
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
                                                   "c:/EchoLazyLookupIOR.txt")));
          pw.println(ior);
          pw.flush();
          pw.close();

          log.info("Servant EchoLazyLookupIORImpl ready and waiting ...");
    }
}
