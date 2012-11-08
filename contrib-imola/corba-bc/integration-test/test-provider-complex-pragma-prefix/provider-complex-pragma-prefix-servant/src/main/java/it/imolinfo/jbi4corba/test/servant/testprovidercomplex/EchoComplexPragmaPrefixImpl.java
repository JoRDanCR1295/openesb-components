/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercomplex;


import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexException;
import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix;
import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefixHelper;
import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefixPOA;
import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT;
import it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong;
import it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence;
import it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype;
import it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi;
import it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq;
import it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType;
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
 * This class is the corba servant used to manage the 'EchoComplex.idl'
 */
public class EchoComplexPragmaPrefixImpl extends EchoComplexPragmaPrefixPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoComplexPragmaPrefixImpl.class.getName());
                             //LogFactory.getLog(EchoComplexImpl.class);

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

                    // get reference to rootpoa & activate the POAManager
                    POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
                    rootpoa.the_POAManager().activate();

                    // create servant and register it with the ORB
                    EchoComplexPragmaPrefixImpl helloImpl = new EchoComplexPragmaPrefixImpl();
                    log.info("EchoComplexPragmaPrefixImpl ..." + helloImpl);

                    // get object reference from the servant
                    org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
                    EchoComplexPragmaPrefix href = EchoComplexPragmaPrefixHelper.narrow(ref);
                    
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
                    String name = "EchoComplexPragmaPrefix";
                    NameComponent[] path = ncRef.to_name(name);
                    ncRef.rebind(path, href);
                    log.info("EchoComplexImpl - echoref rebindato: " + ncRef);

                    log.info("EchoServer ready and waiting ...");

                    //registrazione value factory
                    org.omg.CORBA_2_3.ORB orb23=(org.omg.CORBA_2_3.ORB)orb;
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfStruct:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStructDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfValueType:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/VTPrimiSeq:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeqDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/EchoVT:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MyLong:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.MyLongDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/VTPrimi:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiDefaultFactory());
                    orb23.register_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0", new it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceDefaultFactory());
                  

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
        log.info("echo(String) - message received: " + arg0);
        return arg0;
    }

    public EchoStruct echoExceptionNotThrown(EchoStruct es) throws EchoComplexException {

        log.info("EchoStruct echoExceptionNotThrown(EchoStruct es)" + " - message received: " + es);

        return es;
    }

    public EchoStruct echoExceptionThrown(EchoStruct es) throws EchoComplexException {

        log.info("EchoStruct echoExceptionThrown(EchoStruct es)" + " - message received: " + es);

        throw new EchoComplexException(new EchoStruct[]{es});
    }

    public int[] echoSequence(int[] es) {
        StringBuffer out =  new StringBuffer();
        if (es != null) {
            for (int i = 0; i < es.length; i++) {            
                out.append(es[i] +" ");
            }
        }
        log.info("int[] echoSequence(int[] es) - message received: " + es +" [" + out +"]");

        return es;
    }

    public EchoStruct[] echoSequenceSeqEchoStruct(EchoStruct[] es) {
        log.info("EchoStruct[] echoSequenceSeqEchoStruct(EchoStruct[] es)" + " - message received: " + es);

        return es;
    }

    public MySequence[] echoSequenceSeqMySequence(MySequence[] es) {
        log.info("MySequence[] echoSequenceSeqMySequence(MySequence[] es)" + " - message received: " + es);

        return es;
    }

    public MySequence echoSequenceValueType(MySequence es) {
        log.info("MySequence echoSequenceValueType(MySequence es)" + " - message received: " + es);

        return es;
    }

    public EchoStruct echoStruct(EchoStruct es) {
        log.info("EchoStruct echoStruct(EchoStruct es)" + " - message received: " + es);

        return es;
    }

    public MySequence echoValueBoxedTypeComplex(MySequence e) {
        log.info("MySequence echoValueBoxedTypeComplex(MySequence e)" + " - message received: " + e);
        return e;
    }

    public MyLong echoValueBoxedTypePrimitive(MyLong e) {
        log.info("MyLong echoValueBoxedTypePrimitive(MyLong e)" + " - message received: " + e);
        return e;
    }

    public EchoVT echoValueType(EchoVT e) {
        log.info("EchoVT echoValueType(EchoVT e) - message received: " + e);

        return e;
    }

    public StructOfStruct echoStructOfStruct(StructOfStruct v) {
        log.info("StructOfStruct echoStructOfStruct(StructOfStruct) " + "- message received: " + v);
        return v;
    }

    public StructOfValuetype echoStructOfValuetype(StructOfValuetype v) {
        log.info("StructOfValuetype echoStructOfValuetype(StructOfValuetype) " + "- message received: " + v);
        return v;
    }

    public VTPrimi echoVTPrimi(VTPrimi v) {
        log.info("VTPrimi echoVTPrimi(VTPrimi) - message received: " + v);
        return v;
    }

    public VTPrimiSeq echoVTPrimiSeq(VTPrimiSeq v) {
        log.info("VTPrimiSeq echoVTPrimiSeq(VTPrimiSeq) - message received: " + v);
        return v;
    }

    public ValueTypeOfStruct echoValueTypeOfStruct(ValueTypeOfStruct v) {
        log.info("ValueTypeOfStruct echoValueTypeOfStruct(ValueTypeOfStruct) " + "- message received: " + v);
        return v;
    }

    public ValueTypeOfValueType echoValueTypeOfValueType(ValueTypeOfValueType v) {
        log.info("ValueTypeOfValueType echoValueTypeOfValueType(" + "ValueTypeOfValueType) - message received: " + v);
        return v;
    }


}