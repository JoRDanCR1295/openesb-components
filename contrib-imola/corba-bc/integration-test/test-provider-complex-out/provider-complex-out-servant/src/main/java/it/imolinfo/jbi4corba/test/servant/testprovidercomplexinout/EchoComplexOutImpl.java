/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercomplexinout;

import it.imolinfo.jbi4corba.test.testprovidercomplexout.*;
import org.omg.CORBA.*;

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
public class EchoComplexOutImpl extends EchoComplexOutPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoComplexOutImpl.class.getName());
                             //LogFactory.getLog(EchoComplexInOutImpl.class);

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
                    EchoComplexOutImpl helloImpl = new EchoComplexOutImpl();
                    log.info("EchoComplexOutImpl ..." + helloImpl);

                    // get object reference from the servant
                    org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
                    EchoComplexOut href = EchoComplexOutHelper.narrow(ref);
                    
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
                    String name = "EchoComplexOut";
                    NameComponent[] path = ncRef.to_name(name);
                    ncRef.rebind(path, href);
                    log.info("EchoComplexOutImpl - echoref rebindato: " + ncRef);

                    log.info("EchoServer ready and waiting ...");

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


    
    private EchoStruct changeAndReturnEchoStruct(EchoStruct es){
        
        EchoStruct estruct = new EchoStruct();
        
        estruct.fieldBoolean = es.fieldBoolean;
        es.fieldBoolean = !es.fieldBoolean;
        
        estruct.fieldLong = es.fieldLong;
        es.fieldLong = es.fieldLong + (int)1;
        
        estruct.fieldChar = es.fieldChar;
        es.fieldChar = (char) (es.fieldChar + (int) 1);
        
        estruct.fieldWChar = es.fieldWChar;
        es.fieldWChar = (char) (es.fieldWChar + (int) 1);
        
        
        estruct.fieldString = es.fieldString;
        es.fieldString = es.fieldString + "-changed";
        
        estruct.fieldWString = es.fieldWString;
        es.fieldWString = es.fieldWString + "-changed";
                
        estruct.fieldShort = es.fieldShort;
        es.fieldShort = (short) (es.fieldShort + (short) 1);
        
        estruct.fieldUnsignedShort = es.fieldUnsignedShort;
        es.fieldUnsignedShort = (short) (es.fieldUnsignedShort + (short) 1);
        
        estruct.fieldUnsignedLong = es.fieldUnsignedLong;
        es.fieldUnsignedLong = es.fieldUnsignedLong + (int)1;
        
        estruct.fieldLongLong = es.fieldLongLong;
        es.fieldLongLong = es.fieldLongLong + (int)1;
        
        estruct.fieldUnsignedLongLong = es.fieldUnsignedLongLong;
        es.fieldUnsignedLongLong = es.fieldUnsignedLongLong + (int)1;
        
        estruct.fieldFloat = es.fieldFloat;
        es.fieldFloat = es.fieldFloat + (float)1;
        
        estruct.fieldDouble = es.fieldDouble;
        es.fieldDouble = es.fieldDouble + (double)1;
        
        return estruct;
    }

    public EchoStruct echo(EchoStruct input, EchoStructHolder output) {
        //System.out.println("Input"+input.toString());
        //System.out.println("Output Boolean"+output.value.fieldBoolean);
        EchoStruct result=new EchoStruct();
        result.fieldBoolean=true;
        result.fieldChar=2;
        result.fieldDouble=2;
        result.fieldFloat=2;
        result.fieldLong=2;
        result.fieldLongLong=2;
        result.fieldShort=2;
        result.fieldString="Risposta";
        result.fieldUnsignedLong=2;
        result.fieldUnsignedLongLong=2;
        result.fieldWChar=2;
        result.fieldWString="ciaooa";
        output.value=result;
        return result;
    }

    
}
