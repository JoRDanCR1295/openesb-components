/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testproviderarray;

import it.imolinfo.jbi4corba.test.testproviderarray.ArrayInterface;
import it.imolinfo.jbi4corba.test.testproviderarray.ArrayInterfaceHelper;
import it.imolinfo.jbi4corba.test.testproviderarray.ArrayInterfacePOA;
import it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct;

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
 * This class is the corba servant used to manage the 'EchoArray.idl'
 */
public class EchoArrayImpl extends ArrayInterfacePOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoArrayImpl.class.getName());
                             //LogFactory.getLog(EchoEchoArrayImplImpl.class);
    
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
                    EchoArrayImpl helloImpl = new EchoArrayImpl();
                    log.info("EchoArrayImpl ..." + helloImpl);

                    // get object reference from the servant
                    org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
                    ArrayInterface href = ArrayInterfaceHelper.narrow(ref);
                    
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
                    String name = "EchoArray";
                    NameComponent[] path = ncRef.to_name(name);
                    ncRef.rebind(path, href);
                    log.info("EchoArrayImpl - echoref rebindato: " + ncRef);

                    log.info("EchoArrayServer ready and waiting ...");

                    // wait for invocations from clients
                    orb.run();
                } catch (Exception e) {
                    log.severe("ERROR: " + e);
                    e.printStackTrace(System.out);
                }
                log.info("EchoArrayServer Exiting ...");
            }
        });
        servantThread.setDaemon(daemon);
        servantThread.start();
    }    
    
    // ==========================================
    //                  The operations in the IDL
    // ==========================================


  public boolean[] echoArrayBoolean (boolean[] e) {
      
	  log.info("EchoArrayImpl.echoArrayBoolean - input=" + e);
		return e;
	}

  public char[] echoArrayChar (char[] e) {
	  log.info("EchoArrayImpl.echoArrayChar - input=" + e);
		return e;
	}

  public char[] echoArrayWChar (char[] e) {
	  log.info("EchoArrayImpl.echoArrayWChar - input=" + e);
		return e;
	}
/*
  public byte[] echoArrayOctet (byte[] e) {
	  log.info("EchoArrayImpl.echoArrayOctet - input=" + e.length);
          for (int i = 0; i < e.length; i++) {
              log.info("Element: " + e[i]);
          }
		return e;
	}*/

  public String[] echoArrayString (String[] e) {
	  log.info("EchoArrayImpl.echoArrayString - input=" + e);
		return e;
	}

  public String[] echoArrayWString (String[] e) {
	  log.info("EchoArrayImpl.echoArrayWString - input=" + e);
		return e;
	}

  public short[] echoArrayShort (short[] e) {
	  log.info("EchoArrayImpl.echoArrayShort - input=" + e);
		return e;
	}

  public short[] echoArrayUnsignedShort (short[] e) {
	  log.info("EchoArrayImpl.echoArrayUnsignedShort - input=" + e);
		return e;
	}

  public int[] echoArrayLong (int[] e) {
	  log.info("EchoArrayImpl.echoArrayLong - input=" + e);
		return e;
	}

  public int[] echoArrayUnsignedLong (int[] e) {
	  log.info("EchoArrayImpl.echoArrayUnsignedLong - input=" + e);
		return e;
	}

  public long[] echoArrayLongLong (long[] e) {
	  log.info("EchoArrayImpl.echoArrayLongLong - input=" + e);
		return e;
	}

  public long[] echoArrayUnsignedLongLong (long[] e) {
	  log.info("EchoArrayImpl.echoArrayUnsignedLongLong - input=" + e);
		return e;
	}

  public float[] echoArrayFloat (float[] e) {
	  log.info("EchoArrayImpl.echoArrayFloat - input=" + e);
		return e;
	}

  public double[] echoArrayDouble (double[] e) {
	  log.info("EchoArrayImpl.echoArrayDouble - input=" + e);
		return e;
	}

  public ComplexStruct[] echoArrayComplexStruct (ComplexStruct[] e) {
	  log.info("EchoArrayImpl.echoArrayComplexStruct - input=" + e);
		return e;
	}

  public String[][] echoMatrixString (String[][] e) {
	  log.info("EchoArrayImpl.echoMatrixString - input=" + e);
		return e;
	}

    public byte[] echoArrayOctet(byte[] e) {
        log.info("EchoArrayImpl.echoMatrixString - input=" + e);
		return e;
    }


}
