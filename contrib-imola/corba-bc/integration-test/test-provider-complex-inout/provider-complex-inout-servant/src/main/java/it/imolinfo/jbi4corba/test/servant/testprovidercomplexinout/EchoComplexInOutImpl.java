/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercomplexinout;

import it.imolinfo.jbi4corba.test.testprovidercomplexinout.*;
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
public class EchoComplexInOutImpl extends EchoComplexInOutPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(EchoComplexInOutImpl.class.getName());
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
                    EchoComplexInOutImpl helloImpl = new EchoComplexInOutImpl();
                    log.info("EchoComplexInOutImpl ..." + helloImpl);

                    // get object reference from the servant
                    org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
                    EchoComplexInOut href = EchoComplexInOutHelper.narrow(ref);
                    
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
                    String name = "EchoComplexInOut";
                    NameComponent[] path = ncRef.to_name(name);
                    ncRef.rebind(path, href);
                    log.info("EchoComplexInOutImpl - echoref rebindato: " + ncRef);

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

    public String echo(StringHolder arg0) {
        log.info("echo(String) - message received: " + arg0.value);
        String strBeforeChanged = arg0.value;
        arg0.value = arg0.value + " changed"; 
        return strBeforeChanged;
    }

    public int[] echoSequence(SeqLongHolder es) {        
        int length = es.value.length;
        int[] esint = new int[length];
        StringBuffer out =  new StringBuffer();
        for (int i = 0; i < length; i++){
            esint[i] = es.value[i];
            es.value[i] = es.value[i] + 1;
            out.append(esint[i] +",");
        }
        log.info("int[] echoSequence(int[] esint) - message received: " + es +" [" + out +"]");
        return esint;//es.value;
    }

    public EchoStruct[] echoSequenceSeqEchoStruct(SeqEchoStructHolder es) {
        log.info("EchoStruct[] echoSequenceSeqEchoStruct(EchoStruct[] es)" + " - message received: " + es);
        EchoStruct[] estruct = new EchoStruct[es.value.length];

        for (int i = 0; i < es.value.length; i++) {
            estruct[i] = changeAndReturnEchoStruct(es.value[i]);
        }
        
        return estruct;//es.value;
    }

    public MySequence[] echoSequenceSeqMySequence(SeqMySequenceHolder es) {
        log.info("MySequence[] echoSequenceSeqMySequence(MySequence[] es)" + " - message received: " + es);
        MySequence[] esequence = new MySequence[es.value.length];
        
        for (int i = 0; i < es.value.length; i++){           
            int lengthData = es.value[i].data.length;
            esequence[i] = new MySequenceImpl();
            esequence[i].data = new int[lengthData];
            for (int j = 0; j < lengthData; j++){
                esequence[i].data[j] = es.value[i].data[j];
                es.value[i].data[j] = es.value[i].data[j] +(int)1;
                log.info("esequence ["+i+","+j+"]: "+esequence[i].data[j]);
                log.info("es.value["+i+","+j+"]:" + es.value[i].data[j]);
            }
        }        
        return esequence;//es.value;
    }

    public MySequence echoSequenceValueType(MySequenceHolder es) {
        log.info("MySequence echoSequenceValueType(MySequence es)" + " - message received: " + es);
        
        MySequence esequence = new MySequenceImpl();
        int lengthData = es.value.data.length;
        esequence.data = new int[lengthData];
        for (int i = 0; i < lengthData; i++) {
            esequence.data[i] = es.value.data[i];
            es.value.data[i] = es.value.data[i] + (int) 1;
            log.info("esequence [" + i + "]"+ esequence.data[i]);
            log.info("es.value[" + i + "]" + es.value.data[i]);
        }

        return esequence;//es.value;
    }

    public EchoStruct echoStruct(EchoStructHolder es) {
        log.info("EchoStruct echoStruct(EchoStruct es)" + " - message received: " + es.value.fieldString);
        //EchoStruct estruct = es.value;
        EchoStruct estruct = changeAndReturnEchoStruct(es.value);
        return estruct;//es.value;
    }

    public MySequence echoValueBoxedTypeComplex(MySequenceHolder e) {
        log.info("MySequence echoValueBoxedTypeComplex(MySequence e)" + " - message received: " + e);
        
        MySequence esequence = new MySequenceImpl();
        int lengthData = e.value.data.length;
        esequence.data = new int[lengthData];
        for (int i = 0; i < lengthData; i++) {
            esequence.data[i] = e.value.data[i];
            e.value.data[i] = e.value.data[i] + (int) 1;
            log.info("esequence [" + i + "]"+ esequence.data[i]);
            log.info("es.value[" + i + "]" + e.value.data[i]);
        }

        return esequence;//e.value;
    }

    public MyLong echoValueBoxedTypePrimitive(MyLongHolder e) {
        log.info("MyLong echoValueBoxedTypePrimitive(MyLong e)" + " - message received: " + e);
        
        MyLong elong = new MyLongImpl();
        
        elong.data = e.value.data;
        e.value.data = e.value.data + (int)1;

        return elong;//e.value;
    }

    public EchoVT echoValueType(EchoVTHolder e) {
        log.info("EchoVT echoValueType(EchoVT e) - message received: " + e);
        
        EchoVT evt = new EchoVTImpl();
        evt.publicShort = e.value.publicShort;
        e.value.publicShort = (short) (e.value.publicShort + (short) 1);
        
        return evt;//e.value;
    }

    public StructOfStruct echoStructOfStruct(StructOfStructHolder v) {
        log.info("StructOfStruct echoStructOfStruct(StructOfStruct) " + "- message received: " + v);
        
        StructOfStruct vStructofStruct = new StructOfStruct();
        vStructofStruct.internalStruct = changeAndReturnEchoStruct(v.value.internalStruct);

        return vStructofStruct;//v.value;
    }

    public StructOfValuetype echoStructOfValuetype(StructOfValuetypeHolder v) {
        log.info("StructOfValuetype echoStructOfValuetype(StructOfValuetype) " + "- message received: " + v);
        
        StructOfValuetype vStructOfValuetype = new StructOfValuetype();
        
        vStructOfValuetype.data = this.changeAndReturnVTPrimi(v.value.data);
        
        return vStructOfValuetype;//v.value;
    }

    public VTPrimi echoVTPrimi(VTPrimiHolder v) {
        log.info("VTPrimi echoVTPrimi(VTPrimi) - message received: " + v);
        
        VTPrimi vprimi = changeAndReturnVTPrimi(v.value);
        
        return vprimi;//v.value;
    }

    public VTPrimiSeq echoVTPrimiSeq(VTPrimiSeqHolder v) {
        log.info("VTPrimiSeq echoVTPrimiSeq(VTPrimiSeq) - message received: " + v);
        
        VTPrimiSeq vprimiseq = new VTPrimiSeqImpl();
        int lengthVTPrimi = v.value.data.length;
        vprimiseq.data = new VTPrimiImpl[lengthVTPrimi];
        
        for (int i = 0; i < lengthVTPrimi; i++) {
            vprimiseq.data[i] = this.changeAndReturnVTPrimi(v.value.data[i]);
        }

        return vprimiseq;//v.value;
    }

    public ValueTypeOfStruct echoValueTypeOfStruct(ValueTypeOfStructHolder v) {
        log.info("ValueTypeOfStruct echoValueTypeOfStruct(ValueTypeOfStruct) " + "- message received: " + v);
        
        ValueTypeOfStruct vValuetype = new ValueTypeOfStructImpl();
        vValuetype.data = changeAndReturnEchoStruct(v.value.data);
       
        return vValuetype;//v.value;
    }

    public ValueTypeOfValueType echoValueTypeOfValueType(ValueTypeOfValueTypeHolder v) {
        log.info("ValueTypeOfValueType echoValueTypeOfValueType(" + "ValueTypeOfValueType) - message received: " + v);
        
        ValueTypeOfValueType vValueTypeOfValueType = new ValueTypeOfValueTypeImpl();
        vValueTypeOfValueType.data = changeAndReturnVTPrimi(v.value.data);
        
        return vValueTypeOfValueType;//v.value;
    }

    private VTPrimi changeAndReturnVTPrimi(VTPrimi v){
        
        VTPrimi vprimi = new VTPrimiImpl();
        
        vprimi.fieldBoolean = v.fieldBoolean;
        v.fieldBoolean = !v.fieldBoolean;
        
        
        vprimi.fieldChar = v.fieldChar;
        v.fieldChar = (char) (v.fieldChar + (int) 1);
                
        vprimi.fieldDouble = v.fieldDouble;
        v.fieldDouble = v.fieldDouble + (double)1;
        
        vprimi.fieldFloat = v.fieldFloat;
        v.fieldFloat = v.fieldFloat + (float)1;
        
        vprimi.fieldLong = v.fieldLong;
        v.fieldLong = v.fieldLong + (int) 1;
        
        vprimi.fieldLongLong = v.fieldLongLong;
        v.fieldLongLong = v.fieldLongLong +(int)1;
        
        vprimi.fieldOctet = v.fieldOctet;
        v.fieldOctet = (byte) (v.fieldOctet + (byte) 1);
        
        vprimi.fieldShort = v.fieldShort;
        v.fieldShort = (short) (v.fieldShort + (short) 1);
        
        vprimi.fieldString = v.fieldString;
        v.fieldString = v.fieldString+"-changed";
        
        vprimi.fieldUnsignedLong = v.fieldUnsignedLong;
        v.fieldUnsignedLong = v.fieldUnsignedLong + 1;
        
        vprimi.fieldUnsignedLongLong = v.fieldUnsignedLongLong;
        v.fieldUnsignedLongLong = v.fieldUnsignedLongLong+1;
        
        vprimi.fieldUnsignedShort = v.fieldUnsignedShort;
        v.fieldUnsignedShort = (short) (v.fieldUnsignedShort + (short) 1);
        
        vprimi.fieldWChar = v.fieldWChar;
        v.fieldWChar = (char) (v.fieldWChar + 1);
        
        vprimi.fieldWString = v.fieldWString;
        v.fieldWString = v.fieldWString+"-changed";
        
        return vprimi;
    }
    
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
        
        estruct.fieldOctet = es.fieldOctet;
        es.fieldOctet = (byte) (es.fieldOctet + (byte) 1);
        
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
    
}
