/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovidercomplexmult;


import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplexException;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplexPOA;


import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoVT;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLong;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfValuetype;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimi;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiSeq;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfStruct;
import it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfValueType;
import java.util.logging.Logger;

/**
 * This class is the corba servant used to manage the 'Echo.idl'
 */
public class EchoImpl extends EchoComplexPOA {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoImpl.class.getName());
  //LogFactory.getLog(EchoImpl.class);
  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the servan starts as daemon, useful for integration tests
   */
  
 

     public String echo(String arg0) {
        log.info("echo(String) - message received: " + arg0);
        return arg0+"Interface1";
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
        if(es.length>0){
            es[0].fieldString="Interface1";
        }
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
        es.fieldString="Interface1";
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
        v.internalStruct.fieldString="Interface1";
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