package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/EchoComplex2Operations.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from multipleInterface
* luned� 15 febbraio 2010 16.40.38 CET
*/

public interface EchoComplex2Operations 
{
  String echo2 (String msg);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoVT echoValueType2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoVT e);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLong echoValueBoxedTypePrimitive2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLong e);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence echoValueBoxedTypeComplex2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence e);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct echoStruct2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct es);
  int[] echoSequence2 (int[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence echoSequenceValueType2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence es);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct[] echoSequenceSeqEchoStruct2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence[] echoSequenceSeqMySequence2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct echoExceptionThrown2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplexException;
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct echoExceptionNotThrown2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplexException;

  // UNSUPPORTED : string echoAbstractValueType(in AFoo n);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfStruct echoStructOfStruct2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfStruct v);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimi echoVTPrimi2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimi v);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiSeq echoVTPrimiSeq2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiSeq v);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfValueType echoValueTypeOfValueType2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfValueType v);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfStruct echoValueTypeOfStruct2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.ValueTypeOfStruct v);
  it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfValuetype echoStructOfValuetype2 (it.imolinfo.jbi4corba.test.testprovidercomplexmult.StructOfValuetype v);
} // interface EchoComplex2Operations