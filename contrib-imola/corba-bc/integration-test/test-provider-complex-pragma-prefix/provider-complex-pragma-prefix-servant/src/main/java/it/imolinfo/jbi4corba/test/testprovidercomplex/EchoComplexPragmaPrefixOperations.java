package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplexPragmaPrefixOperations.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


//==================================================
public interface EchoComplexPragmaPrefixOperations 
{
  String echo (String msg);
  it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT echoValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT e);
  it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong echoValueBoxedTypePrimitive (it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong e);
  it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence echoValueBoxedTypeComplex (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence e);
  it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es);
  int[] echoSequence (int[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence echoSequenceValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence es);
  it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct[] echoSequenceSeqEchoStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] echoSequenceSeqMySequence (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] es);
  it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoExceptionThrown (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexException;
  it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoExceptionNotThrown (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexException;

  // UNSUPPORTED : string echoAbstractValueType(in AFoo n);
  it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct echoStructOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct v);
  it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi echoVTPrimi (it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi v);
  it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq echoVTPrimiSeq (it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq v);
  it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType echoValueTypeOfValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType v);
  it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct echoValueTypeOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct v);
  it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype echoStructOfValuetype (it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype v);
} // interface EchoComplexPragmaPrefixOperations