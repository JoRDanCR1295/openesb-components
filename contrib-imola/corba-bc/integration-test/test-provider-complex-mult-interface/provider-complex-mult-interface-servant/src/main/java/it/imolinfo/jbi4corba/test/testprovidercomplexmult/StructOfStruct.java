package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/StructOfStruct.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoMultipleComplex.idl
* mercoledý 19 novembre 2008 17.11.46 CET
*/

public final class StructOfStruct implements org.omg.CORBA.portable.IDLEntity
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct internalStruct = null;

  public StructOfStruct ()
  {
  } // ctor

  public StructOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct _internalStruct)
  {
    internalStruct = _internalStruct;
  } // ctor

} // class StructOfStruct
