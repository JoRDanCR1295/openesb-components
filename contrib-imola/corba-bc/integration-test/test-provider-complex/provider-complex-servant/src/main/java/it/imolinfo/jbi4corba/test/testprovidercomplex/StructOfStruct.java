package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/StructOfStruct.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from provider-complex-jbi4corba-provider/src/main/resources/EchoComplex.idl
* Tuesday, September 11, 2007 3:26:56 PM CEST
*/

public final class StructOfStruct implements org.omg.CORBA.portable.IDLEntity
{
  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct internalStruct = null;

  public StructOfStruct ()
  {
  } // ctor

  public StructOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct _internalStruct)
  {
    internalStruct = _internalStruct;
  } // ctor

} // class StructOfStruct