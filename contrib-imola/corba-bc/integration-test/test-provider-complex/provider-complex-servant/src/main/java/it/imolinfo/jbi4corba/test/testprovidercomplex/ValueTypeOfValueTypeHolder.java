package it.imolinfo.jbi4corba.test.testprovidercomplex;

/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfValueTypeHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from provider-complex-jbi4corba-provider/src/main/resources/EchoComplex.idl
* Tuesday, September 11, 2007 3:26:56 PM CEST
*/

public final class ValueTypeOfValueTypeHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType value = null;

  public ValueTypeOfValueTypeHolder ()
  {
  }

  public ValueTypeOfValueTypeHolder (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeHelper.type ();
  }

}
