package it.imolinfo.jbi4corba.test.testprovidercomplex;

/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/EchoVTHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


// value type
public final class EchoVTHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT value = null;

  public EchoVTHolder ()
  {
  }

  public EchoVTHolder (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTHelper.type ();
  }

}
