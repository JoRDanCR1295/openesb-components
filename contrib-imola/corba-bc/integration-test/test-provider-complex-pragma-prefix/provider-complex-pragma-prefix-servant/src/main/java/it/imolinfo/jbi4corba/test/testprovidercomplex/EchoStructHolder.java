package it.imolinfo.jbi4corba.test.testprovidercomplex;

/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/EchoStructHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/

public final class EchoStructHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct value = null;

  public EchoStructHolder ()
  {
  }

  public EchoStructHolder (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.type ();
  }

}
