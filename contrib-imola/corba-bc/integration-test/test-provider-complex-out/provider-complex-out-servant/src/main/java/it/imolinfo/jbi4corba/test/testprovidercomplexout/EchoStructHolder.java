package it.imolinfo.jbi4corba.test.testprovidercomplexout;

/**
* it/imolinfo/jbi4corba/test/testprovidercomplexout/EchoStructHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexOut.idl
* mercoled� 1 luglio 2009 12.18.32 CEST
*/

public final class EchoStructHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexout.EchoStruct value = null;

  public EchoStructHolder ()
  {
  }

  public EchoStructHolder (it.imolinfo.jbi4corba.test.testprovidercomplexout.EchoStruct initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplexout.EchoStructHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexout.EchoStructHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexout.EchoStructHelper.type ();
  }

}