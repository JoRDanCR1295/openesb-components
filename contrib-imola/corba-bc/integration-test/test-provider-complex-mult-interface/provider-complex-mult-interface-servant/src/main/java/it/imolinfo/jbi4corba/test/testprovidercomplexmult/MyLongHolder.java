package it.imolinfo.jbi4corba.test.testprovidercomplexmult;

/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/MyLongHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoMultipleComplex.idl
* mercoledý 19 novembre 2008 17.11.46 CET
*/


// value type (boxed) - primitive
public final class MyLongHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLong value = null;

  public MyLongHolder ()
  {
  }

  public MyLongHolder (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLong initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLongHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLongHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexmult.MyLongHelper.type ();
  }

}
