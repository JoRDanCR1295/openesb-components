package it.imolinfo.jbi4corba.test.testproviderarray;


/**
* it/imolinfo/jbi4corba/test/testproviderarray/ArrayUnsignedLongLongHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoArray.idl
* venerd� 20 febbraio 2009 12.20.53 CET
*/

public final class ArrayUnsignedLongLongHolder implements org.omg.CORBA.portable.Streamable
{
  public long value[] = null;

  public ArrayUnsignedLongLongHolder ()
  {
  }

  public ArrayUnsignedLongLongHolder (long[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongLongHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongLongHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongLongHelper.type ();
  }

}
