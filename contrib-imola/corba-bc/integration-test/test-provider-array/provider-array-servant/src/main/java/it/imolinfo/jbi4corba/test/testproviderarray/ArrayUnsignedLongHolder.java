package it.imolinfo.jbi4corba.test.testproviderarray;


/**
* it/imolinfo/jbi4corba/test/testproviderarray/ArrayUnsignedLongHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoArray.idl
* venerd� 20 febbraio 2009 12.20.53 CET
*/

public final class ArrayUnsignedLongHolder implements org.omg.CORBA.portable.Streamable
{
  public int value[] = null;

  public ArrayUnsignedLongHolder ()
  {
  }

  public ArrayUnsignedLongHolder (int[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongHelper.type ();
  }

}