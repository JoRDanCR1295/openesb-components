package it.imolinfo.jbi4corba.test.testprovidersimple;

/**
* it/imolinfo/jbi4corba/test/testprovidersimple/EchoInOutHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoInOut.idl
* Monday, February 4, 2008 2:11:58 PM GMT
*/

public final class EchoInOutHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidersimple.EchoInOut value = null;

  public EchoInOutHolder ()
  {
  }

  public EchoInOutHolder (it.imolinfo.jbi4corba.test.testprovidersimple.EchoInOut initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidersimple.EchoInOutHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidersimple.EchoInOutHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidersimple.EchoInOutHelper.type ();
  }

}