package it.imolinfo.jbi4corba.test.testprovidercomplexinout;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexinout/SeqEchoStructHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexInOut.idl
* Tuesday, February 5, 2008 11:09:37 AM GMT
*/


// sequence + typedef
public final class SeqEchoStructHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoStruct value[] = null;

  public SeqEchoStructHolder ()
  {
  }

  public SeqEchoStructHolder (it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoStruct[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplexinout.SeqEchoStructHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexinout.SeqEchoStructHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexinout.SeqEchoStructHelper.type ();
  }

}