package it.imolinfo.jbi4corba.test.testproviderlazylookup;

/**
* it/imolinfo/jbi4corba/test/testproviderlazylookup/InterfaceEchoLazyLookupCorbalocHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoLazyLookup.idl
* marted� 15 gennaio 2008 15.09.47 CET
*/

public final class InterfaceEchoLazyLookupCorbalocHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc value = null;

  public InterfaceEchoLazyLookupCorbalocHolder ()
  {
  }

  public InterfaceEchoLazyLookupCorbalocHolder (it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbalocHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbalocHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbalocHelper.type ();
  }

}
