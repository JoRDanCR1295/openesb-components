package it.imolinfo.jbi4corba.test.testproviderlazylookup;

/**
* it/imolinfo/jbi4corba/test/testproviderlazylookup/InterfaceEchoLazyLookupIORHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoLazyLookup.idl
* marted� 15 gennaio 2008 15.09.47 CET
*/

public final class InterfaceEchoLazyLookupIORHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIOR value = null;

  public InterfaceEchoLazyLookupIORHolder ()
  {
  }

  public InterfaceEchoLazyLookupIORHolder (it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIOR initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIORHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIORHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIORHelper.type ();
  }

}
