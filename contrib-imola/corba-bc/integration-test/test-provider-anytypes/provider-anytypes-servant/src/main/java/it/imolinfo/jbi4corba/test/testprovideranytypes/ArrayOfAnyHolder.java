package it.imolinfo.jbi4corba.test.testprovideranytypes;


/**
* it/imolinfo/jbi4corba/test/testprovideranytypes/ArrayOfAnyHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-anytypes/provider-anytypes-jbi4corba-provider/src/main/resources/AnyTypes.idl
* luned� 2 febbraio 2009 12.26.09 EET
*/

public final class ArrayOfAnyHolder implements org.omg.CORBA.portable.Streamable
{
  public org.omg.CORBA.Any value[][] = null;

  public ArrayOfAnyHolder ()
  {
  }

  public ArrayOfAnyHolder (org.omg.CORBA.Any[][] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovideranytypes.ArrayOfAnyHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovideranytypes.ArrayOfAnyHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovideranytypes.ArrayOfAnyHelper.type ();
  }

}