package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfStruct.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/

public abstract class ValueTypeOfStruct implements org.omg.CORBA.portable.StreamableValue
{
  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct data = null;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStructHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    this.data = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.read (istream);
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.write (ostream, this.data);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStructHelper.type ();
  }
} // class ValueTypeOfStruct
