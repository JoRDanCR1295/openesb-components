package it.imolinfo.jbi4corba.test.testprovidercomplexinout;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexinout/ValueTypeOfStruct.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexInOut.idl
* Tuesday, February 5, 2008 11:09:37 AM GMT
*/

public abstract class ValueTypeOfStruct implements org.omg.CORBA.portable.StreamableValue
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoStruct data = null;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.testprovidercomplexinout.ValueTypeOfStructHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    this.data = it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoStructHelper.read (istream);
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoStructHelper.write (ostream, this.data);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexinout.ValueTypeOfStructHelper.type ();
  }
} // class ValueTypeOfStruct