package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoVT.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoled� 10 febbraio 2010 15.46.40 CET
*/


// value type
public abstract class EchoVT implements org.omg.CORBA.portable.StreamableValue
{
  public short publicShort = (short)0;
  protected int privateLong = (int)0;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoVTHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    this.publicShort = istream.read_short ();
    this.privateLong = istream.read_long ();
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    ostream.write_short (this.publicShort);
    ostream.write_long (this.privateLong);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoVTHelper.type ();
  }
} // class EchoVT