package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MySequence.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.41 CET
*/


// value type (boxed) - complex
public abstract class MySequence implements org.omg.CORBA.portable.StreamableValue
{
  public int data[] = null;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequenceHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    int _len0 = istream.read_long ();
    this.data = new int[_len0];
    istream.read_long_array (this.data, 0, _len0);
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    ostream.write_long (this.data.length);
    ostream.write_long_array (this.data, 0, this.data.length);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MySequenceHelper.type ();
  }
} // class MySequence
