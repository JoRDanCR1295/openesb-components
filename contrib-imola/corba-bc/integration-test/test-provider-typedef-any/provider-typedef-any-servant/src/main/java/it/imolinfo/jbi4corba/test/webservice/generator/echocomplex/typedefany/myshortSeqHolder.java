package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myshortSeqHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.40 CET
*/

public final class myshortSeqHolder implements org.omg.CORBA.portable.Streamable
{
  public short value[] = null;

  public myshortSeqHolder ()
  {
  }

  public myshortSeqHolder (short[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myshortSeqHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myshortSeqHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myshortSeqHelper.type ();
  }

}
