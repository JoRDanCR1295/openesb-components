package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/mylonglongSeqHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.40 CET
*/

public final class mylonglongSeqHolder implements org.omg.CORBA.portable.Streamable
{
  public long value[] = null;

  public mylonglongSeqHolder ()
  {
  }

  public mylonglongSeqHolder (long[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mylonglongSeqHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mylonglongSeqHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.mylonglongSeqHelper.type ();
  }

}
