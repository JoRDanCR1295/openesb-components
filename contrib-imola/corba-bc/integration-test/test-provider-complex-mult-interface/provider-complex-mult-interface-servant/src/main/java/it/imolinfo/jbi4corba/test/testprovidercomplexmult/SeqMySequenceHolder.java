package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/SeqMySequenceHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoMultipleComplex.idl
* mercoled� 19 novembre 2008 17.11.46 CET
*/


// sequence + typedef
public final class SeqMySequenceHolder implements org.omg.CORBA.portable.Streamable
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence value[] = null;

  public SeqMySequenceHolder ()
  {
  }

  public SeqMySequenceHolder (it.imolinfo.jbi4corba.test.testprovidercomplexmult.MySequence[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = it.imolinfo.jbi4corba.test.testprovidercomplexmult.SeqMySequenceHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexmult.SeqMySequenceHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexmult.SeqMySequenceHelper.type ();
  }

}