package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/VTPrimiSeq.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoMultipleComplex.idl
* mercoledý 19 novembre 2008 17.11.46 CET
*/

public abstract class VTPrimiSeq implements org.omg.CORBA.portable.StreamableValue
{
  public it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimi data[] = null;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiSeqHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    int _len0 = istream.read_long ();
    this.data = new it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimi[_len0];
    for (int _o1 = 0;_o1 < this.data.length; ++_o1)
      this.data[_o1] = it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiHelper.read (istream);
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    ostream.write_long (this.data.length);
    for (int _i0 = 0;_i0 < this.data.length; ++_i0)
      it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiHelper.write (ostream, this.data[_i0]);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercomplexmult.VTPrimiSeqHelper.type ();
  }
} // class VTPrimiSeq
