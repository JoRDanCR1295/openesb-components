package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/SeqMySequenceHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


// sequence + typedef
abstract public class SeqMySequenceHelper
{
  private static String  _id = "IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/SeqMySequence:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.type ();
      __typeCode = org.omg.CORBA.ORB.init ().create_sequence_tc (0, __typeCode);
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.testprovidercomplex.SeqMySequenceHelper.id (), "SeqMySequence", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence value[] = null;
    int _len0 = istream.read_long ();
    value = new it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[_len0];
    for (int _o1 = 0;_o1 < value.length; ++_o1)
      value[_o1] = it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.read (istream);
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] value)
  {
    ostream.write_long (value.length);
    for (int _i0 = 0;_i0 < value.length; ++_i0)
      it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.write (ostream, value[_i0]);
  }

}
