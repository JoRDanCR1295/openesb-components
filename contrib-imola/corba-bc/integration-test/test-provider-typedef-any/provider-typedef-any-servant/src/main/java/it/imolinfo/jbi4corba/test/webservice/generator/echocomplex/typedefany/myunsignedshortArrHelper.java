package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myunsignedshortArrHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.40 CET
*/

abstract public class myunsignedshortArrHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myunsignedshortArr:1.0";

  public static void insert (org.omg.CORBA.Any a, short[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static short[] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_ushort);
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (2, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myunsignedshortArrHelper.id (), "myunsignedshortArr", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static short[] read (org.omg.CORBA.portable.InputStream istream)
  {
    short value[] = null;
    value = new short[2];
    for (int _o0 = 0;_o0 < (2); ++_o0)
    {
      value[_o0] = istream.read_ushort ();
    }
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, short[] value)
  {
    if (value.length != (2))
      throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    for (int _i0 = 0;_i0 < (2); ++_i0)
    {
      ostream.write_ushort (value[_i0]);
    }
  }

}
