package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myfloatArrHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.40 CET
*/

abstract public class myfloatArrHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myfloatArr:1.0";

  public static void insert (org.omg.CORBA.Any a, float[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static float[] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_float);
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (2, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myfloatArrHelper.id (), "myfloatArr", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static float[] read (org.omg.CORBA.portable.InputStream istream)
  {
    float value[] = null;
    value = new float[2];
    for (int _o0 = 0;_o0 < (2); ++_o0)
    {
      value[_o0] = istream.read_float ();
    }
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, float[] value)
  {
    if (value.length != (2))
      throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    for (int _i0 = 0;_i0 < (2); ++_i0)
    {
      ostream.write_float (value[_i0]);
    }
  }

}
