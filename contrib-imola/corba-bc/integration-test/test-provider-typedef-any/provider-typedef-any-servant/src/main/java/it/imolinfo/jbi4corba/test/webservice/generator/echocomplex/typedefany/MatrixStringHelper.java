package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MatrixStringHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoled� 10 febbraio 2010 15.46.40 CET
*/

abstract public class MatrixStringHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/MatrixString:1.0";

  public static void insert (org.omg.CORBA.Any a, String[][] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static String[][] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_string_tc (0);
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (3, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (3, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.MatrixStringHelper.id (), "MatrixString", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static String[][] read (org.omg.CORBA.portable.InputStream istream)
  {
    String value[][] = null;
    value = new String[3][];
    for (int _o0 = 0;_o0 < (3); ++_o0)
    {
      value[_o0] = new String[3];
      for (int _o1 = 0;_o1 < (3); ++_o1)
      {
        value[_o0][_o1] = istream.read_string ();
      }
    }
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, String[][] value)
  {
    if (value.length != (3))
      throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    for (int _i0 = 0;_i0 < (3); ++_i0)
    {
      if (value[_i0].length != (3))
        throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
      for (int _i1 = 0;_i1 < (3); ++_i1)
      {
        ostream.write_string (value[_i0][_i1]);
      }
    }
  }

}