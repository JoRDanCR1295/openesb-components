package it.imolinfo.jbi4corba.test.testproviderarray;


/**
* it/imolinfo/jbi4corba/test/testproviderarray/ArrayDoubleHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoArray.idl
* venerd� 20 febbraio 2009 12.20.53 CET
*/

abstract public class ArrayDoubleHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testproviderarray/ArrayDouble:1.0";

  public static void insert (org.omg.CORBA.Any a, double[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static double[] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_double);
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (2, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.testproviderarray.ArrayDoubleHelper.id (), "ArrayDouble", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static double[] read (org.omg.CORBA.portable.InputStream istream)
  {
    double value[] = null;
    value = new double[2];
    for (int _o0 = 0;_o0 < (2); ++_o0)
    {
      value[_o0] = istream.read_double ();
    }
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, double[] value)
  {
    if (value.length != (2))
      throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    for (int _i0 = 0;_i0 < (2); ++_i0)
    {
      ostream.write_double (value[_i0]);
    }
  }

}