package rttitypes;


/**
* rttitypes/EchoComplexEnumHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/


//enum
abstract public class EchoComplexEnumHelper
{
  private static String  _id = "IDL:3hh4.123/rttitypes/EchoComplexEnum:1.0";

  public static void insert (org.omg.CORBA.Any a, rttitypes.EchoComplexEnum that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static rttitypes.EchoComplexEnum extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_enum_tc (rttitypes.EchoComplexEnumHelper.id (), "EchoComplexEnum", new String[] { "E1", "E2", "E3"} );
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static rttitypes.EchoComplexEnum read (org.omg.CORBA.portable.InputStream istream)
  {
    return rttitypes.EchoComplexEnum.from_int (istream.read_long ());
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, rttitypes.EchoComplexEnum value)
  {
    ostream.write_long (value.value ());
  }

}
