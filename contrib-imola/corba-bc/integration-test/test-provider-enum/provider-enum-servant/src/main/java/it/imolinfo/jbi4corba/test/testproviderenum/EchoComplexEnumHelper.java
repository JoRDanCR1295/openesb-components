package it.imolinfo.jbi4corba.test.testproviderenum;


/**
* it/imolinfo/jbi4corba/test/testproviderenum/EchoComplexEnumHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoEnum.idl
* marted� 27 novembre 2007 11.52.46 CET
*/

abstract public class EchoComplexEnumHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testproviderenum/EchoComplexEnum:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_enum_tc (it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnumHelper.id (), "EchoComplexEnum", new String[] { "E1", "E2", "E3"} );
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum read (org.omg.CORBA.portable.InputStream istream)
  {
    return it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum.from_int (istream.read_long ());
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum value)
  {
    ostream.write_long (value.value ());
  }

}
