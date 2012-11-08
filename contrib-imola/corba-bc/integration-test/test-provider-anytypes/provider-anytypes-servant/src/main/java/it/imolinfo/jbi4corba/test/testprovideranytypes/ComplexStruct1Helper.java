package it.imolinfo.jbi4corba.test.testprovideranytypes;


/**
* it/imolinfo/jbi4corba/test/testprovideranytypes/ComplexStruct1Helper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-anytypes/provider-anytypes-jbi4corba-provider/src/main/resources/AnyTypes.idl
* luned� 2 febbraio 2009 12.26.09 EET
*/

abstract public class ComplexStruct1Helper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovideranytypes/ComplexStruct1/ComplexStruct1:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  private static boolean __active = false;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      synchronized (org.omg.CORBA.TypeCode.class)
      {
        if (__typeCode == null)
        {
          if (__active)
          {
            return org.omg.CORBA.ORB.init().create_recursive_tc ( _id );
          }
          __active = true;
          org.omg.CORBA.StructMember[] _members0 = new org.omg.CORBA.StructMember [3];
          org.omg.CORBA.TypeCode _tcOf_members0 = null;
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_boolean);
          _members0[0] = new org.omg.CORBA.StructMember (
            "fieldBoolean",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_char);
          _members0[1] = new org.omg.CORBA.StructMember (
            "fieldChar",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_any);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().create_sequence_tc (0, _tcOf_members0);
          _members0[2] = new org.omg.CORBA.StructMember (
            "fieldAny",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1Helper.id (), "ComplexStruct1", _members0);
          __active = false;
        }
      }
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 value = new it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 ();
    value.fieldBoolean = istream.read_boolean ();
    value.fieldChar = istream.read_char ();
    int _len0 = istream.read_long ();
    value.fieldAny = new org.omg.CORBA.Any[_len0];
    for (int _o1 = 0;_o1 < value.fieldAny.length; ++_o1)
      value.fieldAny[_o1] = istream.read_any ();
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovideranytypes.ComplexStruct1 value)
  {
    ostream.write_boolean (value.fieldBoolean);
    ostream.write_char (value.fieldChar);
    ostream.write_long (value.fieldAny.length);
    for (int _i0 = 0;_i0 < value.fieldAny.length; ++_i0)
      ostream.write_any (value.fieldAny[_i0]);
  }

}
