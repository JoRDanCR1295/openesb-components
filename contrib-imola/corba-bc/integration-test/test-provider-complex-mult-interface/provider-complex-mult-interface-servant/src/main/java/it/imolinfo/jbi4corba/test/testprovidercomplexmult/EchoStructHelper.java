package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/EchoStructHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoMultipleComplex.idl
* mercoledý 19 novembre 2008 17.11.46 CET
*/

abstract public class EchoStructHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercomplexmult/EchoStruct:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct extract (org.omg.CORBA.Any a)
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
          org.omg.CORBA.StructMember[] _members0 = new org.omg.CORBA.StructMember [14];
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
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_wchar);
          _members0[2] = new org.omg.CORBA.StructMember (
            "fieldWChar",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_octet);
          _members0[3] = new org.omg.CORBA.StructMember (
            "fieldOctet",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().create_string_tc (0);
          _members0[4] = new org.omg.CORBA.StructMember (
            "fieldString",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().create_wstring_tc (0);
          _members0[5] = new org.omg.CORBA.StructMember (
            "fieldWString",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_short);
          _members0[6] = new org.omg.CORBA.StructMember (
            "fieldShort",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_ushort);
          _members0[7] = new org.omg.CORBA.StructMember (
            "fieldUnsignedShort",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[8] = new org.omg.CORBA.StructMember (
            "fieldLong",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_ulong);
          _members0[9] = new org.omg.CORBA.StructMember (
            "fieldUnsignedLong",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_longlong);
          _members0[10] = new org.omg.CORBA.StructMember (
            "fieldLongLong",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_ulonglong);
          _members0[11] = new org.omg.CORBA.StructMember (
            "fieldUnsignedLongLong",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_float);
          _members0[12] = new org.omg.CORBA.StructMember (
            "fieldFloat",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_double);
          _members0[13] = new org.omg.CORBA.StructMember (
            "fieldDouble",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStructHelper.id (), "EchoStruct", _members0);
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

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct value = new it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct ();
    value.fieldBoolean = istream.read_boolean ();
    value.fieldChar = istream.read_char ();
    value.fieldWChar = istream.read_wchar ();
    value.fieldOctet = istream.read_octet ();
    value.fieldString = istream.read_string ();
    value.fieldWString = istream.read_wstring ();
    value.fieldShort = istream.read_short ();
    value.fieldUnsignedShort = istream.read_ushort ();
    value.fieldLong = istream.read_long ();
    value.fieldUnsignedLong = istream.read_ulong ();
    value.fieldLongLong = istream.read_longlong ();
    value.fieldUnsignedLongLong = istream.read_ulonglong ();
    value.fieldFloat = istream.read_float ();
    value.fieldDouble = istream.read_double ();
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoStruct value)
  {
    ostream.write_boolean (value.fieldBoolean);
    ostream.write_char (value.fieldChar);
    ostream.write_wchar (value.fieldWChar);
    ostream.write_octet (value.fieldOctet);
    ostream.write_string (value.fieldString);
    ostream.write_wstring (value.fieldWString);
    ostream.write_short (value.fieldShort);
    ostream.write_ushort (value.fieldUnsignedShort);
    ostream.write_long (value.fieldLong);
    ostream.write_ulong (value.fieldUnsignedLong);
    ostream.write_longlong (value.fieldLongLong);
    ostream.write_ulonglong (value.fieldUnsignedLongLong);
    ostream.write_float (value.fieldFloat);
    ostream.write_double (value.fieldDouble);
  }

}
