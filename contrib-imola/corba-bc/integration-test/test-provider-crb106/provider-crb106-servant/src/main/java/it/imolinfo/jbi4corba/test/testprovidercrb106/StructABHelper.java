package it.imolinfo.jbi4corba.test.testprovidercrb106;


/**
* it/imolinfo/jbi4corba/test/testprovidercrb106/StructABHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoCrb106Single.idl
* gioved� 22 novembre 2007 16.36.59 CET
*/

abstract public class StructABHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercrb106/StructAB/StructAB:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB extract (org.omg.CORBA.Any a)
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
          org.omg.CORBA.StructMember[] _members0 = new org.omg.CORBA.StructMember [2];
          org.omg.CORBA.TypeCode _tcOf_members0 = null;
          _tcOf_members0 = it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeAHelper.type ();
          _members0[0] = new org.omg.CORBA.StructMember (
            "a",
            _tcOf_members0,
            null);
          _tcOf_members0 = it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeBHelper.type ();
          _members0[1] = new org.omg.CORBA.StructMember (
            "b",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (it.imolinfo.jbi4corba.test.testprovidercrb106.StructABHelper.id (), "StructAB", _members0);
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

  public static it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB value = new it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB ();
    value.a = it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeAHelper.read (istream);
    value.b = it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeBHelper.read (istream);
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB value)
  {
    it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeAHelper.write (ostream, value.a);
    it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeBHelper.write (ostream, value.b);
  }

}