package it.imolinfo.jbi4corba.test.testprovidercomplexinout;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexinout/StructOfValuetypeHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexInOut.idl
* Tuesday, February 5, 2008 11:09:37 AM GMT
*/

abstract public class StructOfValuetypeHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercomplexinout/StructOfValuetype/StructOfValuetype:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype extract (org.omg.CORBA.Any a)
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
          org.omg.CORBA.StructMember[] _members0 = new org.omg.CORBA.StructMember [1];
          org.omg.CORBA.TypeCode _tcOf_members0 = null;
          _tcOf_members0 = it.imolinfo.jbi4corba.test.testprovidercomplexinout.VTPrimiHelper.type ();
          _members0[0] = new org.omg.CORBA.StructMember (
            "data",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetypeHelper.id (), "StructOfValuetype", _members0);
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

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype value = new it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype ();
    value.data = it.imolinfo.jbi4corba.test.testprovidercomplexinout.VTPrimiHelper.read (istream);
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplexinout.StructOfValuetype value)
  {
    it.imolinfo.jbi4corba.test.testprovidercomplexinout.VTPrimiHelper.write (ostream, value.data);
  }

}
