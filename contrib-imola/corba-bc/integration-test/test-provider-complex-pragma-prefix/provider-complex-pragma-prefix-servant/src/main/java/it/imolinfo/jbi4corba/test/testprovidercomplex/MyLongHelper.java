package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/MyLongHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


// value type (boxed) - primitive
abstract public class MyLongHelper
{
  private static String  _id = "IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MyLong:1.0";


  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong extract (org.omg.CORBA.Any a)
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
          org.omg.CORBA.ValueMember[] _members0 = new org.omg.CORBA.ValueMember[1];
          org.omg.CORBA.TypeCode _tcOf_members0 = null;
          // ValueMember instance for data
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[0] = new org.omg.CORBA.ValueMember ("data", 
              "", 
              _id, 
              "", 
              _tcOf_members0, 
              null, 
              org.omg.CORBA.PUBLIC_MEMBER.value);
          __typeCode = org.omg.CORBA.ORB.init ().create_value_tc (_id, "MyLong", org.omg.CORBA.VM_NONE.value, null, _members0);
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

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong read (org.omg.CORBA.portable.InputStream istream)
  {
    return (it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong)((org.omg.CORBA_2_3.portable.InputStream) istream).read_value (id ());
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong value)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) ostream).write_value (value, id ());
  }


}
