package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfStructHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from provider-complex-jbi4corba-provider/src/main/resources/EchoComplex.idl
* Tuesday, September 11, 2007 3:26:56 PM CEST
*/

abstract public class ValueTypeOfStructHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/ValueTypeOfStruct:1.0";


  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct extract (org.omg.CORBA.Any a)
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
          _tcOf_members0 = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.type ();
          _members0[0] = new org.omg.CORBA.ValueMember ("data", 
              it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.id (), 
              _id, 
              "1.0", 
              _tcOf_members0, 
              null, 
              org.omg.CORBA.PUBLIC_MEMBER.value);
          __typeCode = org.omg.CORBA.ORB.init ().create_value_tc (_id, "ValueTypeOfStruct", org.omg.CORBA.VM_NONE.value, null, _members0);
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

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct read (org.omg.CORBA.portable.InputStream istream)
  {
    return (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct)((org.omg.CORBA_2_3.portable.InputStream) istream).read_value (id ());
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct value)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) ostream).write_value (value, id ());
  }


}
