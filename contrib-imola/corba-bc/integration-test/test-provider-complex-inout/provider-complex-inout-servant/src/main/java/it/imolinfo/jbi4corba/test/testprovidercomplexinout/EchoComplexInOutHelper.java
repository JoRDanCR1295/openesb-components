package it.imolinfo.jbi4corba.test.testprovidercomplexinout;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexinout/EchoComplexInOutHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexInOut.idl
* Tuesday, February 5, 2008 11:09:37 AM GMT
*/


//==================================================
abstract public class EchoComplexInOutHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercomplexinout/EchoComplexInOut:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_interface_tc (it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOutHelper.id (), "EchoComplexInOut");
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut read (org.omg.CORBA.portable.InputStream istream)
  {
    return narrow (istream.read_Object (_EchoComplexInOutStub.class));
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut value)
  {
    ostream.write_Object ((org.omg.CORBA.Object) value);
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut)
      return (it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut)obj;
    else if (!obj._is_a (id ()))
      throw new org.omg.CORBA.BAD_PARAM ();
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplexinout._EchoComplexInOutStub stub = new it.imolinfo.jbi4corba.test.testprovidercomplexinout._EchoComplexInOutStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut unchecked_narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut)
      return (it.imolinfo.jbi4corba.test.testprovidercomplexinout.EchoComplexInOut)obj;
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplexinout._EchoComplexInOutStub stub = new it.imolinfo.jbi4corba.test.testprovidercomplexinout._EchoComplexInOutStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

}
