package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplexPragmaPrefixHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


//==================================================
abstract public class EchoComplexPragmaPrefixHelper
{
  private static String  _id = "IDL:3yy4.321/it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplexPragmaPrefix:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_interface_tc (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefixHelper.id (), "EchoComplexPragmaPrefix");
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix read (org.omg.CORBA.portable.InputStream istream)
  {
    return narrow (istream.read_Object (_EchoComplexPragmaPrefixStub.class));
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix value)
  {
    ostream.write_Object ((org.omg.CORBA.Object) value);
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix)
      return (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix)obj;
    else if (!obj._is_a (id ()))
      throw new org.omg.CORBA.BAD_PARAM ();
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplex._EchoComplexPragmaPrefixStub stub = new it.imolinfo.jbi4corba.test.testprovidercomplex._EchoComplexPragmaPrefixStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix unchecked_narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix)
      return (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexPragmaPrefix)obj;
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplex._EchoComplexPragmaPrefixStub stub = new it.imolinfo.jbi4corba.test.testprovidercomplex._EchoComplexPragmaPrefixStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

}
