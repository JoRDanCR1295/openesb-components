package it.imolinfo.jbi4corba.test.testproviderlazylookup;


/**
* it/imolinfo/jbi4corba/test/testproviderlazylookup/InterfaceEchoLazyLookupCorbalocHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoLazyLookup.idl
* marted� 15 gennaio 2008 15.09.47 CET
*/

abstract public class InterfaceEchoLazyLookupCorbalocHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testproviderlazylookup/InterfaceEchoLazyLookupCorbaloc:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_interface_tc (it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbalocHelper.id (), "InterfaceEchoLazyLookupCorbaloc");
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc read (org.omg.CORBA.portable.InputStream istream)
  {
    return narrow (istream.read_Object (_InterfaceEchoLazyLookupCorbalocStub.class));
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc value)
  {
    ostream.write_Object ((org.omg.CORBA.Object) value);
  }

  public static it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc)
      return (it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc)obj;
    else if (!obj._is_a (id ()))
      throw new org.omg.CORBA.BAD_PARAM ();
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testproviderlazylookup._InterfaceEchoLazyLookupCorbalocStub stub = new it.imolinfo.jbi4corba.test.testproviderlazylookup._InterfaceEchoLazyLookupCorbalocStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

  public static it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc unchecked_narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc)
      return (it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbaloc)obj;
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testproviderlazylookup._InterfaceEchoLazyLookupCorbalocStub stub = new it.imolinfo.jbi4corba.test.testproviderlazylookup._InterfaceEchoLazyLookupCorbalocStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

}
