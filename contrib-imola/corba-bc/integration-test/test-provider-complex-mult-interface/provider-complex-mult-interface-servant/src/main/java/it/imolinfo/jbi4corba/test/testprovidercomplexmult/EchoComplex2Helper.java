package it.imolinfo.jbi4corba.test.testprovidercomplexmult;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexmult/EchoComplex2Helper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from multipleInterface
* luned� 15 febbraio 2010 16.40.38 CET
*/

abstract public class EchoComplex2Helper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testprovidercomplexmult/EchoComplex2:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_interface_tc (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2Helper.id (), "EchoComplex2");
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 read (org.omg.CORBA.portable.InputStream istream)
  {
    return narrow (istream.read_Object (_EchoComplex2Stub.class));
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 value)
  {
    ostream.write_Object ((org.omg.CORBA.Object) value);
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2)
      return (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2)obj;
    else if (!obj._is_a (id ()))
      throw new org.omg.CORBA.BAD_PARAM ();
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplexmult._EchoComplex2Stub stub = new it.imolinfo.jbi4corba.test.testprovidercomplexmult._EchoComplex2Stub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

  public static it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2 unchecked_narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2)
      return (it.imolinfo.jbi4corba.test.testprovidercomplexmult.EchoComplex2)obj;
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      it.imolinfo.jbi4corba.test.testprovidercomplexmult._EchoComplex2Stub stub = new it.imolinfo.jbi4corba.test.testprovidercomplexmult._EchoComplex2Stub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

}
