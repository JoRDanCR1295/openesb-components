package it.imolinfo.jbi4corba.test.testproviderarray;


/**
* it/imolinfo/jbi4corba/test/testproviderarray/ArrayComplexStructHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoArray.idl
* venerd� 20 febbraio 2009 12.20.53 CET
*/

abstract public class ArrayComplexStructHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/testproviderarray/ArrayComplexStruct:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = it.imolinfo.jbi4corba.test.testproviderarray.ComplexStructHelper.type ();
      __typeCode = org.omg.CORBA.ORB.init ().create_array_tc (2, __typeCode );
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.testproviderarray.ArrayComplexStructHelper.id (), "ArrayComplexStruct", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct value[] = null;
    value = new it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[2];
    for (int _o0 = 0;_o0 < (2); ++_o0)
    {
      value[_o0] = it.imolinfo.jbi4corba.test.testproviderarray.ComplexStructHelper.read (istream);
    }
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] value)
  {
    if (value.length != (2))
      throw new org.omg.CORBA.MARSHAL (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    for (int _i0 = 0;_i0 < (2); ++_i0)
    {
      it.imolinfo.jbi4corba.test.testproviderarray.ComplexStructHelper.write (ostream, value[_i0]);
    }
  }

}
