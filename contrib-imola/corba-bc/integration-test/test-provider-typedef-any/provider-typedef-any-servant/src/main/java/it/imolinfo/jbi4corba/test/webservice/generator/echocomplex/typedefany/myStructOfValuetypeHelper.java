package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myStructOfValuetypeHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.41 CET
*/

abstract public class myStructOfValuetypeHelper
{
  private static String  _id = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/myStructOfValuetype:1.0";

  public static void insert (org.omg.CORBA.Any a, it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetype that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetype extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetypeHelper.type ();
      __typeCode = org.omg.CORBA.ORB.init ().create_alias_tc (it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.myStructOfValuetypeHelper.id (), "myStructOfValuetype", __typeCode);
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetype read (org.omg.CORBA.portable.InputStream istream)
  {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetype value = null;
    value = it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetypeHelper.read (istream);
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetype value)
  {
    it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.StructOfValuetypeHelper.write (ostream, value);
  }

}
