package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/VTPrimiDefaultFactory.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexPragmaPrefix.idl
* Wednesday, July 22, 2009 10:57:23 AM CEST
*/


// value type
public class VTPrimiDefaultFactory implements org.omg.CORBA.portable.ValueFactory {

  public java.io.Serializable read_value (org.omg.CORBA_2_3.portable.InputStream is)
  {
    return is.read_value(new VTPrimiImpl ());
  }
}
