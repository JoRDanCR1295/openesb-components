package it.imolinfo.jbi4corba.test.testprovidercomplexinout;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplexinout/MyLongDefaultFactory.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoComplexInOut.idl
* Tuesday, February 5, 2008 11:09:37 AM GMT
*/


// value type (boxed) - primitive
public class MyLongDefaultFactory implements org.omg.CORBA.portable.ValueFactory {

  public java.io.Serializable read_value (org.omg.CORBA_2_3.portable.InputStream is)
  {
    return is.read_value(new MyLongImpl ());
  }
}
