package it.imolinfo.jbi4corba.test.testprovidersimple;


/**
* it/imolinfo/jbi4corba/test/testprovidersimple/EchoPOA.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/Echo.idl
* luned� 19 novembre 2007 17.01.34 CET
*/

public abstract class EchoPOA extends org.omg.PortableServer.Servant
 implements it.imolinfo.jbi4corba.test.testprovidersimple.EchoOperations, org.omg.CORBA.portable.InvokeHandler
{

  // Constructors

  private static java.util.Hashtable _methods = new java.util.Hashtable ();
  static
  {
    _methods.put ("echo", new java.lang.Integer (0));
  }

  public org.omg.CORBA.portable.OutputStream _invoke (String $method,
                                org.omg.CORBA.portable.InputStream in,
                                org.omg.CORBA.portable.ResponseHandler $rh)
  {
    org.omg.CORBA.portable.OutputStream out = null;
    java.lang.Integer __method = (java.lang.Integer)_methods.get ($method);
    if (__method == null)
      throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);

    switch (__method.intValue ())
    {
       case 0:  // it/imolinfo/jbi4corba/test/testprovidersimple/Echo/echo
       {
         String msg = in.read_string ();
         String $result = null;
         $result = this.echo (msg);
         out = $rh.createReply();
         out.write_string ($result);
         break;
       }

       default:
         throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/testprovidersimple/Echo:1.0"};

  public String[] _all_interfaces (org.omg.PortableServer.POA poa, byte[] objectId)
  {
    return (String[])__ids.clone ();
  }

  public Echo _this() 
  {
    return EchoHelper.narrow(
    super._this_object());
  }

  public Echo _this(org.omg.CORBA.ORB orb) 
  {
    return EchoHelper.narrow(
    super._this_object(orb));
  }


} // class EchoPOA
