package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoComplexTypeDefAnyPOA.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.41 CET
*/


//==================================================
public abstract class EchoComplexTypeDefAnyPOA extends org.omg.PortableServer.Servant
 implements it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoComplexTypeDefAnyOperations, org.omg.CORBA.portable.InvokeHandler
{

  // Constructors

  private static java.util.Hashtable _methods = new java.util.Hashtable ();
  static
  {
    _methods.put ("echo", new java.lang.Integer (0));
    _methods.put ("echoinout", new java.lang.Integer (1));
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
       case 0:  // it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoComplexTypeDefAny/echo
       {
         org.omg.CORBA.Any msg = in.read_any ();
         org.omg.CORBA.Any $result = null;
         $result = this.echo (msg);
         out = $rh.createReply();
         out.write_any ($result);
         break;
       }

       case 1:  // it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoComplexTypeDefAny/echoinout
       {
         org.omg.CORBA.AnyHolder msg = new org.omg.CORBA.AnyHolder ();
         msg.value = in.read_any ();
         String $result = null;
         $result = this.echoinout (msg);
         out = $rh.createReply();
         out.write_string ($result);
         out.write_any (msg.value);
         break;
       }

       default:
         throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoComplexTypeDefAny:1.0"};

  public String[] _all_interfaces (org.omg.PortableServer.POA poa, byte[] objectId)
  {
    return (String[])__ids.clone ();
  }

  public EchoComplexTypeDefAny _this() 
  {
    return EchoComplexTypeDefAnyHelper.narrow(
    super._this_object());
  }

  public EchoComplexTypeDefAny _this(org.omg.CORBA.ORB orb) 
  {
    return EchoComplexTypeDefAnyHelper.narrow(
    super._this_object(orb));
  }


} // class EchoComplexTypeDefAnyPOA
