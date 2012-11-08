 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;


/**
* it/imolinfo/jbi4corba/test/bytecodemanipulation/EchoOnewayInterfacePOA.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoOneWay.idl
* mercoled� 4 luglio 2007 15.49.23 CEST
*/

public abstract class EchoOnewayInterfacePOA extends org.omg.PortableServer.Servant
 implements it.imolinfo.jbi4corba.test.bytecodemanipulation.EchoOnewayInterfaceOperations, org.omg.CORBA.portable.InvokeHandler
{

  // Constructors

  private static java.util.Hashtable _methods = new java.util.Hashtable ();
  static
  {
    _methods.put ("echoOneway", new java.lang.Integer (0));
    _methods.put ("echoNotOneway", new java.lang.Integer (1));
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
       case 0:  // it/imolinfo/jbi4corba/test/bytecodemanipulation/EchoOnewayInterface/echoOneway
       {
         String message = in.read_string ();
         this.echoOneway (message);
         out = $rh.createReply();
         break;
       }

       case 1:  // it/imolinfo/jbi4corba/test/bytecodemanipulation/EchoOnewayInterface/echoNotOneway
       {
         String message = in.read_string ();
         this.echoNotOneway (message);
         out = $rh.createReply();
         break;
       }

       default:
         throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/bytecodemanipulation/EchoOnewayInterface:1.0"};

  public String[] _all_interfaces (org.omg.PortableServer.POA poa, byte[] objectId)
  {
    return (String[])__ids.clone ();
  }

  public EchoOnewayInterface _this() 
  {
    return EchoOnewayInterfaceHelper.narrow(
    super._this_object());
  }

  public EchoOnewayInterface _this(org.omg.CORBA.ORB orb) 
  {
    return EchoOnewayInterfaceHelper.narrow(
    super._this_object(orb));
  }


} // class EchoOnewayInterfacePOA
