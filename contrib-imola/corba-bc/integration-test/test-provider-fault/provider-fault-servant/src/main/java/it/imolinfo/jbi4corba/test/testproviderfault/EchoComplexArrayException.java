package it.imolinfo.jbi4corba.test.testproviderfault;


/**
* it/imolinfo/jbi4corba/test/testproviderfault/EchoComplexArrayException.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from test.idl
* Thursday, February 19, 2009 6:25:12 PM CET
*/

public final class EchoComplexArrayException extends org.omg.CORBA.UserException
{
  public it.imolinfo.jbi4corba.test.testproviderfault.EchoStruct reasons[] = null;

  public EchoComplexArrayException ()
  {
    super(EchoComplexArrayExceptionHelper.id());
  } // ctor

  public EchoComplexArrayException (it.imolinfo.jbi4corba.test.testproviderfault.EchoStruct[] _reasons)
  {
    super(EchoComplexArrayExceptionHelper.id());
    reasons = _reasons;
  } // ctor


  public EchoComplexArrayException (String $reason, it.imolinfo.jbi4corba.test.testproviderfault.EchoStruct[] _reasons)
  {
    super(EchoComplexArrayExceptionHelper.id() + "  " + $reason);
    reasons = _reasons;
  } // ctor

} // class EchoComplexArrayException
