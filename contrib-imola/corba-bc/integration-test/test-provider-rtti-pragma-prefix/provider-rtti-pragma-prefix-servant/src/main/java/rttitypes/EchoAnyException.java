package rttitypes;


/**
* rttitypes/EchoAnyException.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class EchoAnyException extends org.omg.CORBA.UserException
{
  public org.omg.CORBA.Any reason = null;

  public EchoAnyException ()
  {
    super(EchoAnyExceptionHelper.id());
  } // ctor

  public EchoAnyException (org.omg.CORBA.Any _reason)
  {
    super(EchoAnyExceptionHelper.id());
    reason = _reason;
  } // ctor


  public EchoAnyException (String $reason, org.omg.CORBA.Any _reason)
  {
    super(EchoAnyExceptionHelper.id() + "  " + $reason);
    reason = _reason;
  } // ctor

} // class EchoAnyException