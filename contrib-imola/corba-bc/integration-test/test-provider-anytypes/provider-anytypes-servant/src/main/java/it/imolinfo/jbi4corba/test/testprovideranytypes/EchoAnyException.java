package it.imolinfo.jbi4corba.test.testprovideranytypes;


/**
* it/imolinfo/jbi4corba/test/testprovideranytypes/EchoAnyException.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-anytypes/provider-anytypes-jbi4corba-provider/src/main/resources/AnyTypes.idl
* luned� 2 febbraio 2009 12.26.09 EET
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