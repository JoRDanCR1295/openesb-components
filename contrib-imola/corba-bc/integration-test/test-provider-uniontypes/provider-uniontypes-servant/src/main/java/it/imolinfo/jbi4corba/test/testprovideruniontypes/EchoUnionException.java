package it.imolinfo.jbi4corba.test.testprovideruniontypes;


/**
* it/imolinfo/jbi4corba/test/testprovideruniontypes/EchoUnionException.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-uniontypes/provider-uniontypes-jbi4corba-provider/src/main/resources/UnionTypes.idl
* luned� 19 gennaio 2009 9.19.47 EET
*/

public final class EchoUnionException extends org.omg.CORBA.UserException
{
  public it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion reason = null;

  public EchoUnionException ()
  {
    super(EchoUnionExceptionHelper.id());
  } // ctor

  public EchoUnionException (it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion _reason)
  {
    super(EchoUnionExceptionHelper.id());
    reason = _reason;
  } // ctor


  public EchoUnionException (String $reason, it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion _reason)
  {
    super(EchoUnionExceptionHelper.id() + "  " + $reason);
    reason = _reason;
  } // ctor

} // class EchoUnionException
