package rttitypes;

/**
* rttitypes/EchoAnyExceptionHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class EchoAnyExceptionHolder implements org.omg.CORBA.portable.Streamable
{
  public rttitypes.EchoAnyException value = null;

  public EchoAnyExceptionHolder ()
  {
  }

  public EchoAnyExceptionHolder (rttitypes.EchoAnyException initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = rttitypes.EchoAnyExceptionHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    rttitypes.EchoAnyExceptionHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return rttitypes.EchoAnyExceptionHelper.type ();
  }

}
