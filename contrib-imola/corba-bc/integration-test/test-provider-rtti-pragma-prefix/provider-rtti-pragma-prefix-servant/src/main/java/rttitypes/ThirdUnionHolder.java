package rttitypes;

/**
* rttitypes/ThirdUnionHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class ThirdUnionHolder implements org.omg.CORBA.portable.Streamable
{
  public rttitypes.ThirdUnion value = null;

  public ThirdUnionHolder ()
  {
  }

  public ThirdUnionHolder (rttitypes.ThirdUnion initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = rttitypes.ThirdUnionHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    rttitypes.ThirdUnionHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return rttitypes.ThirdUnionHelper.type ();
  }

}
