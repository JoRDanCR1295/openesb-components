package rttitypes;

/**
* rttitypes/ComplexStruct1Holder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class ComplexStruct1Holder implements org.omg.CORBA.portable.Streamable
{
  public rttitypes.ComplexStruct1 value = null;

  public ComplexStruct1Holder ()
  {
  }

  public ComplexStruct1Holder (rttitypes.ComplexStruct1 initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = rttitypes.ComplexStruct1Helper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    rttitypes.ComplexStruct1Helper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return rttitypes.ComplexStruct1Helper.type ();
  }

}
