package rttitypes;


/**
* rttitypes/ComplexStruct1.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class ComplexStruct1 implements org.omg.CORBA.portable.IDLEntity
{
  public boolean fieldBoolean = false;
  public char fieldChar = (char)0;
  public rttitypes.SecondUnion fieldWChar = null;

  public ComplexStruct1 ()
  {
  } // ctor

  public ComplexStruct1 (boolean _fieldBoolean, char _fieldChar, rttitypes.SecondUnion _fieldWChar)
  {
    fieldBoolean = _fieldBoolean;
    fieldChar = _fieldChar;
    fieldWChar = _fieldWChar;
  } // ctor

} // class ComplexStruct1