package rttitypes;


/**
* rttitypes/ComplexStruct2.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public final class ComplexStruct2 implements org.omg.CORBA.portable.IDLEntity
{
  public boolean fieldBoolean = false;
  public char fieldChar = (char)0;
  public org.omg.CORBA.Any fieldAny[] = null;

  public ComplexStruct2 ()
  {
  } // ctor

  public ComplexStruct2 (boolean _fieldBoolean, char _fieldChar, org.omg.CORBA.Any[] _fieldAny)
  {
    fieldBoolean = _fieldBoolean;
    fieldChar = _fieldChar;
    fieldAny = _fieldAny;
  } // ctor

} // class ComplexStruct2
