package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoStruct.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.41 CET
*/

public final class EchoStruct implements org.omg.CORBA.portable.IDLEntity
{
  public boolean fieldBoolean = false;
  public char fieldChar = (char)0;
  public char fieldWChar = (char)0;

  // http://jira.codehaus.org/browse/XFIRE-462
  public byte fieldOctet = (byte)0;
  public String fieldString = null;
  public String fieldWString = null;
  public short fieldShort = (short)0;
  public short fieldUnsignedShort = (short)0;
  public int fieldLong = (int)0;
  public int fieldUnsignedLong = (int)0;
  public long fieldLongLong = (long)0;
  public long fieldUnsignedLongLong = (long)0;
  public float fieldFloat = (float)0;
  public double fieldDouble = (double)0;
  public org.omg.CORBA.Any fieldAny = null;

  public EchoStruct ()
  {
  } // ctor

  public EchoStruct (boolean _fieldBoolean, char _fieldChar, char _fieldWChar, byte _fieldOctet, String _fieldString, String _fieldWString, short _fieldShort, short _fieldUnsignedShort, int _fieldLong, int _fieldUnsignedLong, long _fieldLongLong, long _fieldUnsignedLongLong, float _fieldFloat, double _fieldDouble, org.omg.CORBA.Any _fieldAny)
  {
    fieldBoolean = _fieldBoolean;
    fieldChar = _fieldChar;
    fieldWChar = _fieldWChar;
    fieldOctet = _fieldOctet;
    fieldString = _fieldString;
    fieldWString = _fieldWString;
    fieldShort = _fieldShort;
    fieldUnsignedShort = _fieldUnsignedShort;
    fieldLong = _fieldLong;
    fieldUnsignedLong = _fieldUnsignedLong;
    fieldLongLong = _fieldLongLong;
    fieldUnsignedLongLong = _fieldUnsignedLongLong;
    fieldFloat = _fieldFloat;
    fieldDouble = _fieldDouble;
    fieldAny = _fieldAny;
  } // ctor

} // class EchoStruct
