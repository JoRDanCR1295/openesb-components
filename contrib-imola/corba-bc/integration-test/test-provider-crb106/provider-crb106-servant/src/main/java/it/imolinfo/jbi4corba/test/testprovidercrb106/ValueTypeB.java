package it.imolinfo.jbi4corba.test.testprovidercrb106;


/**
* it/imolinfo/jbi4corba/test/testprovidercrb106/ValueTypeB.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoCrb106Single.idl
* gioved� 22 novembre 2007 16.36.59 CET
*/


// value type
public abstract class ValueTypeB implements org.omg.CORBA.portable.StreamableValue
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
  public it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeA typeA = null;

  private static String[] _truncatable_ids = {
    it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeBHelper.id ()
  };

  public String[] _truncatable_ids() {
    return _truncatable_ids;
  }

  public void _read (org.omg.CORBA.portable.InputStream istream)
  {
    this.fieldBoolean = istream.read_boolean ();
    this.fieldChar = istream.read_char ();
    this.fieldWChar = istream.read_wchar ();
    this.fieldOctet = istream.read_octet ();
    this.fieldString = istream.read_string ();
    this.fieldWString = istream.read_wstring ();
    this.fieldShort = istream.read_short ();
    this.fieldUnsignedShort = istream.read_ushort ();
    this.fieldLong = istream.read_long ();
    this.fieldUnsignedLong = istream.read_ulong ();
    this.fieldLongLong = istream.read_longlong ();
    this.fieldUnsignedLongLong = istream.read_ulonglong ();
    this.fieldFloat = istream.read_float ();
    this.fieldDouble = istream.read_double ();
    this.typeA = it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeAHelper.read (istream);
  }

  public void _write (org.omg.CORBA.portable.OutputStream ostream)
  {
    ostream.write_boolean (this.fieldBoolean);
    ostream.write_char (this.fieldChar);
    ostream.write_wchar (this.fieldWChar);
    ostream.write_octet (this.fieldOctet);
    ostream.write_string (this.fieldString);
    ostream.write_wstring (this.fieldWString);
    ostream.write_short (this.fieldShort);
    ostream.write_ushort (this.fieldUnsignedShort);
    ostream.write_long (this.fieldLong);
    ostream.write_ulong (this.fieldUnsignedLong);
    ostream.write_longlong (this.fieldLongLong);
    ostream.write_ulonglong (this.fieldUnsignedLongLong);
    ostream.write_float (this.fieldFloat);
    ostream.write_double (this.fieldDouble);
    it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeAHelper.write (ostream, this.typeA);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeBHelper.type ();
  }
} // class ValueTypeB
