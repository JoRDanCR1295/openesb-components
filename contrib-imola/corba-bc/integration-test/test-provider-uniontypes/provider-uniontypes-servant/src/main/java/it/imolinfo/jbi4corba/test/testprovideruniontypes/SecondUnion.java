package it.imolinfo.jbi4corba.test.testprovideruniontypes;


/**
* it/imolinfo/jbi4corba/test/testprovideruniontypes/SecondUnion.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-uniontypes/provider-uniontypes-jbi4corba-provider/src/main/resources/UnionTypes.idl
* luned� 19 gennaio 2009 9.19.47 EET
*/

public final class SecondUnion implements org.omg.CORBA.portable.IDLEntity
{
  private int ___numeric;
  private String ___alfanumeric;
  private org.omg.CORBA.Any ___two_format;
  private short __discriminator;
  private boolean __uninitialized = true;

  public SecondUnion ()
  {
  }

  public short discriminator ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    return __discriminator;
  }

  public int numeric ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifynumeric (__discriminator);
    return ___numeric;
  }

  public void numeric (int value)
  {
    __discriminator = 1;
    ___numeric = value;
    __uninitialized = false;
  }

  public void numeric (short discriminator, int value)
  {
    verifynumeric (discriminator);
    __discriminator = discriminator;
    ___numeric = value;
    __uninitialized = false;
  }

  private void verifynumeric (short discriminator)
  {
    if (discriminator != 1)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

  public String alfanumeric ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifyalfanumeric (__discriminator);
    return ___alfanumeric;
  }

  public void alfanumeric (String value)
  {
    __discriminator = 2;
    ___alfanumeric = value;
    __uninitialized = false;
  }

  public void alfanumeric (short discriminator, String value)
  {
    verifyalfanumeric (discriminator);
    __discriminator = discriminator;
    ___alfanumeric = value;
    __uninitialized = false;
  }

  private void verifyalfanumeric (short discriminator)
  {
    if (discriminator != 2)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

  public org.omg.CORBA.Any two_format ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifytwo_format (__discriminator);
    return ___two_format;
  }

  public void two_format (org.omg.CORBA.Any value)
  {
    __discriminator = -32768;
    ___two_format = value;
    __uninitialized = false;
  }

  public void two_format (short discriminator, org.omg.CORBA.Any value)
  {
    verifytwo_format (discriminator);
    __discriminator = discriminator;
    ___two_format = value;
    __uninitialized = false;
  }

  private void verifytwo_format (short discriminator)
  {
    if (discriminator == 1 || discriminator == 2)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

} // class SecondUnion