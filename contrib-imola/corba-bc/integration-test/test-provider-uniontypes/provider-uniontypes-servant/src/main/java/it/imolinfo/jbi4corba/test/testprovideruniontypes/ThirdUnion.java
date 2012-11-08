package it.imolinfo.jbi4corba.test.testprovideruniontypes;


/**
* it/imolinfo/jbi4corba/test/testprovideruniontypes/ThirdUnion.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from F:/imolaCSV/nokia/open-jbi-components/contrib-imola/corba-bc/integration-test/test-provider-uniontypes/provider-uniontypes-jbi4corba-provider/src/main/resources/UnionTypes.idl
* luned� 19 gennaio 2009 9.19.47 EET
*/

public final class ThirdUnion implements org.omg.CORBA.portable.IDLEntity
{
  private int ___primo;
  private short ___secondo;
  private it.imolinfo.jbi4corba.test.testprovideruniontypes.FirstUnion[] ___third;
  private boolean ___altro;
  private short __discriminator;
  private boolean __uninitialized = true;

  public ThirdUnion ()
  {
  }

  public short discriminator ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    return __discriminator;
  }

  public int primo ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifyprimo (__discriminator);
    return ___primo;
  }

  public void primo (int value)
  {
    __discriminator = 1;
    ___primo = value;
    __uninitialized = false;
  }

  public void primo (short discriminator, int value)
  {
    verifyprimo (discriminator);
    __discriminator = discriminator;
    ___primo = value;
    __uninitialized = false;
  }

  private void verifyprimo (short discriminator)
  {
    if (discriminator != 1)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

  public short secondo ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifysecondo (__discriminator);
    return ___secondo;
  }

  public void secondo (short value)
  {
    __discriminator = 2;
    ___secondo = value;
    __uninitialized = false;
  }

  public void secondo (short discriminator, short value)
  {
    verifysecondo (discriminator);
    __discriminator = discriminator;
    ___secondo = value;
    __uninitialized = false;
  }

  private void verifysecondo (short discriminator)
  {
    if (discriminator != 2)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

  public it.imolinfo.jbi4corba.test.testprovideruniontypes.FirstUnion[] third ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifythird (__discriminator);
    return ___third;
  }

  public void third (it.imolinfo.jbi4corba.test.testprovideruniontypes.FirstUnion[] value)
  {
    __discriminator = 3;
    ___third = value;
    __uninitialized = false;
  }

  public void third (short discriminator, it.imolinfo.jbi4corba.test.testprovideruniontypes.FirstUnion[] value)
  {
    verifythird (discriminator);
    __discriminator = discriminator;
    ___third = value;
    __uninitialized = false;
  }

  private void verifythird (short discriminator)
  {
    if (discriminator != 3)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

  public boolean altro ()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION ();
    verifyaltro (__discriminator);
    return ___altro;
  }

  public void altro (boolean value)
  {
    __discriminator = -32768;
    ___altro = value;
    __uninitialized = false;
  }

  public void altro (short discriminator, boolean value)
  {
    verifyaltro (discriminator);
    __discriminator = discriminator;
    ___altro = value;
    __uninitialized = false;
  }

  private void verifyaltro (short discriminator)
  {
    if (discriminator == 1 || discriminator == 2 || discriminator == 3)
      throw new org.omg.CORBA.BAD_OPERATION ();
  }

} // class ThirdUnion
