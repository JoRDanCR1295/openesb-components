package it.imolinfo.jbi4corba.test.testproviderenum;


/**
* it/imolinfo/jbi4corba/test/testproviderenum/EchoComplexEnum.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from src/main/resources/EchoEnum.idl
* marted� 27 novembre 2007 11.52.46 CET
*/

public class EchoComplexEnum implements org.omg.CORBA.portable.IDLEntity
{
  private        int __value;
  private static int __size = 3;
  private static it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum[] __array = new it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum [__size];

  public static final int _E1 = 0;
  public static final it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum E1 = new it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum(_E1);
  public static final int _E2 = 1;
  public static final it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum E2 = new it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum(_E2);
  public static final int _E3 = 2;
  public static final it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum E3 = new it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum(_E3);

  public int value ()
  {
    return __value;
  }

  public static it.imolinfo.jbi4corba.test.testproviderenum.EchoComplexEnum from_int (int value)
  {
    if (value >= 0 && value < __size)
      return __array[value];
    else
      throw new org.omg.CORBA.BAD_PARAM ();
  }

  protected EchoComplexEnum (int value)
  {
    __value = value;
    __array[__value] = this;
  }
} // class EchoComplexEnum
