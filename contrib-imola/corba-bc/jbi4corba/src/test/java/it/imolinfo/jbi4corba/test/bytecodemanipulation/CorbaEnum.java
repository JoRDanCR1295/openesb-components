 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;

public class CorbaEnum implements org.omg.CORBA.portable.IDLEntity
{
  private        int __value;
  private static int __size = 3;
  private static it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum[] __array = new it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum [__size];

  public static final int _E1 = 0;
  public static final it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum E1 = new it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum(_E1);
  public static final int _E2 = 1;
  public static final it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum E2 = new it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum(_E2);
  public static final int _E3 = 2;
  public static final it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum E3 = new it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum(_E3);

  public int value ()
  {
    return __value;
  }

  public static it.imolinfo.jbi4corba.test.bytecodemanipulation.CorbaEnum from_int (int value)
  {
    if (value >= 0 && value < __size)
      return __array[value];
    else
      throw new org.omg.CORBA.BAD_PARAM ();
  }

  protected CorbaEnum (int value)
  {
    __value = value;
    __array[__value] = this;
  }
} // class EchoComplexEnum
