/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test;

import org.codehaus.xfire.test.AbstractXFireTest;

public class BaseCommareaTest extends AbstractXFireTest {
  
  public BaseCommareaTest(String arg) {
    super();
    setName(arg);
  }
  
  public BaseCommareaTest() {
    super();
  }
  
  
  public static final String testRootCommarea="src/test/etc/commareas";
}
