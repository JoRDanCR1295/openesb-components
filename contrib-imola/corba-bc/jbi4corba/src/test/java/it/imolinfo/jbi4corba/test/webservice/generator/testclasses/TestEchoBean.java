 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator.testclasses;

public class TestEchoBean implements EchoOperations {

    public java.lang.String echo(java.lang.String msg) {
        System.out.println("TestEchoBean, received: " + msg);
        return msg;
    }
   
}

