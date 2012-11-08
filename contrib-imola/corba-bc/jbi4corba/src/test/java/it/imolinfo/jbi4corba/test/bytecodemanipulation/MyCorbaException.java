 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;


public class MyCorbaException extends org.omg.CORBA.UserException {
    public int reasonCode = (int)0;
    public int [] foo;

    private static String  _id = "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/EchoComplexException:1.0";

    public MyCorbaException ()
    {
        super(_id);
        reasonCode = 0;
    } // ctor

    public MyCorbaException (int _reasonCode)
    {
        super(_id);
        reasonCode = _reasonCode;
    } // ctor


    public MyCorbaException (String $reason, int _reasonCode)
    {
        super(_id + "  " + $reason);
        reasonCode = _reasonCode;
    } // ctor

    public int [] getArray() {
        return foo;
    }
}
