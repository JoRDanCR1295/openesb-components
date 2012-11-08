 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.WSDLGenerator;

import java.io.File;
import java.io.IOException;

import javax.wsdl.WSDLException;

import junit.framework.TestCase;


/**
 * Test of InterfaceCounterTest
 * 
 * 
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class InterfacesCounterTest extends TestCase {

    public InterfacesCounterTest() {
    }
    //This Test check the count of the number of interface in the IDL
    public void testInterfaceCounter()
            throws ClassGenerationException, IOException, WSDLException {

        File idl = new File("src/test/etc/idl/EchoMultipleComplex.idl");


        WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();


        try {
        	
            generator.idlInterfacesCounter(idl);
          
            //Count Interface
            assertEquals(generator.idlInterfacesCounter(idl), 2);


        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());


        }
    }
}
