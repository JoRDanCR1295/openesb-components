 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.parsing;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.generator.bcm.InternalMethodDescriptionParser;

import java.util.List;

import junit.framework.TestCase;

/**
 * Tests the InternalMethodDescriptionParser class for the internal description method parsing.
 * @author marco
 */
public class InternalMethodDescriptionTest extends TestCase {
    /**
     * Logger.
     */
    private static final Logger LOG
    = LoggerFactory.getLogger(InternalMethodDescriptionTest.class);

    /**
     * Testing for ... "Echo.idl"
     */
    public void testExtractMethodSignature() {
        try {
            String testDescription = "([Lit/imolinfo/jbi4corba/test/webservice/generator/EchoStruct;[ILorg/omg/CORBA/StringHolder;)Ljava/lang/String;"; 
            InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(testDescription);
            List<String> params = parser.parse();
            assertEquals(3, params.size());
            assertEquals("[Lit/imolinfo/jbi4corba/test/webservice/generator/EchoStruct;", params.get(0));
            assertEquals("[I", params.get(1));
            assertEquals("Lorg/omg/CORBA/StringHolder;", params.get(2));            
            for (int i = 0; i < params.size(); i++) {
                System.err.println("Param:" + params.get(i));
            }
        } catch (Exception e) {
            String m = "Error in ... testExtractMethodSignature:" + e.getMessage();
            LOG.error(m, e);
            fail(m);
        }
        LOG.debug("<<<<< testExtractMethodSignature - end");
    }
    
    /**
     * Testing for ()Ljava/lang/String;
     */
    public void testExtractEmptyMethodSignature() {
        try {
            String testDescription = "()Ljava/lang/String;"; 
            InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(testDescription);
            List<String> params = parser.parse();
            assertEquals(0, params.size());
            assertEquals("Ljava/lang/String;", parser.getMethodDescriptionTail());
            for (int i = 0; i < params.size(); i++) {
                System.err.println("Param:" + params.size());
            }
        } catch (Exception e) {
            String m = "Error in ... testExtractMethodSignature:" + e.getMessage();
            LOG.error(m, e);
            fail(m);
        }
        LOG.debug("<<<<< testExtractMethodSignature - end");
    }
    
    
    /**
     * Testing for (ILjava/lang/String;)[I
     */
    public void testExtractPrimitiveReturnTypeMethodSignature() {
        try {
            String testDescription = "(ILjava/lang/String;)[I"; 
            InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(testDescription);
            List<String> params = parser.parse();
            assertEquals(2, params.size());
            assertEquals("I", params.get(0));
            assertEquals("Ljava/lang/String;", params.get(1));  
            assertEquals("[I", parser.getMethodDescriptionTail());
            
            for (int i = 0; i < params.size(); i++) {
                System.err.println("Param:" + params.get(i));
            }
        } catch (Exception e) {
            String m = "Error in ... testExtractMethodSignature:" + e.getMessage();
            LOG.error(m, e);
            fail(m);
        }
        LOG.debug("<<<<< testExtractMethodSignature - end");
    }    
        
}
