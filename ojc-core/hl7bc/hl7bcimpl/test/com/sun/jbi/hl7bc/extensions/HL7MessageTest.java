/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HL7MessageTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Raghunadh
 */
public class HL7MessageTest extends TestCase {
    HL7Message instance = new HL7Message();
    
    public HL7MessageTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HL7MessageTest.class);
        
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.hl7bc.extensions.HL7Message.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        QName elementType = QName.valueOf("{http://www.sun.com/hl7}hl7Message");
        instance.setElementType(elementType);
        
        QName result = new QName("http://www.sun.com/hl7", "hl7Message");
        assertEquals(elementType, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.hl7bc.extensions.HL7Message.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        Boolean result = instance.getRequired();
        assertEquals(required, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }

   
     /**
     * Test of setEncodingStyle and getEncodingStyle method, of class com.sun.jbi.hl7bc.extensions.HL7Message.
     */
    public void testSetGetHL7EncodingStyle() {
        System.out.println("Testing setHL7EncodingStyle and getHL7EncodingStyle");
        
        String val = "HL7encoder";
        instance.setEncodingStyle(val);
        String result = instance.getEncodingStyle();
        assertEquals(val, result);
        
        System.out.println("Successfully Tested setEncodingStyle and getEncodingStyle");
    }

  
    /**
     * Test of setPart and getPart method, of class com.sun.jbi.hl7bc.extensions.HL7Message.
     */
    public void testSetGetPart() {
        System.out.println("Testing setPart and getPart");
        
        String val = "messagePart";
        instance.setPart(val);
        String result = instance.getPart();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setPart and getPart");
    }

     /**
     * Test of getUseType method, of class com.sun.jbi.hl7bc.extensions.HL7Message.
     */
    public void testSetGetUseType() {
        System.out.println("Testing getUseType");
        
        String val = "encoded";
        String result = instance.getUseType();
        assertEquals(val, result);
        
        System.out.println("Successfully getUseType");
    }

       
}
