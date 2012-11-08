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
 * @(#)HL7AddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import junit.framework.*;
import java.net.URISyntaxException;
import java.net.URI;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Raghunadh
 */
public class HL7AddressTest extends TestCase {

    HL7Address instance = null;

    public HL7AddressTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new HL7Address();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HL7AddressTest.class);
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.hl7bc.extensions.HL7Address.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/hl7/", "address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);

        // 2. testing setElementType
        QName val = new QName("http://my-hl7-address-test", "address");
        expResult = new QName("http://my-hl7-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.hl7bc.extensions.HL7Address.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        Boolean val = Boolean.TRUE;
        Boolean expResult = Boolean.TRUE;
        instance.setRequired(val);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setRequired and getRequired");

    }

    /**
     * Test of setHL7ServerLocation and getHL7ServerLocation method, of class com.sun.jbi.hl7bc.extensions.HL7Address.
     */
    public void testSetGetHL7ServerLocation() {
        System.out.println("Testing setHL7ServerLocation and setHL7ServerLocation");

        String val = "localhost";
        String expResult = "localhost";
        instance.setHL7ServerLocation(val);
        String result = instance.getHL7ServerLocation();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setHL7ServerLocation and getHL7ServerLocation");

    }

    /**
     * Test of setHL7ServerPort and getHL7ServerPort method, of class com.sun.jbi.hl7bc.extensions.HL7Address.
     */
    public void testSetGetHL7ServerPort() {
        System.out.println("Testing setRequired and getRequired");

        Integer val = new Integer(8);
        Integer expResult = new Integer(8);
        instance.setHL7ServerPort(val);
        Integer result = instance.getHL7ServerPort();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getHL7ServerPort and setHL7ServerPort");

    }

    /**
     * Test of setHL7ServerLocationURL and getHL7ServerLocationURL method, of class com.sun.jbi.hl7bc.extensions.HL7Address.
     */
    public void testSetGetHL7ServerLocationURL() {
        System.out.println("Testing setHL7ServerLocation and setHL7ServerLocation");

        String val = "hl7://localhost:4040";
        String expResult = "hl7://localhost:4040";
        instance.setHL7ServerLocationURL(val);
        String result = instance.getHL7ServerLocationURL();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setHL7ServerLocationURL and getHL7ServerLocationURL");

    }

}
