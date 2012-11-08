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
 * @(#)SwiftAddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

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
public class SwiftAddressTest extends TestCase {

    SwiftAddress instance = null;

    public SwiftAddressTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new SwiftAddress();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SwiftAddressTest.class);
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.swiftbc.extensions.SwiftAddress.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);

        // 2. testing setElementType
        QName val = new QName("http://my-Swift-address-test", "address");
        expResult = new QName("http://my-Swift-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.swiftbc.extensions.SwiftAddress.
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
     * Test of setSwiftServerLocation and getSwiftServerLocation method, of class com.sun.jbi.swiftbc.extensions.SwiftAddress.
     */
    public void testSetGetSwiftServerLocation() {
        System.out.println("Testing setSwiftServerLocation and setSwiftServerLocation");

        String val = "localhost";
        String expResult = "localhost";
        instance.setSwiftServerLocation(val);
        String result = instance.getSwiftServerLocation();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setSwiftServerLocation and getSwiftServerLocation");

    }

    /**
     * Test of setSwiftServerPort and getSwiftServerPort method, of class com.sun.jbi.swiftbc.extensions.SwiftAddress.
     */
    public void testSetGetSwiftServerPort() {
        System.out.println("Testing setRequired and getRequired");

        Integer val = new Integer(8);
        Integer expResult = new Integer(8);
        instance.setSwiftServerPort(val);
        Integer result = instance.getSwiftServerPort();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getSwiftServerPort and setSwiftServerPort");

    }

    /**
     * Test of setSwiftServerLocationURL and getSwiftServerLocationURL method, of class com.sun.jbi.swiftbc.extensions.SwiftAddress.
     */
    public void testSetGetSwiftServerLocationURL() {
        System.out.println("Testing setSwiftServerLocation and setSwiftServerLocation");

        String val = "swift://localhost:4040";
        String expResult = "swift://localhost:4040";
        instance.setSwiftServerLocationURL(val);
        String result = instance.getSwiftServerLocationURL();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setSwiftServerLocationURL and getSwiftServerLocationURL");

    }

}
