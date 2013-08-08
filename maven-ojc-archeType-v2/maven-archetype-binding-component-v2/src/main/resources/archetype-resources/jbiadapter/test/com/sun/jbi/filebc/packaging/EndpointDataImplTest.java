#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})EndpointDataImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.packaging;

import junit.framework.*;

/**
 *
 * @author sweng
 */
public class EndpointDataImplTest extends TestCase {

    EndpointDataImpl instance = null;

    public EndpointDataImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new EndpointDataImpl("{http://localhost/filebctest/FileOut}portTypeFileOut",
                "{http://localhost/filebctest/FileOut}serviceFileOut",
                "portFileOut",
                0);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointDataImplTest.class);

        return suite;
    }

    /**
     * Test of getInterface method, of class com.sun.jbi.filebc.packaging.EndpointDataImpl.
     */
    public void testGetInterface() {
        System.out.println("Testing getInterface");

        String expResult = "{http://localhost/filebctest/FileOut}portTypeFileOut";
        String result = instance.getInterface();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getInterface");

    }

    /**
     * Test of getService method, of class com.sun.jbi.filebc.packaging.EndpointDataImpl.
     */
    public void testGetService() {
        System.out.println("Testing getService");

        String expResult = "{http://localhost/filebctest/FileOut}serviceFileOut";
        String result = instance.getService();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getService");
    }

    /**
     * Test of getEndpoint method, of class com.sun.jbi.filebc.packaging.EndpointDataImpl.
     */
    public void testGetEndpoint() {
        System.out.println("Testing getEndpoint");

        String expResult = "portFileOut";
        String result = instance.getEndpoint();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getEndpoint");
    }

    /**
     * Test of getDirection method, of class com.sun.jbi.filebc.packaging.EndpointDataImpl.
     */
    public void testGetDirection() {
        System.out.println("Testing getDirection");

        int expResult = 0;
        int result = instance.getDirection();
        assertEquals(expResult, result);

        System.out.println("Successfully tested getDirection");
    }
}
