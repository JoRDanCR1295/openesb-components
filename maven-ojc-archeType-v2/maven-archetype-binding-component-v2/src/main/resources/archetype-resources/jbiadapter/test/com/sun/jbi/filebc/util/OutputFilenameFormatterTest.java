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
 * @(${symbol_pound})OutputFilenameFormatterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import junit.framework.*;

/**
 *
 * @author sweng
 */
public class OutputFilenameFormatterTest extends TestCase {

    public OutputFilenameFormatterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getNextOutputFileName method, of class com.sun.jbi.filebc.util.OutputFilenameFormatter.
     */
    public void testGetNextOutputFileName() {
        System.out.println("Testing getNextOutputFileName");

        String expResult = "test0.dat";
        String result = "";

        try {
            result = OutputFilenameFormatter.getNextOutputFileName("test%d.dat");
        } catch (Exception e) {
            fail("Failed to test getNextOutputFileName.");
        }
        assertEquals(expResult, result);

        expResult = "test1.dat";
        try {
            result = OutputFilenameFormatter.getNextOutputFileName("test%d.dat");
        } catch (Exception e) {
            fail("Failed to test getNextOutputFileName.");
        }
        assertEquals(expResult, result);

        expResult = "test2.dat";
        try {
            result = OutputFilenameFormatter.getNextOutputFileName("test%d.dat");
        } catch (Exception e) {
            fail("Failed to test getNextOutputFileName.");
        }
        assertEquals(expResult, result);

        System.out.println("Successfully tested getNextOutputFileName");
    }

    /**
     * Test of getOutputFileName method, of class com.sun.jbi.filebc.util.OutputFilenameFormatter.
     */
    public void testGetOutputFileName() {
        System.out.println("Testing getOutputFileName");

        String expResult = "Test0.txt";
        String result = "";
        try {
            result = OutputFilenameFormatter.getOutputFileName("Test%d.txt");
        } catch (Exception e) {
            fail("Failed to test GetOutputFileName.");
        }
        assertEquals(expResult, result);

        expResult = "dummy0.dat";
        try {
            result = OutputFilenameFormatter.getOutputFileName("dummy%d.dat");
        } catch (Exception e) {
            fail("Failed to test GetOutputFileName.");
        }
        assertEquals(expResult, result);

        expResult = "Test0.txt";
        try {
            result = OutputFilenameFormatter.getOutputFileName("Test%d.txt");
        } catch (Exception e) {
            fail("Failed to test GetOutputFileName.");
        }
        assertEquals(expResult, result);

        System.out.println("Successfully tested getOutputFileName");
    }
}
