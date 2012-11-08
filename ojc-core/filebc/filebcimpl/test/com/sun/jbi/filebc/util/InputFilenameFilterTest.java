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
 * @(#)InputFilenameFilterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import junit.framework.*;
import java.io.File;
import java.util.concurrent.atomic.AtomicInteger;

/**
 *
 * @author sweng
 */
public class InputFilenameFilterTest extends TestCase {

    InputFilenameFilter instance;
    InputFilenameFilter instance2;

    public InputFilenameFilterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new InputFilenameFilter();
        AtomicInteger maxFiles = new AtomicInteger(20);
        instance2 = new InputFilenameFilter("file%d.txt", "", maxFiles, false);
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of setFilterExpression and getFilterExpression method, of class com.sun.jbi.filebc.util.InputFilenameFilter.
     */
    public void testSetGetFilterExpression() throws Exception {
        System.out.println("Testing setFilterExpression");
        String filename;
        String expResult;
        String result = "";

        // test %t
        filename = "Input%tABC.txt";
        expResult = "Input[0-9]{4,4}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])(\\-(0[0-9]|1[0-9]|2[0-3])\\-[0-5][0-9]\\-[0-5][0-9]\\-[0-9]{0,3})?ABC\\.txt";
        try {
            instance.setFilterExpression(filename);
            result = instance.getFilterExpression();
        } catch (Exception e) {
            fail("Failed to test setFilterExpression");
        }
        assertEquals(expResult, result);

        // test %d
        filename = "Input%d.dat";
        expResult = "Input[0-9]+\\.dat";
        try {
            instance.setFilterExpression(filename);
            result = instance.getFilterExpression();
        } catch (Exception e) {
            fail("Failed to test setFilterExpression");
        }
        assertEquals(expResult, result);

        // test %u
        filename = "Input%uSomething.dat";
        expResult = "Input" + FileNamePatternUtil.IB_UUID_REGEX + "Something\\.dat";
        try {
            instance.setFilterExpression(filename);
            result = instance.getFilterExpression();
        } catch (Exception e) {
            fail("Failed to test setFilterExpression");
        }
        assertEquals(expResult, result);
        System.out.println("Successfully testing setFilterExpression");
    }

    /**
     * Test of accept method, of class com.sun.jbi.filebc.util.InputFilenameFilter.
     */
    public void testAccept() {
        System.out.println("Testing accept");

        boolean result = false;

        try {
            instance2.setFilterExpression("Input%d.dat");
            result = instance2.accept(new File("."), "Input12394828738.dat");
        } catch (Exception e) {
            e.printStackTrace();
            fail("Faile to test accept method");
        }
        assertTrue(result);

        try {
            result = instance2.accept(new File("."), "Input12394828738something.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }
        assertFalse(result);

        try {
            instance2.setFilterExpression("Input%u.dat");
            result = instance2.accept(new File("."), "Input67e9e747-ee91-4669-9ae1-1048785f6f9e.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertTrue(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19680528-12-36-36-001.dat");
        } catch (Exception e) {
            e.printStackTrace();
            fail("Faile to test accept method");
        }

        assertTrue(result);

        try {
            instance2.setFilterExpression("Input%tsomething.dat");
            result = instance2.accept(new File("."), "Input19680528-12-36-36-001something.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertTrue(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19681528-12-36-36-001.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertFalse(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19680528-24-36-36-001.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertFalse(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19680528-12-60-36-001.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertFalse(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19680528-12-36-36-001.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertTrue(result);

        try {
            instance2.setFilterExpression("Input%t.dat");
            result = instance2.accept(new File("."), "Input19680528.dat");
        } catch (Exception e) {
            fail("Faile to test accept method");
        }

        assertTrue(result);

        System.out.println("Successfully tested accept");
    }
}
