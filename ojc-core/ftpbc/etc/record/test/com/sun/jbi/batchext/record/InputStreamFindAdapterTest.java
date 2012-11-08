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
 * @(#)InputStreamFindAdapterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.record;

import junit.framework.*;
import java.io.InputStream;

/*
 * JUnit based test.
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class InputStreamFindAdapterTest extends TestCase {
    
    public InputStreamFindAdapterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InputStreamFindAdapterTest.class);
        
        return suite;
    }

    /**
     * Test of available method, of class com.sun.jbi.batchext.record.InputStreamFindAdapter.
     */
    public void testAvailable() throws Exception {
        System.out.println("available");
        
        InputStreamFindAdapter instance = null;
        
        int expResult = 0;
        int result = instance.available();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of read method, of class com.sun.jbi.batchext.record.InputStreamFindAdapter.
     */
    public void testRead() throws Exception {
        System.out.println("read");
        
        byte[] buffer = null;
        int count = 0;
        InputStreamFindAdapter instance = null;
        
        int expResult = 0;
        int result = instance.read(buffer, count);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of find method, of class com.sun.jbi.batchext.record.InputStreamFindAdapter.
     */
    public void testFind() throws Exception {
        System.out.println("find");
        
        byte b = 0;
        InputStreamFindAdapter instance = null;
        
        InputStreamFindResult expResult = null;
        InputStreamFindResult result = instance.find(b);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setDelimOnLastRecord method, of class com.sun.jbi.batchext.record.InputStreamFindAdapter.
     */
    public void testSetDelimOnLastRecord() {
        System.out.println("setDelimOnLastRecord");
        
        boolean bDelimOnLast = true;
        InputStreamFindAdapter instance = null;
        
        instance.setDelimOnLastRecord(bDelimOnLast);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getDelimOnLastRecord method, of class com.sun.jbi.batchext.record.InputStreamFindAdapter.
     */
    public void testGetDelimOnLastRecord() {
        System.out.println("getDelimOnLastRecord");
        
        InputStreamFindAdapter instance = null;
        
        boolean expResult = true;
        boolean result = instance.getDelimOnLastRecord();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
