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
 * @(#)LocalFileNameTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;

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
public class LocalFileNameTest extends TestCase {
    
    public LocalFileNameTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileNameTest.class);
        
        return suite;
    }

    /**
     * Test of isDirectory method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testIsDirectory() {
        System.out.println("isDirectory");
        
        LocalFileName instance = null;
        
        boolean expResult = true;
        boolean result = instance.isDirectory();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of isFile method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testIsFile() {
        System.out.println("isFile");
        
        LocalFileName instance = null;
        
        boolean expResult = true;
        boolean result = instance.isFile();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getFullPathName method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testGetFullPathName() {
        System.out.println("getFullPathName");
        
        LocalFileName instance = null;
        
        String expResult = "";
        String result = instance.getFullPathName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testGetDirectoryName() {
        System.out.println("getDirectoryName");
        
        LocalFileName instance = null;
        
        String expResult = "";
        String result = instance.getDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getFileName method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testGetFileName() {
        System.out.println("getFileName");
        
        LocalFileName instance = null;
        
        String expResult = "";
        String result = instance.getFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of toString method, of class com.sun.jbi.batchext.local.LocalFileName.
     */
    public void testToString() {
        System.out.println("toString");
        
        LocalFileName instance = null;
        
        String expResult = "";
        String result = instance.toString();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
