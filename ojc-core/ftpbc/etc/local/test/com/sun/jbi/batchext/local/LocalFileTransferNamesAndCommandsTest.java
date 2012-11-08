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
 * @(#)LocalFileTransferNamesAndCommandsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import com.sun.jbi.batchext.NamePattern;
import com.sun.jbi.batchext.TransferNamesAndCommands;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;

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
public class LocalFileTransferNamesAndCommandsTest extends TestCase {
    LocalFileTransferNamesAndCommands instance;
    
    public LocalFileTransferNamesAndCommandsTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileTransferNamesAndCommands.class);
        
        return suite;
    }

    /**
     * Test of getTargetDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetTargetDirectoryName() throws Exception {
        System.out.println("getTargetDirectoryName");
        
        String expResult = "";
        String result = instance.getTargetDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTargetFileName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetTargetFileName() throws Exception {
        System.out.println("getTargetFileName");
        
        String expResult = "";
        String result = instance.getTargetFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getAppend method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetAppend() {
        System.out.println("getAppend");
        
        boolean expResult = true;
        boolean result = instance.getAppend();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPreTransferCommand() throws Exception {
        System.out.println("getPreTransferCommand");
        
        String expResult = "";
        String result = instance.getPreTransferCommand();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPreDirectoryName() throws Exception {
        System.out.println("getPreDirectoryName");
        
        String expResult = "";
        String result = instance.getPreDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreFileName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPreFileName() throws Exception {
        System.out.println("getPreFileName");
        
        String expResult = "";
        String result = instance.getPreFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPostTransferCommand() throws Exception {
        System.out.println("getPostTransferCommand");
        
        String expResult = "";
        String result = instance.getPostTransferCommand();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPostDirectoryName() throws Exception {
        System.out.println("getPostDirectoryName");
        
        String expResult = "";
        String result = instance.getPostDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostFileName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetPostFileName() throws Exception {
        System.out.println("getPostFileName");
        
        String expResult = "";
        String result = instance.getPostFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTransferDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetTransferDirectoryName() throws Exception {
        System.out.println("getTransferDirectoryName");
        
        String expResult = "";
        String result = instance.getTransferDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTransferFileName method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testGetTransferFileName() throws Exception {
        System.out.println("getTransferFileName");
        
        String expResult = "";
        String result = instance.getTransferFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolveNamePattern method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands.
     */
    public void testResolveNamePattern() throws Exception {
        System.out.println("resolveNamePattern");
        
        String patternTag = "";
        String pattern = "";
        String originalName = "";
        BatchLocal local = null;

        String expResult = "";
        String result = instance.resolveNamePattern(patternTag, pattern, originalName, local);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
