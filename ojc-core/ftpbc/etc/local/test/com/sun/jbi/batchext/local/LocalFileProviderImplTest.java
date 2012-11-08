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
 * @(#)LocalFileProviderImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.streaming.FileInputStream;
import com.sun.jbi.batchext.streaming.StreamUtil;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
public class LocalFileProviderImplTest extends TestCase {
    
    public LocalFileProviderImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileProviderImplTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testInitialize() {
        System.out.println("initialize");
        
        BatchLocal etd = null;
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.initialize(etd);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of reset method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testReset() {
        System.out.println("reset");
        
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        boolean expResult = true;
        boolean result = instance.reset();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of append method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testAppend() {
        System.out.println("append");
        
        String srcFileName = "";
        String destFileName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        boolean expResult = true;
        boolean result = instance.append(srcFileName, destFileName);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of copy method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testCopy() throws Exception {
        System.out.println("copy");
        
        String srcFileName = "";
        String destFileName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.copy(srcFileName, destFileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of delete method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testDelete() throws Exception {
        System.out.println("delete");
        
        String fileName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.delete(fileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of mkdir method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testMkdir() throws Exception {
        System.out.println("mkdir");
        
        String dirName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.mkdir(dirName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of mkdirs method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testMkdirs() throws Exception {
        System.out.println("mkdirs");
        
        String dirName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.mkdirs(dirName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of rename method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testRename() throws Exception {
        System.out.println("rename");
        
        String srcFileName = "";
        String destFileName = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        instance.rename(srcFileName, destFileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getFirstFileName method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testGetFirstFileName() {
        System.out.println("getFirstFileName");
        
        String directory = "";
        boolean directoryIsPattern = true;
        String file = "";
        boolean fileIsPattern = true;
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        LocalFileName expResult = null;
        LocalFileName result = instance.getFirstFileName(directory, directoryIsPattern, file, fileIsPattern);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getFileInputStream method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testGetFileInputStream() throws Exception {
        System.out.println("getFileInputStream");
        
        String name = "";
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        FileInputStream expResult = null;
        FileInputStream result = instance.getFileInputStream(name);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getFileOutputStream method, of class com.sun.jbi.batchext.local.LocalFileProviderImpl.
     */
    public void testGetFileOutputStream() throws Exception {
        System.out.println("getFileOutputStream");
        
        String name = "";
        boolean append = true;
        LocalFileProviderImpl instance = new LocalFileProviderImpl();
        
        FileOutputStream expResult = null;
        FileOutputStream result = instance.getFileOutputStream(name, append);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
