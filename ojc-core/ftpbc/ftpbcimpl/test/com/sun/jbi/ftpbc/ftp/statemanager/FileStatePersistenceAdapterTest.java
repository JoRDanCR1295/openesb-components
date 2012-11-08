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
 * @(#)FileStatePersistenceAdapterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.statemanager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.Serializable;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

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
public class FileStatePersistenceAdapterTest extends TestCase {
    File testFile;
    FileStatePersistenceAdapter instance;
    
    public FileStatePersistenceAdapterTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // create a test file with 10 bytes content
        testFile = File.createTempFile("testFileStatePersistenceAdapter","");
        FileOutputStream fos = new FileOutputStream(testFile);
        byte[] tenBytes = "0123456789".getBytes();
        fos.write(tenBytes);
        fos.flush();
        fos.close();
        
        instance = new FileStatePersistenceAdapter(testFile.getParent(), testFile.getName());
    }
    
    protected void tearDown() throws Exception {
        try {
            testFile.deleteOnExit();
            testFile.delete();
            instance.close();
        } catch (Exception ex) {
            ;
        }
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FileStatePersistenceAdapterTest.class);
        
        return suite;
    }
    
    /**
     * Test of save method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testSave() throws Exception {
        System.out.println("save");
        
        Serializable state = null;
        instance.save(state);

        state = "some data from FileStatePersistenceAdapter";
        instance.save(state);
        
    }
    
    /**
     * Test of restore method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testRestore() throws Exception {
        System.out.println("restore");
        
        Serializable expResult = null;
        try {
            Serializable result = instance.restore();
            fail("An exception is expected - the pre-filled content is not restore-able");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        //depend on status, it could be true or false: assertEquals(expResult, result);
        
    }
    
    /**
     * Test of close method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        
        instance.close();
        
    }
    
    /**
     * Test of createDirectory method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testCreateDirectory() throws Exception {
        System.out.println("createDirectory");
        
        File dir = new File(System.getProperty("java.io.tmpdir"), "testCreateDirectory");
        
        boolean expResult = true;
        boolean result = FileStatePersistenceAdapter.createDirectory(dir);
        assertEquals(expResult, result);
        
        dir.delete();
    }
    
    /**
     * Test of writeStateToFile method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testWriteStateToFile() throws Exception {
        System.out.println("writeStateToFile");
        
        String fullPathFilename = testFile.getPath();
        Serializable state = "test data for state";
        
        instance.writeStateToFile(fullPathFilename, state);
        
    }
    
    /**
     * Test of readStateFromFile method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testReadStateFromFile() throws Exception {
        System.out.println("readStateFromFile");
        
        String fullPathFilename = "NotExisted";
        Serializable expResult = null;
        Serializable result = instance.readStateFromFile(fullPathFilename);
        assertEquals(expResult, result);
        
        fullPathFilename = testFile.getPath();
        try {
            instance.readStateFromFile(fullPathFilename);
            fail("An exception is expected - the pre-filled data is not de-Serializable");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapter.
     */
    public void testFileStatePersistenceAdapter () throws Exception {
        System.out.println ("FileStatePersistenceAdapter");
        
        assertNotNull (new FileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), "FileExistenceIsNotCheckedHere"));
        assertNotNull (new FileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), "")); // why empty file name is allowed?
        
        assertNotNull (new FileStatePersistenceAdapter (System.getProperty("java.io.tmpdir") + File.separator + "testFileStatePersistenceAdapter", "FileExistenceIsNotCheckedHere"));
        
        try {
            new FileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), null);
            fail("An exception is expected - null file name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            new FileStatePersistenceAdapter (null, "abc");
            fail("An exception is expected - null dir name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
