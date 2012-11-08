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
 * @(#)FileStatePersistenceAdapterTransTest.java 
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
public class FileStatePersistenceAdapterTransTest extends TestCase {
    File testFile;
    FileStatePersistenceAdapterTrans instance;
    
    public FileStatePersistenceAdapterTransTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // create a test file with 10 bytes content
        testFile = File.createTempFile("testFileStatePersistenceAdapterTrans","");
        FileOutputStream fos = new FileOutputStream(testFile);
        byte[] tenBytes = "0123456789".getBytes();
        fos.write(tenBytes);
        fos.flush();
        fos.close();
        
        instance = new FileStatePersistenceAdapterTrans(testFile.getParent(), testFile.getName());
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
        TestSuite suite = new TestSuite(FileStatePersistenceAdapterTransTest.class);
        
        return suite;
    }
    
    /**
     * Test of restore method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testRestore() throws Exception {
        System.out.println("restore");
        
        Serializable expResult = null;
        try {
            Serializable result = instance.restore();
            if ( result == null ) {
                // state file does not exists - deleted since setup
                // this should not fail the test
            }
            fail("An exception is expected - the pre-filled content is not restore-able");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        //depend on status, it could be true or false: assertEquals(expResult, result);
        
    }
    
    /**
     * Test of prepare method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testPrepare() throws Exception {
        System.out.println("prepare");
        
        instance.prepare();
        
    }
    
    /**
     * Test of save method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testSave() throws Exception {
        System.out.println("save");
        
        Serializable state = null;
        instance.save(state);
        
        state = "some data from FileStatePersistenceAdapterTrans";
        instance.save(state);
        
    }
    
    /**
     * Test of commit method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testCommit() throws Exception {
        System.out.println("commit");
        
        instance.commit();
        
    }
    
    /**
     * Test of rollback method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testRollback() throws Exception {
        System.out.println("rollback");
        
        instance.rollback();
        
    }
    
    /**
     * Test of close method, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        
        instance.close();
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.statemanager.FileStatePersistenceAdapterTrans.
     */
    public void testFileStatePersistenceAdapterTrans () throws Exception {
        System.out.println ("FileStatePersistenceAdapterTrans");
        
        assertNotNull (new FileStatePersistenceAdapterTrans (System.getProperty("java.io.tmpdir"), "FileExistenceIsNotCheckedHere"));
        assertNotNull (new FileStatePersistenceAdapterTrans (System.getProperty("java.io.tmpdir"), "")); // why empty file name is allowed?
        
        assertNotNull (new FileStatePersistenceAdapterTrans (System.getProperty("java.io.tmpdir") + File.separator + "testFileStatePersistenceAdapterTrans", "FileExistenceIsNotCheckedHere"));
        
        try {
            new FileStatePersistenceAdapterTrans (System.getProperty("java.io.tmpdir"), null);
            fail("An exception is expected - null file name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            new FileStatePersistenceAdapterTrans (null, "abc");
            fail("An exception is expected - null dir name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
