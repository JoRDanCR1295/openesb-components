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
 * @(#)FtpFileStatePersistenceAdapterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
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
public class FtpFileStatePersistenceAdapterTest extends TestCase {
    private File testFile;
    private FtpFileStatePersistenceAdapter instance;
    
    public FtpFileStatePersistenceAdapterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        testFile = File.createTempFile("testFileStatePersistenceAdapter","");
        instance = new FtpFileStatePersistenceAdapter(testFile.getParent(), testFile.getName());
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
        TestSuite suite = new TestSuite(FtpFileStatePersistenceAdapterTest.class);
        
        return suite;
    }
    
    /**
     * Test of writeStateToFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileStatePersistenceAdapter.
     */
    public void testWriteStateToFile() throws Exception {
        System.out.println("writeStateToFile");
        
        String fullPathFilename = testFile.getPath();
        Serializable state = new FtpFileState();
        
        instance.writeStateToFile(fullPathFilename, state);
        
    }

    /**
     * Test of readStateFromFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileStatePersistenceAdapter.
     */
    public void testReadStateFromFile() throws Exception {
        System.out.println("readStateFromFile");
        
        String fullPathFilename = testFile.getPath();
        
        FtpFileState expResult = new FtpFileState();
        Arrays.fill(expResult.getSequenceNo(), 100);
        // write first
        instance.writeStateToFile(fullPathFilename, expResult);
        
        FtpFileState result = (FtpFileState) instance.readStateFromFile(fullPathFilename);
        for (int i = 0; i < expResult.getSequenceNo().length; i++) {
            assertEquals(expResult.getSequenceNo(i), result.getSequenceNo(i));
        }
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileStatePersistenceAdapter.
     */
    public void testFtpFileStateDBPersistenceAdapter () throws Exception {
        System.out.println ("FtpFileStatePersistenceAdapter");
        
        assertNotNull (new FtpFileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), "FileExistenceIsNotCheckedHere"));
        assertNotNull (new FtpFileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), "")); // why empty file name is allowed?
        
        assertNotNull (new FtpFileStatePersistenceAdapter (System.getProperty("java.io.tmpdir") + File.separator + "testFtpFileStatePersistenceAdapter", "FileExistenceIsNotCheckedHere"));
        
        try {
            new FtpFileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), null);
            fail("An exception is expected - null file name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            new FtpFileStatePersistenceAdapter (null, "abc");
            fail("An exception is expected - null dir name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
