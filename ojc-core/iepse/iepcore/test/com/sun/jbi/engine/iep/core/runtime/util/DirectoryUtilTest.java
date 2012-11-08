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
 * @(#)DirectoryUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;
//import com.sun.org.apache.xerces.internal.parsers.JAXPConfiguration;
import junit.framework.*;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
/*
 * DirectoryUtilTest.java
 * JUnit based test
 *
 * Created on February 15, 2007, 5:10 PM
 */

/**
 *
 * @author rdwivedi
 */
public class DirectoryUtilTest extends TestCase {
    File mTmpDir = null;
    File mCpyDir = null;
    File mOtherDir = null;
    
    public DirectoryUtilTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mTmpDir = new File("junTestTemp");
        if(mTmpDir.exists()) {
            mTmpDir.delete();
        }
        mTmpDir.mkdir();
        mCpyDir = new File("junTestTempCopy");
        if(mCpyDir.exists()) {
            mCpyDir.delete();
        }
        mCpyDir.mkdir();
    }

    protected void tearDown() throws Exception {
       File[] filesA=  mTmpDir.listFiles();
       File[] filesB=  mCpyDir.listFiles();
       for(int i = 0 ; i < filesA.length; i++) {
           filesA[i].delete();
       }
        mTmpDir.delete();
        for(int i = 0 ; i < filesB.length; i++) {
           filesB[i].delete();
       }
        mCpyDir.delete();
        
    }
    

    public static Test suite() {
        TestSuite suite = new TestSuite(DirectoryUtilTest.class);
        
        return suite;
    }

    /**
     * Test of getFiles method, of class com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.
     */
    public void testGetFiles() throws Exception {
        File.createTempFile("a11",null,mTmpDir);
        File.createTempFile("a22",null,mTmpDir);
        File.createTempFile("a33",null,mTmpDir);

        
        List result = com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.getFiles(mTmpDir.getAbsolutePath());
        assertEquals(3, result.size());
        
       
    }

    

    /**
     * Test of copyFile method, of class com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.
     */
    public void testCopyFile() throws Exception {
        
        File source = File.createTempFile("mainOne",null,mTmpDir);
        File dest = File.createTempFile("copiedOne",null,mTmpDir);
        
        boolean expResult = true;
        boolean result = com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.copyFile(source, dest);
        assertEquals(expResult, result);
    }

    /**
     * Test of copyFilesInDir method, of class com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.
     */
    public void testCopyFilesInDir() throws Exception {
        
        
        File.createTempFile("a11",null,mTmpDir);
        File.createTempFile("a22",null,mTmpDir);
        File.createTempFile("a33",null,mTmpDir);
        
        File sourceDir = mTmpDir;
        File targetDir = mCpyDir;
        FileFilter filter = null;
        boolean recursive = true;
        
        com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.copyFilesInDir(sourceDir, targetDir, filter, recursive);
        int len = mCpyDir.listFiles().length;
        
        assertEquals(3,len);
        
    }

    


    /**
     * Test of createDir method, of class com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.
     */
    public void testCreateDir() {
        String dirName = mTmpDir.getAbsolutePath()+  "tmpTmpDir";
        boolean result = com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.createDir(dirName);
        mOtherDir = new File(dirName);
        assertTrue(mOtherDir.exists());
        mOtherDir.delete();
        
    }

    /**
     * Test of deleteDir method, of class com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.
     */
  /*  public void testDeleteDir() {
        
        
        String dirName = "";
        
        boolean expResult = true;
        boolean result = com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil.deleteDir(dirName);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }
*/
    

   
 
    
}
