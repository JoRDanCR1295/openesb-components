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
 * @(#)FtpFileTransferNamesAndCommandsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.util.Properties;
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
public class FtpFileTransferNamesAndCommandsTest extends TestCase {
    FtpFileTransferNamesAndCommands instance;
    FtpInterface ftp;
    
    public FtpFileTransferNamesAndCommandsTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        Properties props = new Properties();
        //props.put("General Settings/Connection Mode", "Manual");
        props.put("FTP/Host Name", FtpTprops.FTP_TEST_HOST);
        props.put("FTP/Directory Listing Style", "UNIX");
        
        ftp = new FtpInterface();
        ftp.initialize(props);
        instance = new FtpFileTransferNamesAndCommandsGet(ftp);
    }
    
    protected void tearDown() throws Exception {
        try {
            ftp.reset();
            if ( ftp.getClient() != null )
                ftp.getClient().close();
        } catch (Exception ex) {
            ;
        }
        ftp = null;
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileTransferNamesAndCommandsTest.class);
        
        return suite;
    }
    
    /**
     * Test of getTargetDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetTargetDirectoryName() throws Exception {
        System.out.println("getTargetDirectoryName");
        // server can be configured such that 
        // the target directory name is not an empty value
        // String expResult = "";
        String result = instance.getTargetDirectoryName();
        assertNotNull(result);
        
    }
    
    /**
     * Test of getTargetFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetTargetFileName() throws Exception {
        System.out.println("getTargetFileName");
        // server can be configured such that 
        // the target directory name is not an empty value
        //String expResult = "";
        String result = instance.getTargetFileName();
        assertNotNull(result);
        
    }
    
    /**
     * Test of getPreTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPreTransferCommand() throws Exception {
        System.out.println("getPreTransferCommand");
        
        String expResult = "None";
        String result = instance.getPreTransferCommand();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPreDirectoryName() throws Exception {
        System.out.println("getPreDirectoryName");
        
        String expResult = "";
        String result = instance.getPreDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPreFileName() throws Exception {
        System.out.println("getPreFileName");
        
        String expResult = "";
        String result = instance.getPreFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPostTransferCommand() throws Exception {
        System.out.println("getPostTransferCommand");
        
        String expResult = "None";
        String result = instance.getPostTransferCommand();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPostDirectoryName() throws Exception {
        System.out.println("getPostDirectoryName");
        
        String expResult = "";
        String result = instance.getPostDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetPostFileName() throws Exception {
        System.out.println("getPostFileName");
        
        String expResult = "";
        String result = instance.getPostFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getAppend method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetAppend() {
        System.out.println("getAppend");
        
        boolean expResult = false;
        boolean result = instance.getAppend();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getCleanupOnPutFailure method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testGetCleanupOnPutFailure() {
        System.out.println("getCleanupOnPutFailure");
        
        boolean expResult = false;
        boolean result = instance.getCleanupOnPutFailure();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setCleanupOnPutFailure method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testSetCleanupOnPutFailure() {
        System.out.println("setCleanupOnPutFailure");
        
        boolean cleanup = true;
        
        instance.setCleanupOnPutFailure(cleanup);
        
        assertEquals(cleanup, instance.getCleanupOnPutFailure());
    }
    
    /**
     * Test of resolvePostTransfer method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testResolvePostTransfer() throws Exception {
        System.out.println("resolvePostTransfer");
        
        instance.resolvePostTransfer();
        
    }
    
    /**
     * Test of resolvePreTransfer method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testResolvePreTransfer() throws Exception {
        System.out.println("resolvePreTransfer");
        
        instance.resolvePreTransfer();
        
    }
    
    /**
     * Test of resolveTargetLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testResolveTargetLocation() throws Exception {
        System.out.println("resolveTargetLocation");
        
        instance.resolveTargetLocation();
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands.
     */
    public void testFtpFileTransferNamesAndCommands() throws Exception {
        System.out.println("FtpFileTransferNamesAndCommands");
        // abstract now 
        /*
        assertNotNull(new FtpFileTransferNamesAndCommands(ftp));
        assertNotNull(new FtpFileTransferNamesAndCommands(new FtpFileTransferNamesAndCommands(ftp), new FtpFileConfiguration(ftp)));
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommands(null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommands(null, null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommands(null, new FtpFileConfiguration(ftp)));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommands(new FtpFileTransferNamesAndCommands(ftp), null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        */
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
