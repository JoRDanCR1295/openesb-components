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
 * @(#)FtpFileTransferNamesAndCommandsPutTest.java 
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
public class FtpFileTransferNamesAndCommandsPutTest extends TestCase {
    FtpFileTransferNamesAndCommandsPut instance;
    FtpInterface ftp;
    
    public FtpFileTransferNamesAndCommandsPutTest(String testName) {
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
        instance = new FtpFileTransferNamesAndCommandsPut(ftp);
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
        TestSuite suite = new TestSuite(FtpFileTransferNamesAndCommandsPutTest.class);
        
        return suite;
    }

    /**
     * Test of resolveTargetLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut.
     */
    public void testResolveTargetLocation() throws Exception {
        System.out.println("resolveTargetLocation");
        
        instance.resolveTargetLocation();
        
    }

    /**
     * Test of resolveStageLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut.
     */
    public void testResolveStageLocation() throws Exception {
        System.out.println("resolveStageLocation");
        
        instance.resolveStageLocation();
        
    }

    /**
     * Test of getStageDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut.
     */
    public void testGetStageDirectoryName() throws Exception {
        System.out.println("getStageDirectoryName");
        
        assertEquals(null, instance.getStageDirectoryName());
        
    }

    /**
     * Test of getStageFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut.
     */
    public void testGetStageFileName() throws Exception {
        System.out.println("getStageFileName");
        
        assertEquals(null, instance.getStageFileName());
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut.
     */
    public void testFtpFileTransferNamesAndCommandsPut() throws Exception {
        System.out.println("FtpFileTransferNamesAndCommandsPut");
        
        assertNotNull(new FtpFileTransferNamesAndCommandsPut(ftp));
        assertNotNull(new FtpFileTransferNamesAndCommandsPut(new FtpFileTransferNamesAndCommandsPut(ftp), new FtpFileConfiguration(ftp)));
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommandsPut(null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommandsPut(null, null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommandsPut(null, new FtpFileConfiguration(ftp)));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        try {
            assertNotNull(new FtpFileTransferNamesAndCommandsPut(new FtpFileTransferNamesAndCommandsPut(ftp), null));
            fail("An exception is expected - invalid input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
