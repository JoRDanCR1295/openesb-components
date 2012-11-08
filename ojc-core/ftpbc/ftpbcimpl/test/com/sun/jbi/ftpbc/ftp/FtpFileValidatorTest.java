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
 * @(#)FtpFileValidatorTest.java 
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
public class FtpFileValidatorTest extends TestCase {
    FtpFileValidator instance;
    
    public FtpFileValidatorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        Properties props = new Properties();
        props.put("General Settings/Connection Mode", "Manual");
        
        FtpInterface ftp = new FtpInterface();
        ftp.initialize(props);
        instance = new FtpFileValidator(ftp.getConfiguration(), "");
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileValidatorTest.class);
        
        return suite;
    }

    /**
     * Test of getMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testGetMode() throws Exception {
        System.out.println("getMode");
        
        String expResult = "";
        String result = instance.getMode();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of setMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testSetMode() throws Exception {
        System.out.println("setMode");
        
        String vMode = "get";
        instance.setMode(vMode);
        assertEquals(vMode, instance.getMode());
        
    }

    /**
     * Test of getConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testGetConfiguration() throws Exception {
        System.out.println("getConfiguration");
        
        assertNotNull(instance.getConfiguration());
        
    }

    /**
     * Test of setConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testSetConfiguration() throws Exception {
        System.out.println("setConfiguration");
        
        FtpFileConfiguration configuration = null;
        
        instance.setConfiguration(configuration);
        assertEquals(configuration, instance.getConfiguration());
        
        configuration = new FtpFileConfiguration(null);
        
        instance.setConfiguration(configuration);
        assertEquals(configuration, instance.getConfiguration());
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate() throws Exception {
        System.out.println("validate");
        try {
            instance.validate();
        } catch (Exception ex) {
            ex.printStackTrace();
            fail("No exception is expected - no actual validation happens");
        }

    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_1() throws Exception {
        System.out.println("validate_1");
        
        instance.getConfiguration().setDirectoryListingStyle("MVS GDG");
        instance.getConfiguration().setPostTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - PostTransferCommand is not supported for MVS GDG.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_2() throws Exception {
        System.out.println("validate_2");
        
        instance.getConfiguration().setDirectoryListingStyle("MVS GDG");
        instance.getConfiguration().setPreTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - PreTransferCommand is not supported for MVS GDG");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_3() throws Exception {
        System.out.println("validate_3");
        
        instance.getConfiguration().setPreFileName("abc.pre");

        instance.getConfiguration().setPreTransferCommand("copy");
        try {
            instance.validate();
            fail("An exception is expected - The PreDirectoryName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        instance.getConfiguration().setPreTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - The PreDirectoryName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_4() throws Exception {
        System.out.println("validate_4");
        
        instance.getConfiguration().setPreDirectoryName("/abc");
        
        instance.getConfiguration().setPreTransferCommand("copy");
        try {
            instance.validate();
            fail("An exception is expected - The PreFileName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        instance.getConfiguration().setPreTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - The PreFileName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_5() throws Exception {
        System.out.println("validate_5");
        
        instance.getConfiguration().setPostDirectoryName("/abc");
        
        instance.getConfiguration().setPostTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - The PostFileName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_6() throws Exception {
        System.out.println("validate_6");
        
        instance.getConfiguration().setPostFileName("abc.post");
        
        instance.getConfiguration().setPostTransferCommand("rename");
        try {
            instance.validate();
            fail("An exception is expected - The PostDirectoryName cannot be empty");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_7() throws Exception {
        System.out.println("validate_7");
        
        instance.getConfiguration().setMaxSequenceNumber(100);
        instance.getConfiguration().setStartingSequenceNumber(200);
        try {
            instance.validate();
            fail("An exception is expected - The maxSequenceNumber is less than the startingSequenceNumber");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_8() throws Exception {
        System.out.println("validate_8");
        
        instance.getConfiguration().setSocksEnabled(true);
        instance.getConfiguration().setSocksHostName(null);
        try {
            instance.validate();
            fail("An exception is expected - The socksHostName shouldn't be empty for SOCKS setting");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of validate method, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testValidate_14() throws Exception {
        System.out.println("validate_14");
        
        instance.getConfiguration().setPostTransferCommand("delete");
        instance.setMode("put");
        try {
            instance.validate();
            fail("An exception is expected - That is invalid setting for outbound transfer");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileValidator.
     */
    public void testFtpFileValidator() throws Exception {
        System.out.println("FtpFileValidator");
        
        assertNotNull(new FtpFileValidator(new FtpFileConfiguration(new FtpInterface()), "get"));
        assertNotNull(new FtpFileValidator(new FtpFileConfiguration(new FtpInterface()), "put"));
        assertNotNull(new FtpFileValidator(new FtpFileConfiguration(new FtpInterface()), "other"));
        
        assertNotNull(new FtpFileValidator(null, null));
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
