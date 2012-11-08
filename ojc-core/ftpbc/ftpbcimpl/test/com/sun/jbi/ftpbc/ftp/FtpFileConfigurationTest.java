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
 * @(#)FtpFileConfigurationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
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
public class FtpFileConfigurationTest extends TestCase {
    FtpFileConfiguration instance;
    FtpInterface ftp;
    
    public FtpFileConfigurationTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        
        Properties props = new Properties();
        props.put("General Settings/Connection Mode", "Manual");
        //!no automatic connection, so host/port etc will NOT be used to communicate with external
        //!props.put("FTP/Host Name", "ftp.microsoft.com"); // "ftp.microsoft.com", "atlas.stc.com"
        //!props.put("FTP/Directory Listing Style", "UNIX");
        
        ftp = new FtpInterface();
        ftp.initialize(props);
        
        boolean old = false;
        if (old) {
            instance = new FtpFileConfiguration(ftp);
        } else {
            instance = ftp.getConfiguration();
        }
        
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
        TestSuite suite = new TestSuite(FtpFileConfigurationTest.class);
        
        return suite;
    }
    
    /**
     * Test of getDirectoryListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetDirectoryListingStyle() throws Exception {
        System.out.println("getDirectoryListingStyle");
        
        String expResult = "UNIX";
        String result = instance.getDirectoryListingStyle();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getUserDefinedDirectoryListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetUserDefinedDirectoryListingStyle() throws Exception {
        System.out.println("getUserDefinedDirectoryListingStyle");
        
        String expResult = "";
        String result = instance.getUserDefinedDirectoryListingStyle();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getHostName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetHostName() throws Exception {
        System.out.println("getHostName");
        
        String expResult = "localhost";
        String result = instance.getHostName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetMode() throws Exception {
        System.out.println("getMode");
        
        String expResult = "Binary";
        String result = instance.getMode();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPassword() throws Exception {
        System.out.println("getPassword");
        
        String expResult = "";
        String result = instance.getPassword();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTargetDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetTargetDirectoryName() throws Exception {
        System.out.println("getTargetDirectoryName");
        
        String expResult = "";
        String result = instance.getTargetDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getServerPort method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetServerPort() throws Exception {
        System.out.println("getServerPort");
        
        int expResult = 21;
        int result = instance.getServerPort();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getUserName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetUserName() throws Exception {
        System.out.println("getUserName");
        
        String expResult = "anonymous";
        String result = instance.getUserName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setDirectoryListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetDirectoryListingStyle() throws Exception {
        System.out.println("setDirectoryListingStyle");
        
        String newDirectoryListingStyle = "WrongValue";
        try {
            instance.setDirectoryListingStyle(newDirectoryListingStyle);
            fail("An exception is expected - Wrong style value");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newDirectoryListingStyle = "MVS PDS";
        instance.setDirectoryListingStyle(newDirectoryListingStyle);
        assertEquals(newDirectoryListingStyle, instance.getDirectoryListingStyle());
    }
    
    /**
     * Test of setUserDefinedDirectoryListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetUserDefinedDirectoryListingStyle() throws Exception {
        System.out.println("setUserDefinedDirectoryListingStyle");
        
        String newUserDefinedDirectoryListingStyle = null;
        
        instance.setUserDefinedDirectoryListingStyle(newUserDefinedDirectoryListingStyle);
        assertEquals("null is converted to empty string internally ", "", instance.getUserDefinedDirectoryListingStyle());

        newUserDefinedDirectoryListingStyle = "Blah";
        try {
            instance.setUserDefinedDirectoryListingStyle(newUserDefinedDirectoryListingStyle);
            fail("An exception is expected - no user file is specified");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of setHostName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetHostName() throws Exception {
        System.out.println("setHostName");
        
        String newHostName = null;
        try {
            instance.setHostName(newHostName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newHostName = "new.host";
        instance.setHostName(newHostName);
        assertEquals(newHostName, instance.getHostName());
    }
    
    /**
     * Test of setMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetMode() throws Exception {
        System.out.println("setMode");
        
        String newMode = "Wrong";

        try {
            instance.setMode(newMode);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newMode = "Ebcdic";
        instance.setMode(newMode);
        assertEquals(newMode, instance.getMode());
        
    }
    
    /**
     * Test of setPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPassword() throws Exception {
        System.out.println("setPassword");
        
        String newPassword = null;
        
        instance.setPassword(newPassword);
        assertEquals("null is converted to empty string internally ", "", instance.getPassword());
        
        newPassword = "new value";
        
        instance.setPassword(newPassword);
        assertEquals(newPassword, instance.getPassword());
        
    }
    
    /**
     * Test of setTargetDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetTargetDirectoryName() throws Exception {
        System.out.println("setTargetDirectoryName");
        
        String newTargetDirectoryName = null;
        
        instance.setTargetDirectoryName(newTargetDirectoryName);
        assertEquals("null is converted to empty string internally ", "", instance.getTargetDirectoryName());
        
        newTargetDirectoryName = "/my/dir";
        
        instance.setTargetDirectoryName(newTargetDirectoryName);
        assertEquals(newTargetDirectoryName, instance.getTargetDirectoryName());
        
    }
    
    /**
     * Test of setServerPort method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetServerPort() throws Exception {
        System.out.println("setServerPort");
        
        int newServerPort = 0;
        
        try {
            instance.setServerPort(newServerPort);
            fail("An exception is expected - port number should be > 1.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newServerPort = 65536;
        try {
            instance.setServerPort(newServerPort);
            fail("An exception is expected - port number should be <= 65535.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        newServerPort = 2121;
        instance.setServerPort(newServerPort);
        assertEquals(newServerPort, instance.getServerPort());
        
    }
    
    /**
     * Test of setUserName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetUserName() throws Exception {
        System.out.println("setUserName");
        
        String newUserName = null;
        try {
            instance.setUserName(newUserName);
            fail("An exception is expected - UserName is required.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newUserName = "username";
        instance.setUserName(newUserName);
        assertEquals(newUserName, instance.getUserName());
        
    }
    
    /**
     * Test of getPostTransferRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostTransferRawCommands() throws Exception {
        System.out.println("getPostTransferRawCommands");
        
        String expResult = "";
        String result = instance.getPostTransferRawCommands();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreTransferRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreTransferRawCommands() throws Exception {
        System.out.println("getPreTransferRawCommands");
        
        String expResult = "";
        String result = instance.getPreTransferRawCommands();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTargetFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetTargetFileName() throws Exception {
        System.out.println("getTargetFileName");
        
        String expResult = "";
        String result = instance.getTargetFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isUsePASV method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsUsePASV() throws Exception {
        System.out.println("isUsePASV");
        
        boolean expResult = true;
        boolean result = instance.isUsePASV();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setPostTransferRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostTransferRawCommands() throws Exception {
        System.out.println("setPostTransferRawCommands");
        
        String newPostTransferRawCommands = null;
        
        instance.setPostTransferRawCommands(newPostTransferRawCommands);
        assertEquals("null is converted to empty string internally ", "", instance.getPostTransferRawCommands());
        
        newPostTransferRawCommands = "CWD /";
        
        instance.setPostTransferRawCommands(newPostTransferRawCommands);
        assertEquals(newPostTransferRawCommands, instance.getPostTransferRawCommands());
        
    }
    
    /**
     * Test of setPreTransferRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreTransferRawCommands() throws Exception {
        System.out.println("setPreTransferRawCommands");
        
        String newPreTransferRawCommands = null;
        
        instance.setPreTransferRawCommands(newPreTransferRawCommands);
        assertEquals("null is converted to empty string internally ", "", instance.getPreTransferRawCommands());
        
        newPreTransferRawCommands = "SITE ...";
        
        instance.setPreTransferRawCommands(newPreTransferRawCommands);
        assertEquals(newPreTransferRawCommands, instance.getPreTransferRawCommands());
        
    }
    
    /**
     * Test of setTargetFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetTargetFileName() throws Exception {
        System.out.println("setTargetFileName");
        
        String newTargetFileName = null;
        
        instance.setTargetFileName(newTargetFileName);
        assertEquals("null is converted to empty string internally ", "", instance.getTargetFileName());
        
        newTargetFileName = "/my/dir";
        
        instance.setTargetFileName(newTargetFileName);
        assertEquals(newTargetFileName, instance.getTargetFileName());
        
    }
    
    /**
     * Test of setUsePASV method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetUsePASV() throws Exception {
        System.out.println("setUsePASV");
        
        boolean newUsePASV = false;
        
        instance.setUsePASV(newUsePASV);
        assertFalse(instance.getUsePASV());
        
    }
    
    /**
     * Test of getMaxSequenceNumber method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetMaxSequenceNumber() throws Exception {
        System.out.println("getMaxSequenceNumber");
        
        long expResult = 999999;
        long result = instance.getMaxSequenceNumber();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getStartingSequenceNumber method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetStartingSequenceNumber() throws Exception {
        System.out.println("getStartingSequenceNumber");
        
        long expResult = 1;
        long result = instance.getStartingSequenceNumber();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setMaxSequenceNumber method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetMaxSequenceNumber() throws Exception {
        System.out.println("setMaxSequenceNumber");
        
        long newMaxSequenceNumber = 0L;
        try {
            instance.setMaxSequenceNumber(newMaxSequenceNumber);
            fail("An exception is expected - it should be > 0");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newMaxSequenceNumber = Long.MAX_VALUE;
        try {
            instance.setMaxSequenceNumber(newMaxSequenceNumber);
            fail("An exception is expected - it should be <= Integer.MAX_VALUE");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newMaxSequenceNumber = 123456;
        instance.setMaxSequenceNumber(newMaxSequenceNumber);
        assertEquals(newMaxSequenceNumber, instance.getMaxSequenceNumber());
        
    }
    
    /**
     * Test of setStartingSequenceNumber method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetStartingSequenceNumber() throws Exception {
        System.out.println("setStartingSequenceNumber");
        
        long newStartingSequenceNumber = -1;
        
        try {
            instance.setStartingSequenceNumber(newStartingSequenceNumber);
            fail("An exception is expected - it should be >= 0");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newStartingSequenceNumber = Long.MAX_VALUE;
        try {
            instance.setStartingSequenceNumber(newStartingSequenceNumber);
            fail("An exception is expected - it should be <= Integer.MAX_VALUE");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newStartingSequenceNumber = 123456;
        instance.setStartingSequenceNumber(newStartingSequenceNumber);
        assertEquals(newStartingSequenceNumber, instance.getStartingSequenceNumber());
        
        
    }
    
    /**
     * Test of setEncryptedPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetEncryptedPassword() throws Exception {
        System.out.println("setEncryptedPassword");
        
        String newPassword = null;
        try {
            instance.setEncryptedPassword(newPassword);
            fail("An exception is expected - the password cannot be decrypted");
        } catch (FtpFileException ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of getSocksServerPort method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksServerPort() throws Exception {
        System.out.println("getSocksServerPort");
        
        int expResult = 1080;
        int result = instance.getSocksServerPort();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksVersion method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksVersion() throws Exception {
        System.out.println("getSocksVersion");
        
        int expResult = -1;
        int result = instance.getSocksVersion();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSocksServerPort method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksServerPort() throws Exception {
        System.out.println("setSocksServerPort");
        
        int newSocksServerPort = 0;
        
        try {
            instance.setSocksServerPort(newSocksServerPort);
            fail("An exception is expected - port number should be > 1.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newSocksServerPort = 65536;
        try {
            instance.setSocksServerPort(newSocksServerPort);
            fail("An exception is expected - port number should be <= 65535.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        newSocksServerPort = 2080;
        instance.setSocksServerPort(newSocksServerPort);
        assertEquals(newSocksServerPort, instance.getSocksServerPort());
        
    }
    
    /**
     * Test of setSocksVersion method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksVersion() throws Exception {
        System.out.println("setSocksVersion");
        
        int newSocksVersion = 1;

        try {
            instance.setSocksVersion(newSocksVersion);
            fail("An exception is expected - version must be 4 | 5 | -1.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newSocksVersion = 4;
        instance.setSocksVersion(newSocksVersion);
        assertEquals(newSocksVersion, instance.getSocksVersion());
        
        newSocksVersion = 5;
        instance.setSocksVersion(newSocksVersion);
        assertEquals(newSocksVersion, instance.getSocksVersion());
        
        newSocksVersion = -1;
        instance.setSocksVersion(newSocksVersion);
        assertEquals(newSocksVersion, instance.getSocksVersion());
    }
    
    /**
     * Test of getClientClassName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetClientClassName() throws Exception {
        System.out.println("getClientClassName");
        
        String expResult = "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl";
        String result = instance.getClientClassName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setClientClassName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetClientClassName() throws Exception {
        System.out.println("setClientClassName");
        
        String newClientClassName = null;
        instance.setClientClassName(newClientClassName);
        assertEquals("null or empty value makes default value used", "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl", instance.getClientClassName());
        
        newClientClassName = "not.exist";
        try {
            instance.setClientClassName(newClientClassName);
            fail("An exception is expected - invalid value - " + newClientClassName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newClientClassName = "java.lang.String";
        try {
            instance.setClientClassName(newClientClassName);
            fail("An exception is expected - invalid value - " + newClientClassName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * Test of getUsePASV method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetUsePASV() throws Exception {
        System.out.println("getUsePASV");
        
        boolean expResult = true;
        boolean result = instance.getUsePASV();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksHostName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksHostName() throws Exception {
        System.out.println("getSocksHostName");
        
        String expResult = "";
        String result = instance.getSocksHostName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksPassword() throws Exception {
        System.out.println("getSocksPassword");
        
        String expResult = "";
        String result = instance.getSocksPassword();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksUserName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksUserName() throws Exception {
        System.out.println("getSocksUserName");
        
        String expResult = "";
        String result = instance.getSocksUserName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSocksEncryptedPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksEncryptedPassword() throws Exception {
        System.out.println("setSocksEncryptedPassword");
        
        String newSocksPassword = "Abdfd45";
        try {
            instance.setSocksEncryptedPassword(newSocksPassword);
            fail("An exception is expected - invalid value - an encrypted value is needed to be decrypted.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of setSocksHostName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksHostName() throws Exception {
        System.out.println("setSocksHostName");
        
        String newSocksHostName = "a.host";
        
        instance.setSocksHostName(newSocksHostName);
        assertEquals(newSocksHostName, instance.getSocksHostName());
        
    }
    
    /**
     * Test of setSocksPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksPassword() throws Exception {
        System.out.println("setSocksPassword");
        
        String newSocksPassword = "123Sdf";
        
        instance.setSocksPassword(newSocksPassword);
        assertEquals(newSocksPassword, instance.getSocksPassword());
        
    }
    
    /**
     * Test of setSocksUserName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksUserName() throws Exception {
        System.out.println("setSocksUserName");
        
        String newSocksUserName = null;
        instance.setSocksUserName(newSocksUserName);
        assertEquals("", instance.getSocksUserName());
        
        newSocksUserName = "bill";
        
        instance.setSocksUserName(newSocksUserName);
        assertEquals(newSocksUserName, instance.getSocksUserName());
        
    }
    
    /**
     * Test of getSocksEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksEnabled() throws Exception {
        System.out.println("getSocksEnabled");
        
        boolean expResult = false;
        boolean result = instance.getSocksEnabled();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSocksEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSocksEnabled() throws Exception {
        System.out.println("isSocksEnabled");
        
        boolean expResult = false;
        boolean result = instance.isSocksEnabled();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSocksEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSocksEnabled() throws Exception {
        System.out.println("setSocksEnabled");
        
        boolean newSocksEnabled = true;
        
        instance.setSocksEnabled(newSocksEnabled);
        assertTrue(instance.getSocksEnabled());
        
    }
    
    /**
     * Test of getCommandConnectionTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetCommandConnectionTimeout() throws Exception {
        System.out.println("getCommandConnectionTimeout");
        
        int expResult = 45000;
        int result = instance.getCommandConnectionTimeout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getDataConnectionTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetDataConnectionTimeout() throws Exception {
        System.out.println("getDataConnectionTimeout");
        
        int expResult = 45000;
        int result = instance.getDataConnectionTimeout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setCommandConnectionTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetCommandConnectionTimeout() throws Exception {
        System.out.println("setCommandConnectionTimeout");
        
        int newCommandConnectionTimeout = 100;
        
        instance.setCommandConnectionTimeout(newCommandConnectionTimeout);
        assertEquals(newCommandConnectionTimeout, instance.getCommandConnectionTimeout());
        
    }
    
    /**
     * Test of setDataConnectionTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetDataConnectionTimeout() throws Exception {
        System.out.println("setDataConnectionTimeout");
        
        int newDataConnectionTimeout = 101;
        
        instance.setDataConnectionTimeout(newDataConnectionTimeout);
        assertEquals(newDataConnectionTimeout, instance.getDataConnectionTimeout());
        
    }
    
    /**
     * Test of getProviderClassName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetProviderClassName() throws Exception {
        System.out.println("getProviderClassName");
        
        String expResult = "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl";
        String result = instance.getProviderClassName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setTargetDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetTargetDirectoryNameIsPattern() throws Exception {
        System.out.println("setTargetDirectoryNameIsPattern");
        
        boolean newTargetDirectoryNameIsPattern = true;
        
        instance.setTargetDirectoryNameIsPattern(newTargetDirectoryNameIsPattern);
        assertTrue(instance.getTargetDirectoryNameIsPattern());
        
    }
    
    /**
     * Test of getAppend method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetAppend() throws Exception {
        System.out.println("getAppend");
        
        boolean expResult = false;
        boolean result = instance.getAppend();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostTransferCommand() throws Exception {
        System.out.println("getPostTransferCommand");
        
        String expResult = "None";
        String result = instance.getPostTransferCommand();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostDirectoryName() throws Exception {
        System.out.println("getPostDirectoryName");
        
        String expResult = "";
        String result = instance.getPostDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostDirectoryNameIsPattern() throws Exception {
        System.out.println("getPostDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.getPostDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreTransferCommand() throws Exception {
        System.out.println("getPreTransferCommand");
        
        String expResult = "None";
        String result = instance.getPreTransferCommand();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreDirectoryName() throws Exception {
        System.out.println("getPreDirectoryName");
        
        String expResult = "";
        String result = instance.getPreDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreDirectoryNameIsPattern() throws Exception {
        System.out.println("getPreDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.getPreDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTargetDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetTargetDirectoryNameIsPattern() throws Exception {
        System.out.println("getTargetDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.getTargetDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getStatePersistenceBaseLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetStatePersistenceBaseLocation() throws Exception {
        System.out.println("getStatePersistenceBaseLocation");
        
        String expResult = System.getProperty("user.home", "/temp");
        String result = instance.getStatePersistenceBaseLocation();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setAppend method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetAppend() throws Exception {
        System.out.println("setAppend");
        
        boolean newAppend = true;
        
        instance.setAppend(newAppend);
        assertTrue(instance.getAppend());
        
    }
    
    /**
     * Test of setConnectionEstablishmentMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetConnectionEstablishmentMode() throws Exception {
        System.out.println("setConnectionEstablishmentMode");
        
        String newConnectionEstablishmentMode = null;
        try {
            instance.setConnectionEstablishmentMode(newConnectionEstablishmentMode);
            fail("An exception is expected - invalid mode - " + newConnectionEstablishmentMode);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newConnectionEstablishmentMode = "WrongMode";
        try {
            instance.setConnectionEstablishmentMode(newConnectionEstablishmentMode);
            fail("An exception is expected - invalid mode - " + newConnectionEstablishmentMode);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newConnectionEstablishmentMode = "Automatic";
        instance.setConnectionEstablishmentMode(newConnectionEstablishmentMode);
        assertEquals(newConnectionEstablishmentMode, instance.getConnectionEstablishmentMode());
        
        newConnectionEstablishmentMode = "Manual";
        instance.setConnectionEstablishmentMode(newConnectionEstablishmentMode);
        assertEquals(newConnectionEstablishmentMode, instance.getConnectionEstablishmentMode());
        
    }
    
    /**
     * Test of setPostTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostTransferCommand() throws Exception {
        System.out.println("setPostTransferCommand");
        
        String newPostTransferCommand = null;

        try {
            instance.setPostTransferCommand(newPostTransferCommand);
            fail("An exception is expected - invalid command - " + newPostTransferCommand);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newPostTransferCommand = "WrongCommand";
        try {
            instance.setPostTransferCommand(newPostTransferCommand);
            fail("An exception is expected - invalid command - " + newPostTransferCommand);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newPostTransferCommand = "delete";
        instance.setPostTransferCommand(newPostTransferCommand);
        assertEquals(newPostTransferCommand, instance.getPostTransferCommand());
        
        newPostTransferCommand = "rename";
        instance.setPostTransferCommand(newPostTransferCommand);
        assertEquals(newPostTransferCommand, instance.getPostTransferCommand());
        
        newPostTransferCommand = "none";
        instance.setPostTransferCommand(newPostTransferCommand);
        assertEquals(newPostTransferCommand, instance.getPostTransferCommand());
        
    }
    
    /**
     * Test of setPostDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostDirectoryName() throws Exception {
        System.out.println("setPostDirectoryName");
        
        String newPostDirectoryName = null;
        
        instance.setPostDirectoryName(newPostDirectoryName);
        assertEquals("null is converted to empty string internally ", "", instance.getPostDirectoryName());
        
        newPostDirectoryName = "/my/dir";
        
        instance.setPostDirectoryName(newPostDirectoryName);
        assertEquals(newPostDirectoryName, instance.getPostDirectoryName());
        
    }
    
    /**
     * Test of setPostDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostDirectoryNameIsPattern() throws Exception {
        System.out.println("setPostDirectoryNameIsPattern");
        
        boolean newPostDirectoryNameIsPattern = true;
        
        instance.setPostDirectoryNameIsPattern(newPostDirectoryNameIsPattern);
        assertTrue(instance.getPostDirectoryNameIsPattern());
        
    }
    
    /**
     * Test of setPreTransferCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreTransferCommand() throws Exception {
        System.out.println("setPreTransferCommand");
        
        String newPreTransferCommand = null;
        
        try {
            instance.setPreTransferCommand(newPreTransferCommand);
            fail("An exception is expected - invalid command - " + newPreTransferCommand);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newPreTransferCommand = "WrongCommand";
        try {
            instance.setPreTransferCommand(newPreTransferCommand);
            fail("An exception is expected - invalid command - " + newPreTransferCommand);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newPreTransferCommand = "copy";
        instance.setPreTransferCommand(newPreTransferCommand);
        assertEquals(newPreTransferCommand, instance.getPreTransferCommand());
        
        newPreTransferCommand = "rename";
        instance.setPreTransferCommand(newPreTransferCommand);
        assertEquals(newPreTransferCommand, instance.getPreTransferCommand());
        
        newPreTransferCommand = "none";
        instance.setPreTransferCommand(newPreTransferCommand);
        assertEquals(newPreTransferCommand, instance.getPreTransferCommand());
        
    }
    
    /**
     * Test of setPreDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreDirectoryName() throws Exception {
        System.out.println("setPreDirectoryName");
        
        String newPreDirectoryName = null;
        
        instance.setPreDirectoryName(newPreDirectoryName);
        assertEquals("null is converted to empty string internally ", "", instance.getPreDirectoryName());
        
        newPreDirectoryName = "/my/dir";
        
        instance.setPreDirectoryName(newPreDirectoryName);
        assertEquals(newPreDirectoryName, instance.getPreDirectoryName());
        
    }
    
    /**
     * Test of setPreDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreDirectoryNameIsPattern() throws Exception {
        System.out.println("setPreDirectoryNameIsPattern");
        
        boolean newPreDirectoryNameIsPattern = true;
        
        instance.setPreDirectoryNameIsPattern(newPreDirectoryNameIsPattern);
        assertTrue(instance.getPreDirectoryNameIsPattern());
        
    }
    
    /**
     * Test of setProviderClassName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetProviderClassName() throws Exception {
        System.out.println("setProviderClassName");
        
        String newProviderClassName = null;
        
        instance.setProviderClassName(newProviderClassName);
        assertEquals("null or empty value makes default value used", "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl", instance.getProviderClassName());
        
        newProviderClassName = "not.exist";
        try {
            instance.setProviderClassName(newProviderClassName);
            fail("An exception is expected - invalid value - " + newProviderClassName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newProviderClassName = "java.lang.String";
        try {
            instance.setProviderClassName(newProviderClassName);
            fail("An exception is expected - invalid value - " + newProviderClassName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of setTargetFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetTargetFileNameIsPattern() throws Exception {
        System.out.println("setTargetFileNameIsPattern");
        
        boolean newTargetFileNameIsPattern = false;
        
        instance.setTargetFileNameIsPattern(newTargetFileNameIsPattern);
        assertFalse(instance.getTargetFileNameIsPattern());
        
    }
    
    /**
     * Test of getStageEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetStageEnabled() throws Exception {
        System.out.println("getStageEnabled");
        
        boolean expResult = false;
        boolean result = instance.getStageEnabled();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isStageEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsStageEnabled() throws Exception {
        System.out.println("isStageEnabled");
        
        boolean expResult = false;
        boolean result = instance.isStageEnabled();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setStageEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetStageEnabled() throws Exception {
        System.out.println("setStageEnabled");
        
        boolean newStageEnabled = true;
        
        instance.setStageEnabled(newStageEnabled);
        assertEquals(newStageEnabled, instance.getStageEnabled());
        
    }
    
    /**
     * Test of getStageDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetStageDirectoryName() throws Exception {
        System.out.println("getStageDirectoryName");
        
        String expResult = "";
        String result = instance.getStageDirectoryName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getStageFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetStageFileName() throws Exception {
        System.out.println("getStageFileName");
        
        String expResult = "";
        String result = instance.getStageFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setStageDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetStageDirectoryName() throws Exception {
        System.out.println("setStageDirectoryName");
        
        String newStageDirectoryName = null;
        
        instance.setStageDirectoryName(newStageDirectoryName);
        assertEquals("null is converted to empty string internally ", "", instance.getStageDirectoryName());
        
        newStageDirectoryName = "/my/dir";
        
        instance.setStageDirectoryName(newStageDirectoryName);
        assertEquals(newStageDirectoryName, instance.getStageDirectoryName());
        
    }
    
    /**
     * Test of setStageFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetStageFileName() throws Exception {
        System.out.println("setStageFileyName");
        
        String newStageFileName = null;
        
        instance.setStageFileName(newStageFileName);
        assertEquals("null is converted to empty string internally ", "", instance.getStageFileName());
        
        newStageFileName = "my file";
        
        instance.setStageFileName(newStageFileName);
        assertEquals(newStageFileName, instance.getStageFileName());
        
    }
    
    /**
     * Test of getSequenceNumberPersistenceMedia method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSequenceNumberPersistenceMedia() throws Exception {
        System.out.println("getSequenceNumberPersistenceMedia");
        
        String expResult = FtpFileConfiguration.MEDIA_FLAT_FILE;
        String result = instance.getSequenceNumberPersistenceMedia();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSequenceNumberPersistenceFlatFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSequenceNumberPersistenceFlatFile() throws Exception {
        System.out.println("isSequenceNumberPersistenceFlatFile");
        
        boolean expResult = true;
        boolean result = instance.isSequenceNumberPersistenceFlatFile();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSequenceNumberPersistenceBinaryFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSequenceNumberPersistenceBinaryFile() throws Exception {
        System.out.println("isSequenceNumberPersistenceBinaryFile");
        
        boolean expResult = false;
        boolean result = instance.isSequenceNumberPersistenceBinaryFile();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSequenceNumberPersistenceDB method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSequenceNumberPersistenceDB() throws Exception {
        System.out.println("isSequenceNumberPersistenceDB");
        
        boolean expResult = false;
        boolean result = instance.isSequenceNumberPersistenceDB();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSequenceNumberPersistenceMedia method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSequenceNumberPersistenceMedia() throws Exception {
        System.out.println("setSequenceNumberPersistenceMedia");
        
        String newSequenceNumberPersistenceMedia = null;
        try {
            instance.setSequenceNumberPersistenceMedia(newSequenceNumberPersistenceMedia);
            fail("An exception is expected - invalid value - " + newSequenceNumberPersistenceMedia);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newSequenceNumberPersistenceMedia = "WrongMedia";
        try {
            instance.setSequenceNumberPersistenceMedia(newSequenceNumberPersistenceMedia);
            fail("An exception is expected - invalid value - " + newSequenceNumberPersistenceMedia);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        newSequenceNumberPersistenceMedia = "FlatFile";
        instance.setSequenceNumberPersistenceMedia(newSequenceNumberPersistenceMedia);
        assertEquals(newSequenceNumberPersistenceMedia, instance.getSequenceNumberPersistenceMedia());
        
        newSequenceNumberPersistenceMedia = "BinaryFile";
        instance.setSequenceNumberPersistenceMedia(newSequenceNumberPersistenceMedia);
        assertEquals(newSequenceNumberPersistenceMedia, instance.getSequenceNumberPersistenceMedia());
        
        newSequenceNumberPersistenceMedia = "DB";
        instance.setSequenceNumberPersistenceMedia(newSequenceNumberPersistenceMedia);
        assertEquals(newSequenceNumberPersistenceMedia, instance.getSequenceNumberPersistenceMedia());
        
    }
    
    /**
     * Test of getConnectionEstablishmentMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetConnectionEstablishmentMode() throws Exception {
        System.out.println("getConnectionEstablishmentMode");
        
        String expResult = "Manual";
        String result = instance.getConnectionEstablishmentMode();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of initialConfigValues method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testInitialConfigValues() throws Exception {
        System.out.println("initialConfigValues");
        
        Properties props = null;
        try {
            instance.initialConfigValues(props);
            fail("An exception is expected - invalid input - " + props);
        } catch (FtpFileException ex) {
            ex.printStackTrace();
        }
        
        props = new Properties();
        try {
            instance.initialConfigValues(props);
        } catch (FtpFileException ex) {
            fail("No exception is expected");
        }
        
    }
    
    /**
     * Test of reset method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testReset() throws Exception {
        System.out.println("reset");
        
        instance.reset();
        
    }
    
    /**
     * Test of isAppend method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsAppend() throws Exception {
        System.out.println("isAppend");
        
        boolean expResult = false;
        boolean result = instance.isAppend();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPostDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPostDirectoryNameIsPattern() throws Exception {
        System.out.println("isPostDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.isPostDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPreDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPreDirectoryNameIsPattern() throws Exception {
        System.out.println("isPreDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.isPreDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isTargetDirectoryNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsTargetDirectoryNameIsPattern() throws Exception {
        System.out.println("isTargetDirectoryNameIsPattern");
        
        boolean expResult = false;
        boolean result = instance.isTargetDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTargetFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetTargetFileNameIsPattern() throws Exception {
        System.out.println("getTargetFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.getTargetFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isConnectionEstablishmentModeAutomatic method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsConnectionEstablishmentModeAutomatic() throws Exception {
        System.out.println("isConnectionEstablishmentModeAutomatic");
        
        boolean expResult = false;
        boolean result = instance.isConnectionEstablishmentModeAutomatic();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isConnectionEstablishmentModeManual method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsConnectionEstablishmentModeManual() throws Exception {
        System.out.println("isConnectionEstablishmentModeManual");
        
        boolean expResult = true;
        boolean result = instance.isConnectionEstablishmentModeManual();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isModeAscii method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsModeAscii() throws Exception {
        System.out.println("isModeAscii");
        
        boolean expResult = false;
        boolean result = instance.isModeAscii();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isModeBinary method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsModeBinary() throws Exception {
        System.out.println("isModeBinary");
        
        boolean expResult = true;
        boolean result = instance.isModeBinary();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isModeEbcdic method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsModeEbcdic() throws Exception {
        System.out.println("isModeEbcdic");
        
        boolean expResult = false;
        boolean result = instance.isModeEbcdic();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPostTransferCommandDelete method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPostTransferCommandDelete() throws Exception {
        System.out.println("isPostTransferCommandDelete");
        
        boolean expResult = false;
        boolean result = instance.isPostTransferCommandDelete();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPostTransferCommandNone method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPostTransferCommandNone() throws Exception {
        System.out.println("isPostTransferCommandNone");
        
        boolean expResult = true;
        boolean result = instance.isPostTransferCommandNone();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPostTransferCommandRename method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPostTransferCommandRename() throws Exception {
        System.out.println("isPostTransferCommandRename");
        
        boolean expResult = false;
        boolean result = instance.isPostTransferCommandRename();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPreTransferCommandCopy method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPreTransferCommandCopy() throws Exception {
        System.out.println("isPreTransferCommandCopy");
        
        boolean expResult = false;
        boolean result = instance.isPreTransferCommandCopy();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPreTransferCommandNone method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPreTransferCommandNone() throws Exception {
        System.out.println("isPreTransferCommandNone");
        
        boolean expResult = true;
        boolean result = instance.isPreTransferCommandNone();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPreTransferCommandRename method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPreTransferCommandRename() throws Exception {
        System.out.println("isPreTransferCommandRename");
        
        boolean expResult = false;
        boolean result = instance.isPreTransferCommandRename();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSocksVersion4 method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSocksVersion4() throws Exception {
        System.out.println("isSocksVersion4");
        
        boolean expResult = false;
        boolean result = instance.isSocksVersion4();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSocksVersion5 method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSocksVersion5() throws Exception {
        System.out.println("isSocksVersion5");
        
        boolean expResult = false;
        boolean result = instance.isSocksVersion5();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSocksVersionUnknown method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsSocksVersionUnknown() throws Exception {
        System.out.println("isSocksVersionUnknown");
        
        boolean expResult = true;
        boolean result = instance.isSocksVersionUnknown();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostFileName() throws Exception {
        System.out.println("getPostFileName");
        
        String expResult = "";
        String result = instance.getPostFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPostFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostFileNameIsPattern() throws Exception {
        System.out.println("getPostFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.getPostFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreFileName() throws Exception {
        System.out.println("getPreFileName");
        
        String expResult = "";
        String result = instance.getPreFileName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getPreFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreFileNameIsPattern() throws Exception {
        System.out.println("getPreFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.getPreFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPostFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPostFileNameIsPattern() throws Exception {
        System.out.println("isPostFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.isPostFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPreFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsPreFileNameIsPattern() throws Exception {
        System.out.println("isPreFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.isPreFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isTargetFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testIsTargetFileNameIsPattern() throws Exception {
        System.out.println("isTargetFileNameIsPattern");
        
        boolean expResult = true;
        boolean result = instance.isTargetFileNameIsPattern();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setPostFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostFileName() throws Exception {
        System.out.println("setPostFileName");
        
        String newPostFileName = null;
        instance.setPostFileName(newPostFileName);
        assertEquals("", instance.getPostFileName());
        
        newPostFileName = "file_abc";
        instance.setPostFileName(newPostFileName);
        assertEquals(newPostFileName, instance.getPostFileName());
        
    }
    
    /**
     * Test of setPostFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPostFileNameIsPattern() throws Exception {
        System.out.println("setPostFileNameIsPattern");
        
        boolean newPostFileNameIsPattern = false;
        instance.setPostFileNameIsPattern(newPostFileNameIsPattern);
        assertFalse(instance.getPostFileNameIsPattern());
        
    }
    
    /**
     * Test of setPreFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreFileName() throws Exception {
        System.out.println("setPreFileName");
        
        String newPreFileName = null;
        instance.setPreFileName(newPreFileName);
        assertEquals("", instance.getPreFileName());
        
        newPreFileName = "a_file";
        instance.setPreFileName(newPreFileName);
        assertEquals(newPreFileName, instance.getPreFileName());
        
    }
    
    /**
     * Test of setPreFileNameIsPattern method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetPreFileNameIsPattern() throws Exception {
        System.out.println("setPreFileNameIsPattern");
        
        boolean newPreFileNameIsPattern = false;
        instance.setPreFileNameIsPattern(newPreFileNameIsPattern);
        assertFalse(instance.getPreFileNameIsPattern());
        
    }
    
    /**
     * Test of getConnector method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetConnector() throws Exception {
        System.out.println("getConnector");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getConnector();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getEncryptedPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetEncryptedPassword() throws Exception {
        System.out.println("getEncryptedPassword");
        
        assertNotNull(instance.getEncryptedPassword());
        
    }
    
    /**
     * Test of getExtensions method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetExtensions() throws Exception {
        System.out.println("getExtensions");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getExtensions();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getFtp method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetFtp() throws Exception {
        System.out.println("getFtp");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getFtp();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getFTPRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetFTPRawCommands() throws Exception {
        System.out.println("getFTPRawCommands");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getFTPRawCommands();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getGeneralSettings method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetGeneralSettings() throws Exception {
        System.out.println("getGeneralSettings");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getGeneralSettings();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getPostTransfer method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPostTransfer() throws Exception {
        System.out.println("getPostTransfer");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getPostTransfer();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getPreTransfer method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetPreTransfer() throws Exception {
        System.out.println("getPreTransfer");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getPreTransfer();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getSequenceNumbering method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSequenceNumbering() throws Exception {
        System.out.println("getSequenceNumbering");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getSequenceNumbering();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getSOCKS method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSOCKS() throws Exception {
        System.out.println("getSOCKS");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getSOCKS();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getSocksEncryptedPassword method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSocksEncryptedPassword() throws Exception {
        System.out.println("getSocksEncryptedPassword");
        
        assertNotNull(instance.getSocksEncryptedPassword());
        
    }
    
    /**
     * Test of getTargetLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetTargetLocation() throws Exception {
        System.out.println("getTargetLocation");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getTargetLocation();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of getDynamicConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetDynamicConfiguration() throws Exception {
        System.out.println("getDynamicConfiguration");
        
        FtpFileConfiguration expResult = instance;
        FtpFileConfiguration result = instance.getDynamicConfiguration();
        assertSame(expResult, result);
        
    }
    
    /**
     * Test of setUserHeuristicsLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetUserHeuristicsLocation() throws Exception {
        System.out.println("setUserHeuristicsLocation");
        
        String userHeuristicLoc = null;
        instance.setUserHeuristicsLocation(userHeuristicLoc);
        assertEquals("No any validation here", userHeuristicLoc, instance.getUserHeuristicsLocation());
        
        userHeuristicLoc = "/some/location";
        instance.setUserHeuristicsLocation(userHeuristicLoc);
        assertEquals("No any validation here", userHeuristicLoc, instance.getUserHeuristicsLocation());
        
    }
    
    /**
     * Test of getUserHeuristicsLocation method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetUserHeuristicsLocation() throws Exception {
        System.out.println("getUserHeuristicsLocation");
        
        String expResult = "";
        String result = instance.getUserHeuristicsLocation();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setMaxRetry method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetMaxRetry() throws Exception {
        System.out.println("setMaxRetry");
        
        long aRetry = 0L;
        instance.setMaxRetry(aRetry);
        assertEquals("No any validation here", aRetry, instance.getMaxRetry());
        
        aRetry = -1;
        instance.setMaxRetry(aRetry);
        assertEquals("No any validation here", aRetry, instance.getMaxRetry());
        
    }
    
    /**
     * Test of getMaxRetry method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetMaxRetry() throws Exception {
        System.out.println("getMaxRetry");
        
        long expResult = 0L;
        long result = instance.getMaxRetry();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setRetryInterval method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetRetryInterval() throws Exception {
        System.out.println("setRetryInterval");
        
        long aInterval = 0L;
        instance.setRetryInterval(aInterval);
        assertEquals("No any validation here", aInterval, instance.getRetryInterval());
        
        aInterval = -2;
        instance.setRetryInterval(aInterval);
        assertEquals("No any validation here", aInterval, instance.getRetryInterval());
        
    }
    
    /**
     * Test of getRetryInterval method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetRetryInterval() throws Exception {
        System.out.println("getRetryInterval");
        
        long expResult = 1000L;
        long result = instance.getRetryInterval();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSynchronized method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testSetSynchronized() throws Exception {
        System.out.println("setSynchronized");
        
        boolean b = false;
        instance.setSynchronized(b);
        assertFalse(instance.getSynchronized());
        
    }
    
    /**
     * Test of getSynchronized method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetSynchronized() throws Exception {
        System.out.println("getSynchronized");
        
        boolean expResult = true;
        boolean result = instance.getSynchronized();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getOID method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetOID() throws Exception {
        System.out.println("getOID");
        
        String expResult = null;
        String result = instance.getOID();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getExternalName method, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testGetExternalName() throws Exception {
        System.out.println("getExternalName");
        
        String expResult = null;
        String result = instance.getExternalName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileConfiguration.
     */
    public void testFtpFileConfiguration() throws Exception {
        System.out.println("FtpFileConfiguration");
        
        assertNotNull(new FtpFileConfiguration(ftp));
        assertNotNull(new FtpFileConfiguration(new FtpInterface()));
        assertNotNull(new FtpFileConfiguration(null));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
