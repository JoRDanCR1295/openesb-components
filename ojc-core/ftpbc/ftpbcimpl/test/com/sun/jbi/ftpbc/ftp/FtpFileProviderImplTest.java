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
 * @(#)FtpFileProviderImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.SocketException;
import java.util.Properties;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.commons.net.SocketFactory;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPFileListParser;

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
public class FtpFileProviderImplTest extends TestCase {
    FtpFileProviderImpl instance;
    FtpInterface ftp;
    
    public FtpFileProviderImplTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        boolean old = false;
        if(old) {
            instance = new FtpFileProviderImpl();
            instance.setDirListingStyle("UNIX");
            instance.connect(FtpTprops.FTP_TEST_HOST, 21);
            instance.login("anonymous", "test@test.com");
            //......
        } else {
            Properties props = new Properties();
            //props.put("General Settings/Connection Mode", "Manual");
            props.put("FTP/Host Name", FtpTprops.FTP_TEST_HOST);
            props.put("FTP/Directory Listing Style", "UNIX");
            
            ftp = new FtpInterface();
            ftp.initialize(props);
            instance = (FtpFileProviderImpl)ftp.getProvider();
        }
    }
    
    protected void tearDown() throws Exception {
        try {
            ftp.reset();
            instance.logout();
            instance.disConnect();
        } catch (Exception ex) {
            ;
        }
        ftp = null;
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileProviderImplTest.class);
        
        return suite;
    }
    
    /**
     * Test of listFiles method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFiles_files_listRealData() throws Exception {
        System.out.println("listFiles_files_listRealData");
        
        FTPFile[] files = null;
        boolean listRealData = true;
        assertNull(instance.listFiles(files, listRealData));
        
        files = new FTPFile[1];
        files[0] = new FTPFile();
        assertNull(instance.listFiles(files, listRealData));
        
        listRealData = false;
        FTPFile[] result = instance.listFiles(files, listRealData);
        assertNotNull(result);
    }
    
    /**
     * Test of listFiles method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFiles_pathName() throws Exception {
        System.out.println("listFiles_pathName");
        
        String pathName = "";
        assertNotNull(instance.listFiles(pathName));
        
    }
    
    /**
     * Test of listFiles method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFiles_files_fileType() throws Exception {
        System.out.println("listFiles_files_fileType");
        
        FTPFile[] files = null;
        int fileType = FTPFile.DIRECTORY_TYPE;
        assertNull(instance.listFiles(files, fileType));
        
        files = new FTPFile[1];
        files[0] = new FTPFile();
        assertNull(instance.listFiles(files, fileType));
        
        fileType = FTPFile.UNKNOWN_TYPE;
        FTPFile[] result = instance.listFiles(files, fileType);
        assertNotNull(result);
    }
    
    /**
     * Test of listFiles method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFiles_pathName_regExp() throws Exception {
        System.out.println("listFiles_pathName_regExp");
        
        String pathName = "";
        String regExp = "";
        assertNotNull(instance.listFiles(pathName, regExp));
        
    }
    
    /**
     * Test of listFiles method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFiles() throws Exception {
        System.out.println("listFiles");
        
        assertNotNull(instance.listFiles());
        
    }
    
    /**
     * Test of appendFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFile_remoteDirName_remoteBaseFileName_localFileName() throws Exception {
        System.out.println("appendFile_remoteDirName_remoteBaseFileName_localFileName");
        
        String remoteDirName = "";
        String remoteBaseFileName = "";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.appendFile(remoteDirName, remoteBaseFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of appendFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFile_remoteDirName_remoteBaseFileName_local() throws Exception {
        System.out.println("appendFile_remoteDirName_remoteBaseFileName_local");
        
        String remoteDirName = "";
        String remoteBaseFileName = "";
        InputStream local = null;
        boolean expResult = false;
        boolean result = instance.appendFile(remoteDirName, remoteBaseFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of appendFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFile_remoteFileName_localFileName() throws Exception {
        System.out.println("appendFile_remoteFileName_localFileName");
        
        String remoteFileName = "";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.appendFile(remoteFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of appendFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFile_remoteFileName_local() throws Exception {
        System.out.println("appendFile_remoteFileName_local");
        
        String remoteFileName = "";
        InputStream local = null;
        boolean expResult = false;
        boolean result = instance.appendFile(remoteFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of appendFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFileStream_remoteFileName() throws Exception {
        System.out.println("appendFileStream_remoteFileName");
        
        String remoteFileName = "";
        OutputStream expResult = null;
        OutputStream result = instance.appendFileStream(remoteFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of appendFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAppendFileStream_dirName_baseFileName() throws Exception {
        System.out.println("appendFileStream_dirName_baseFileName");
        
        String dirName = "";
        String baseFileName = "";
        OutputStream expResult = null;
        OutputStream result = instance.appendFileStream(dirName, baseFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of deleteFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testDeleteFile_remoteFileName() throws Exception {
        System.out.println("deleteFile_remoteFileName");
        
        String remoteFileName = "";
        boolean expResult = false;
        boolean result = instance.deleteFile(remoteFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of deleteFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testDeleteFile_dirName_baseFileName() throws Exception {
        System.out.println("deleteFile_dirName_baseFileName");
        
        String dirName = "";
        String baseFileName = "";
        boolean expResult = false;
        boolean result = instance.deleteFile(dirName, baseFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isTraceRawCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsTraceRawCommand() throws Exception {
        System.out.println("isTraceRawCommand");
        
        boolean expResult = false;
        boolean result = instance.isTraceRawCommand();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of rename method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRename_remoteFileNameFrom_remoteFileNameTo() throws Exception {
        System.out.println("rename_remoteFileNameFrom_remoteFileNameTo");

        String remoteFileNameFrom = "";
        String remoteFileNameTo = "";

        boolean expResult = false;
        boolean result = instance.rename(remoteFileNameFrom, remoteFileNameTo);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of rename method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRename_dirNameFrom_baseFileNameFrom_dirNameTo_baseFileNameTo() throws Exception {
        System.out.println("rename_dirNameFrom_baseFileNameFrom_dirNameTo_baseFileNameTo");
        
        String dirNameFrom = "";
        String baseFileNameFrom = "";
        String dirNameTo = "";
        String baseFileNameTo = "";
        boolean expResult = false;
        boolean result = instance.rename(dirNameFrom, baseFileNameFrom, dirNameTo, baseFileNameTo);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of retrieveFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFile_remoteDirName_remoteBaseFileName_localFileName() throws Exception {
        System.out.println("retrieveFile_remoteDirName_remoteBaseFileName_localFileName");
        
        String remoteDirName = "";
        String remoteBaseFileName = "";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.retrieveFile(remoteDirName, remoteBaseFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of retrieveFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFile_remoteFileName_localFileName() throws Exception {
        System.out.println("retrieveFile_remoteFileName_localFileName");
        
        String remoteFileName = "";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.retrieveFile(remoteFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of retrieveFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFile_remoteFileName_local() throws Exception {
        System.out.println("retrieveFile_remoteFileName_local");
        
        String remoteFileName = "";
        OutputStream local = null;
        boolean expResult = false;
        boolean result = instance.retrieveFile(remoteFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of retrieveFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFile_remoteDirName_remoteBaseFileName_local() throws Exception {
        System.out.println("retrieveFile_remoteDirName_remoteBaseFileName_local");
        
        String remoteDirName = "";
        String remoteBaseFileName = "";
        OutputStream local = null;
        boolean expResult = false;
        boolean result = instance.retrieveFile(remoteDirName, remoteBaseFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of retrieveFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFileStream_remoteFileName() throws Exception {
        System.out.println("retrieveFileStream_remoteFileName");
        
        String remoteFileName = "";
        InputStream expResult = null;
        InputStream result = instance.retrieveFileStream(remoteFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of retrieveFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRetrieveFileStream_dirName_baseFileName() throws Exception {
        System.out.println("retrieveFileStream_dirName_baseFileName");
        
        String dirName = "";
        String baseFileName = "";
        InputStream expResult = null;
        InputStream result = instance.retrieveFileStream(dirName, baseFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of sendCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSendCommand_command_parm() throws Exception {
        System.out.println("sendCommand_command_parm");
        
        String command = "";
        String parm = "";
        int expResult = 500;
        int result = instance.sendCommand(command, parm);
        assertEquals(expResult, result);
        
        command = "CWD";
        parm = "/";
        expResult = 250;
        result = instance.sendCommand(command, parm);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of sendCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSendCommand_command() throws Exception {
        System.out.println("sendCommand_command");
        
        String command = "PWD";
        int expResult = 257;
        int result = instance.sendCommand(command);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setTraceRawCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetTraceRawCommand() throws Exception {
        System.out.println("setTraceRawCommand");
        
        boolean newTraceRawCommand = true;
        instance.setTraceRawCommand(newTraceRawCommand);
        assertTrue(instance.isTraceRawCommand());
        
    }
    
    /**
     * Test of storeFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFile_remoteFileName_local() throws Exception {
        System.out.println("storeFile_remoteFileName_local");
        
        String remoteFileName = "";
        InputStream local = null;
        boolean expResult = false;
        boolean result = instance.storeFile(remoteFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of storeFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFile_remoteFileName_localFileName() throws Exception {
        System.out.println("storeFile_remoteFileName_localFileName");
        
        String remoteFileName = "";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.storeFile(remoteFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of storeFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFile_remoteDirName_remoteBaseFileName_local() throws Exception {
        System.out.println("storeFile_remoteDirName_remoteBaseFileName_local");
        
        String remoteDirName = "";
        String remoteBaseFileName = "";
        InputStream local = null;
        boolean expResult = false;
        boolean result = instance.storeFile(remoteDirName, remoteBaseFileName, local);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of storeFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFile_remoteDirName_remoteBaseFileName_localFileName() throws Exception {
        System.out.println("storeFile_remoteDirName_remoteBaseFileName_localFileName");
        
        String remoteDirName = "";
        String remoteBaseFileName ="";
        String localFileName = "";
        boolean expResult = true;
        boolean result;
        try {
            result = instance.storeFile(remoteDirName, remoteBaseFileName, localFileName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of storeFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFileStream_dirName_baseFileName() throws Exception {
        System.out.println("storeFileStream_dirName_baseFileName");
        
        String dirName = "";
        String baseFileName = "";
        OutputStream expResult = null;
        OutputStream result = instance.storeFileStream(dirName, baseFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of storeFileStream method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testStoreFileStream_remoteFileName() throws Exception {
        System.out.println("storeFileStream_remoteFileName");
        
        String remoteFileName = "";
        OutputStream expResult = null;
        OutputStream result = instance.storeFileStream(remoteFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getReplyCode method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetReplyCode() throws Exception {
        System.out.println("getReplyCode");
        
        int expResult = 214; // remotehelp
        int result = instance.getReplyCode();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getReplyString method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetReplyString() throws Exception {
        System.out.println("getReplyString");
        
        assertNotNull(instance.getReplyString()); // remotehelp
        
    }
    
    /**
     * Test of getReplyStrings method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetReplyStrings() throws Exception {
        System.out.println("getReplyStrings");
        
        String[] result = instance.getReplyStrings();
        assertNotNull(result); // remotehelp
        assertTrue(result.length > 0); // remotehelp
        
    }
    
    /**
     * Test of ascii method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testAscii() throws Exception {
        System.out.println("ascii");
        
        boolean expResult = true;
        boolean result = instance.ascii();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of binary method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testBinary() throws Exception {
        System.out.println("binary");
        
        boolean expResult = true;
        boolean result = instance.binary();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of cd method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testCd() throws Exception {
        System.out.println("cd");
        
        String dirName = "/";
        boolean expResult = true;
        boolean result = instance.cd(dirName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of connect method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testConnect_host_port() throws Exception {
        System.out.println("connect_host_port");
        
        String host = "";
        int port = 0;
        try {
            instance.connect(host, port);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = FtpTprops.FTP_TEST_HOST;
        port = 0;
        try {
            instance.connect(host, port);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = "not.existed.com";
        port = 21;
        try {
            instance.connect(host, port);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = FtpTprops.FTP_TEST_HOST;
        port = 21;
        instance.connect(host, port);
        
    }
    
    /**
     * Test of connect method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testConnect_host_port_encoding() throws Exception {
        System.out.println("connect_host_port_encoding");
        
        String host = "";
        int port = 0;
        String encoding = "UTF-8";
        try {
            instance.connect(host, port, encoding);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = FtpTprops.FTP_TEST_HOST;
        port = 0;
        try {
            instance.connect(host, port, encoding);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = "not.existed.com";
        port = 21;
        try {
            instance.connect(host, port, encoding);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        host = FtpTprops.FTP_TEST_HOST;
        port = 21;
        instance.connect(host, port, encoding);
        
    }
    
    /**
     * Test of getDataConnectionMode method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetDataConnectionMode() throws Exception {
        System.out.println("getDataConnectionMode");
        
        int expResult = FTPClient.PASSIVE_LOCAL_DATA_CONNECTION_MODE;
        int result = instance.getDataConnectionMode();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSystemName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetSystemName() throws Exception {
        System.out.println("getSystemName");
        
        String expResult = "Windows_NT"; // "UNIX Type: L8", etc
        String result = instance.getSystemName();
        assertNotNull(result);
        
    }
    
    /**
     * Test of image method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testImage() throws Exception {
        System.out.println("image");
        
        boolean expResult = true;
        boolean result = instance.image();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isConnected method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsConnected() throws Exception {
        System.out.println("isConnected");
        
        boolean expResult = true;
        boolean result = instance.isConnected();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isNegativePermanent method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsNegativePermanent() throws Exception {
        System.out.println("isNegativePermanent");
        
        int replyCode = 567;
        boolean expResult = true;
        boolean result = instance.isNegativePermanent(replyCode);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isNegativeTransient method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsNegativeTransient() throws Exception {
        System.out.println("isNegativeTransient");
        
        int replyCode = 456;
        boolean expResult = true;
        boolean result = instance.isNegativeTransient(replyCode);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPositiveCompletion method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsPositiveCompletion() throws Exception {
        System.out.println("isPositiveCompletion");
        
        int replyCode = 234;
        boolean expResult = true;
        boolean result = instance.isPositiveCompletion(replyCode);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPositiveIntermediate method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsPositiveIntermediate() throws Exception {
        System.out.println("isPositiveIntermediate");
        
        int replyCode = 345;
        boolean expResult = true;
        boolean result = instance.isPositiveIntermediate(replyCode);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isPositivePreliminary method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsPositivePreliminary() throws Exception {
        System.out.println("isPositivePreliminary");
        
        int replyCode = 123;
        boolean expResult = true;
        boolean result = instance.isPositivePreliminary(replyCode);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of listHelp method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListHelp() throws Exception {
        System.out.println("listHelp");
        
        assertNotNull(instance.listHelp());
        
    }
    
    /**
     * Test of listHelp method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListHelp_command() throws Exception {
        System.out.println("listHelp");
        String command = "DELE";
        assertNotNull(instance.listHelp(command));
        
    }
    
    /**
     * Test of login method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void ATS_testLogin() throws Exception {
        System.out.println("login");
        
        instance.logout();
        instance.disConnect();
        instance.connect(FtpTprops.FTP_TEST_HOST, 21);
        
        String user = "";
        String password = "";
        boolean expResult = false;
        boolean result = instance.login(user, password);
        assertEquals(expResult, result);
        
        user = "MyName";
        password = "";
        expResult = false;
        result = instance.login(user, password);
        assertEquals(expResult, result);
        
        user = "anonymous";
        password = "123@abc.com";
        expResult = true;
        result = instance.login(user, password);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of logout method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void ATS_testLogout() throws Exception {
        System.out.println("logout");
        
        boolean expResult = true;
        boolean result = instance.logout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of pwd method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testPwd() throws Exception {
        System.out.println("pwd");
        
        String expResult = "/";
        String result = instance.pwd();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of sendSiteCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSendSiteCommand() throws Exception {
        System.out.println("sendSiteCommand");
        
        String command = "site ?";
        boolean expResult = false;
        boolean result = instance.sendSiteCommand(command);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of useActive method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testUseActive() throws Exception {
        System.out.println("useActive");
        
        instance.useActive();
        
    }
    
    /**
     * Test of usePassive method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testUsePassive() throws Exception {
        System.out.println("usePassive");
        
        instance.usePassive();
        
    }
    
    /**
     * Test of archiveFile method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testArchiveFile() throws Exception {
        System.out.println("archiveFile");
        
        String dirNameFrom = "";
        String baseFileNameFrom = "";
        String dirNameTo = "";
        String baseFileNameTo = "";
        boolean expResult = false;
        boolean result = instance.archiveFile(dirNameFrom, baseFileNameFrom, dirNameTo, baseFileNameTo);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of completePendingCommand method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testCompletePendingCommand() throws Exception {
        System.out.println("completePendingCommand");
        boolean expResult = false;
        boolean result;
        try {
            instance.setSoTimeout(100); // to prevent from infinite blocking
            instance.listFiles(); // to enable completePendingCommand() to make sense
            result = instance.completePendingCommand(); // block on timeout
            fail("An exception is expected - timeout because no pending command");
        } catch (Exception ex) {
            ex.printStackTrace();
        } 
    }
    
    /**
     * Test of listFileNames method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFileNames_dir_isDirRegex_file_isFileRegex() throws Exception {
        System.out.println("listFileNames_dir_isDirRegex_file_isFileRegex");
        
        String dir = "";
        boolean isDirRegex = true;
        String file = "";
        boolean isFileRegex = true;
        String[] expResult = null;
        String[] result = instance.listFileNames(dir, isDirRegex, file, isFileRegex);
        assertNotNull(result);
        assertTrue(0 == result.length);
        
    }
    
    /**
     * Test of listFileNames method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testListFileNames_files() throws Exception {
        System.out.println("listFileNames_files");
        
        FTPFile[] files = null;
        String[] expResult = null;
        String[] result = instance.listFileNames(files);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getHeuristics method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetHeuristics() throws Exception {
        System.out.println("getHeuristics");
        
        assertNotNull(instance.getHeuristics());
        
    }
    
    /**
     * Test of mkdir method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testMkdir() throws Exception {
        System.out.println("mkdir");
        
        String dir = "MyTestDir";
        boolean expResult = false;
        boolean result = instance.mkdir(dir);
        assertEquals("no permission", expResult, result);
        
    }
    
    /**
     * Test of mkdirs method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testMkdirs() throws Exception {
        System.out.println("mkdirs");
        
        String dir = "/my/test/dirs";
        boolean expResult = false;
        boolean result = instance.mkdirs(dir);
        assertEquals("no permission", expResult, result);
        
    }
    
    /**
     * Test of setSocketFactory method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetSocketFactory() throws Exception {
        System.out.println("setSocketFactory");
        
        SocketFactory factory = null;
        instance.setSocketFactory(factory);
        
    }
    
    /**
     * Test of disConnect method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testDisConnect() throws Exception {
        System.out.println("disConnect");
        
        instance.disConnect();
        
    }
    
    /**
     * Test of ebcdic method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testEbcdic() throws Exception {
        System.out.println("ebcdic");
        
        boolean expResult = false;
        boolean result = instance.ebcdic();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSoLinger method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetSoLinger() throws Exception {
        System.out.println("getSoLinger");
        
        int expResult = -1;
        int result = instance.getSoLinger();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSoTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetSoTimeout() throws Exception {
        System.out.println("getSoTimeout");
        
        int expResult = 45000;
        int result = instance.getSoTimeout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTcpNoDelay method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetTcpNoDelay() throws Exception {
        System.out.println("getTcpNoDelay");
        
        boolean expResult = false;
        boolean result = instance.getTcpNoDelay();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isRemoteVerificationEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testIsRemoteVerificationEnabled() throws Exception {
        System.out.println("isRemoteVerificationEnabled");
        
        boolean expResult = true;
        boolean result = instance.isRemoteVerificationEnabled();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setRemoteVerificationEnabled method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetRemoteVerificationEnabled() throws Exception {
        System.out.println("setRemoteVerificationEnabled");
        
        boolean enable = true;
        instance.setRemoteVerificationEnabled(enable);
        assertTrue(instance.isRemoteVerificationEnabled());
        
    }
    
    /**
     * Test of setSoLinger method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetSoLinger() throws Exception {
        System.out.println("setSoLinger");
        
        boolean on = true;
        int val = 10;
        instance.setSoLinger(on, val);
        assertEquals(val, instance.getSoLinger());
        
    }
    
    /**
     * Test of setSoTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetSoTimeout() throws Exception {
        System.out.println("setSoTimeout");
        
        int timeout = 3000;
        instance.setSoTimeout(timeout);
        assertEquals(timeout, instance.getSoTimeout());
        
    }
    
    /**
     * Test of setTcpNoDelay method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetTcpNoDelay() throws Exception {
        System.out.println("setTcpNoDelay");
        
        boolean on = true;
        instance.setTcpNoDelay(on);
        assertTrue(instance.getTcpNoDelay());
        
    }
    
    /**
     * Test of rmdir method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testRmdir() throws Exception {
        System.out.println("rmdir");
        
        String dirName = "/SomeDir";
        boolean expResult = false;
        boolean result = instance.rmdir(dirName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setDataSocketTimeout method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetDataSocketTimeout() throws Exception {
        System.out.println("setDataSocketTimeout");
        
        int timeout = 2000;
        instance.setDataSocketTimeout(timeout);
        //assertEquals(timeout, instance.getDataSocketTimeout());
        
    }
    
    /**
     * Test of initialize method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        try {
            instance.initialize(null);
            fail("An exception is expected - null input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        instance.initialize(ftp);
        
    }
    
    /**
     * Test of setDirListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetDirListingStyle() throws Exception {
        System.out.println("setDirListingStyle");
        
        String dirListingStyle = "bad";
        try {
            instance.setDirListingStyle(dirListingStyle);
            fail("An exception is expected - invalid input - " + dirListingStyle);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        dirListingStyle = "NT 4.0";
        instance.setDirListingStyle(dirListingStyle);
        assertEquals(dirListingStyle, instance.getHeuristics().getDirListingStyle());
        
    }
    
    /**
     * Test of setUserDefinedHeuristicsInfo method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetUserDefinedHeuristicsInfo() throws Exception {
        System.out.println("setUserDefinedHeuristicsInfo");
        
        String userDefinedDirListingStyle = "";
        String userDefinedHeuristicsCfgFile = "";
        instance.setUserDefinedHeuristicsInfo(userDefinedDirListingStyle, userDefinedHeuristicsCfgFile);
        
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_1() throws Exception {
        System.out.println("getFirstFileName_1");
        // the tests are against ftp.microsoft.com as for 10/31/2006.

        // test 1
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "";
//            boolean isDirRegex = true;
//            String file = "";
//            boolean isFileRegex = true;
//            String expResult = "";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals("empty dir & file gives empty output", expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_1 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_2() throws Exception {
        System.out.println("getFirstFileName_2");
        // the tests are against ftp.microsoft.com as for 10/31/2006.
        
        // test 2
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "/";
//            boolean isDirRegex = true;
//            String file = "";
//            boolean isFileRegex = true;
//            String expResult = "";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals("empty file name gives empty output", expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_2 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
        
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_3() throws Exception {
        System.out.println("getFirstFileName_3");
        // the tests are against ftp.microsoft.com as for 10/31/2006.
        
        // test 3
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "/";
//            boolean isDirRegex = true;
//            String file = ".";
//            boolean isFileRegex = true;
//            String expResult = "";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals("no file under root", expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_3 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
        
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_4() throws Exception {
        System.out.println("getFirstFileName_4");
        // the tests are against ftp.microsoft.com as for 10/31/2006.
        
        // test 4
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "/bussys";
//            boolean isDirRegex = true;
//            String file = "%I"; //".";
//            boolean isFileRegex = true;
//            String expResult = "/bussys/ReadMe1.txt";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals(expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_4 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
        
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_5() throws Exception {
        System.out.println("getFirstFileName_5");
        // the tests are against ftp.microsoft.com as for 10/31/2006.
        
        // test 5
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "/%I"; // "/.";
//            boolean isDirRegex = true;
//            String file = "%I";// ".";
//            boolean isFileRegex = true;
//            String expResult = "/bussys/ReadMe1.txt";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals("/bussys/ReadMe1.txt is the first matching", expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_5 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
        
    }
    
    /**
     * Test of getFirstFileName method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetFirstFileName_6() throws Exception {
        System.out.println("getFirstFileName_6");
        // the tests are against ftp.microsoft.com as for 10/31/2006.
        
        // test 6
//        try {
//            instance.disConnect();
//            instance.connect("ftp.microsoft.com", 21); // "ftp.microsoft.com"
//            instance.login("anonymous", "test@test.com");
//
//            String dir = "/%I"; // "/.";
//            boolean isDirRegex = true;
//            String file = "r%I"; // "r";
//            boolean isFileRegex = true;
//            String expResult = "/bussys/readme.txt";
//            String result = instance.getFirstFileName(dir, isDirRegex, file, isFileRegex);
//            assertEquals("/bussys/readme.txt is the first matching", expResult, result);
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test getFirstFileName_6 may succeed or fail depending on site structure on ftp.microsoft.com as for 10/31/2006.");
//        }
        
    }
    
    /**
     * Test of getDefaultFileListParser method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetDefaultFileListParser() throws Exception {
        System.out.println("getDefaultFileListParser");
        
        assertNotNull(instance.getDefaultFileListParser());
        
    }
    
    /**
     * Test of getDelegate method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testGetDelegate() throws Exception {
        System.out.println("getDelegate");
        
        assertNotNull(instance.getDelegate());
        
    }
    
    /**
     * Test of setDefaultFileListParser method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetDefaultFileListParser() throws Exception {
        System.out.println("setDefaultFileListParser");
        
        FTPFileListParser parser = new FtpHeuristicsFileListParser();
        instance.setDefaultFileListParser(parser);
        
    }
    
    /**
     * Test of setDelegate method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetDelegate() throws Exception {
        System.out.println("setDelegate");
        
        FTPClient client = null;
        instance.setDelegate(client);
        assertEquals(client, instance.getDelegate());
        
    }
    
    /**
     * Test of setHeuristics method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetHeuristics() throws Exception {
        System.out.println("setHeuristics");
        
        FtpHeuristics heuristics = null;
        instance.setHeuristics(heuristics);
        assertEquals(heuristics, instance.getHeuristics());
        
    }
    
    /**
     * Test of isUseRegexAsMatcher method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testisUseRegexAsMatcher() throws Exception {
        System.out.println("isUseRegexAsMatcher");
        
        boolean expected = false;
        assertEquals(expected, instance.isUseRegexAsMatcher());
        
    }
    
    /**
     * Test of setUseRegexAsMatcher method, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testSetUseRegexAsMatcher() throws Exception {
        System.out.println("setUseRegexAsMatcher");
        
        boolean on = true;
        instance.setUseRegexAsMatcher(on);
        assertEquals(on, instance.isUseRegexAsMatcher());
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl.
     */
    public void testFtpFileProviderImpl() throws Exception {
        System.out.println("FtpFileProviderImpl");
        
        assertNotNull(new FtpFileProviderImpl());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
