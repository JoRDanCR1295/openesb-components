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
 * @(#)FtpFileClientImplTest.java 
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
public class FtpFileClientImplTest extends TestCase {

    FtpFileClientImpl instance;
    FtpInterface ftp;

    public FtpFileClientImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        Properties props = new Properties();
        props.put("FTP/Host Name", FtpTprops.FTP_TEST_HOST);
        props.put("FTP/Directory Listing Style", "UNIX");

        ftp = new FtpInterface();
        ftp.initialize(props);

        boolean old = false;
        if (old) {
            instance = new FtpFileClientImpl();
            instance.initialize(ftp);
        } else {
            instance = (FtpFileClientImpl) ftp.getClient();
        }

    }

    protected void tearDown() throws Exception {
        try {
            ftp.reset();
            instance.close();
        } catch (Exception ex) {
            ;
        }
        ftp = null;
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileClientImplTest.class);

        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        try {
            instance.initialize(null);
            fail("An exception is expected - null input.");
        } catch (Exception ex) {
            ;
        }

        try {
            instance.initialize(ftp);
        } catch (Exception ex) {
            fail("No exception is expected.");
        }

    }

    /**
     * Test of doRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoRawCommands() throws Exception {
        System.out.println("doRawCommands");

        String commands = "CWD /;PWD";
        try {
            instance.doRawCommands(commands);
        } catch (Exception ex) {
            fail("No exception is expected - valid commands");
        }

        commands = "BAD command";
        try {
            instance.doRawCommands(commands);
            fail("An exception is expected - invalid commands");
        } catch (Exception ex) {
            ;
        }

    }

    /**
     * Test of close method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testClose() throws Exception {
        System.out.println("close");

        instance.close();

    }

    /**
     * Test of open method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testOpen() throws Exception {
        System.out.println("open");

        instance.open();

    }

    /**
     * Test of connect method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testOpen_encoding() throws Exception {
        System.out.println("open_encoding");

        instance.open(FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
        instance.open(FtpFileConfigConstants.ENCODING_EUC_JP);
        instance.open(FtpFileConfigConstants.ENCODING_JIS);
        instance.open(FtpFileConfigConstants.ENCODING_SJIS);

    }

    /*
     * Test of getPayload method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testGetPayload() throws Exception {
        System.out.println("getPayload");

        byte[] expResult = null;
        byte[] result = instance.getPayload();
        assertEquals(expResult, result);

    }

    /**
     * Test of setPayload method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testSetPayload() throws Exception {
        System.out.println("setPayload");

        byte[] newPayload = "data".getBytes();

        instance.setPayload(newPayload);
        assertEquals(newPayload, instance.getPayload());

    }

    /**
     * Test of initialConfigValues method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testInitialConfigValues() throws Exception {
        System.out.println("initialConfigValues");
        try {
            instance.initialConfigValues(null);
            fail("An exception is expected - null input.");
        } catch (Exception ex) {
            ;
        }

        try {
            instance.initialConfigValues(new Properties());
        } catch (Exception ex) {
            fail("No exception is expected - even empty Properties :-)");
        }
    }

    /**
     * Test of isOpen method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testIsOpen() throws Exception {
        System.out.println("isOpen");

        boolean expResult = true;
        boolean result = instance.isOpen();
        assertEquals(expResult, result);

    }

    /**
     * Test of restoreConfigValues method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testRestoreConfigValues() throws Exception {
        System.out.println("restoreConfigValues");

        instance.restoreConfigValues();

    }

    /**
     * Test of connect method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testConnect() throws Exception {
        System.out.println("connect");

        instance.connect();

    }

    /**
     * Test of connect method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testConnect_encoding() throws Exception {
        System.out.println("connect_encoding");

        instance.connect(FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
        instance.connect(FtpFileConfigConstants.ENCODING_EUC_JP);
        instance.connect(FtpFileConfigConstants.ENCODING_JIS);
        instance.connect(FtpFileConfigConstants.ENCODING_SJIS);

    }

    /**
     * Test of disconnect method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDisconnect() throws Exception {
        System.out.println("disconnect");

        instance.disconnect();

    }

    /**
     * Test of isConnected method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testIsConnected() throws Exception {
        System.out.println("isConnected");

        boolean expResult = true;
        boolean result = instance.isConnected();
        assertEquals(expResult, result);

    }

    /**
     * Test of setInputStreamAdapter method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testSetInputStreamAdapter() throws Exception {
        System.out.println("setInputStreamAdapter");

        instance.setPayload("Not-Null".getBytes());

        instance.setInputStreamAdapter(null);

        assertNull("setInputStreamAdapter() will reset payload to null", instance.getPayload());

    }

    /**
     * Test of setOutputStreamAdapter method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testSetOutputStreamAdapter() throws Exception {
        System.out.println("setOutputStreamAdapter");

        instance.setPayload("Not-Null".getBytes());

        instance.setOutputStreamAdapter(null);

        assertNull("setOutputStreamAdapter() will reset payload to null", instance.getPayload());

    }

    /**
     * Test of getResolvedNamesForGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testGetResolvedNamesForGet() throws Exception {
        System.out.println("getResolvedNamesForGet");

        assertNotNull(instance.getResolvedNamesForGet());

    }

    /**
     * Test of getResolvedNamesForPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testGetResolvedNamesForPut() throws Exception {
        System.out.println("getResolvedNamesForPut");

        assertNotNull(instance.getResolvedNamesForPut());

    }

    /**
     * Test of doPostTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoPostTransferGet() throws Exception {
        System.out.println("doPostTransferGet");
        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.doPostTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - \"None\" is the default command");
        }

    }

    /**
     * Test of doPreTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoPreTransferGet() throws Exception {
        System.out.println("doPreTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.doPreTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - \"None\" is the default command");
        }

    }

    /**
     * Test of doTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoTransferGet() throws Exception {
        System.out.println("doTransferGet");
        // originally the test assume the target location is blank
        // but that is not true - certain configuration change on the target server
        // can result in that :
        // after 
        // TransferNamesAndCommands tncg = instance.getResolvedNamesForGet()
        // tncg.getTargetDirectoryName() can be a non-empty value
        // and/or
        // tncg.getTargetFileName() can be a non-empty value
        // so the test should be changed to accommodate these;
        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            if ((tncg.getTargetDirectoryName() != null && tncg.getTargetDirectoryName().trim().length() > 0) || (tncg.getTargetFileName() != null && tncg.getTargetFileName().trim().length() > 0)) {
                // do not further try get
                return;
            }
            instance.doTransferGet(tncg);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail("No exception is expected although incomplete configuration (no target location) - it just does nothing and return then method get() will check and throw FileNotFoundException");
        }

    }

    /**
     * Test of doPostTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoPostTransferPut() throws Exception {
        System.out.println("doPostTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.doPostTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - \"None\" is the default command");
        }

    }

    /**
     * Test of doPreTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoPreTransferPut() throws Exception {
        System.out.println("doPreTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.doPreTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - \"None\" is the default command");
        }

    }

    /**
     * Test of doTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testDoTransferPut() throws Exception {
        System.out.println("doTransferPut");

        try {
            instance.setPayload("any data".getBytes());
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.doTransferPut(tncp);
            fail("An exception is expected - incomplete configuration - no target location and no writing permission");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Test of put method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testPut() throws Exception {
        System.out.println("put");

        try {
            instance.setPayload("any data".getBytes());
            instance.put();
            fail("An exception is expected - incomplete configuration - no target location and no writing permission");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Test of undoPreTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoPreTransferGet() throws Exception {
        System.out.println("undoPreTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.undoPreTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoTransferGet() throws Exception {
        System.out.println("undoTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.undoTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoPreTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoPreTransferPut() throws Exception {
        System.out.println("undoPreTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.undoPreTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoTransferPut() throws Exception {
        System.out.println("undoTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.undoTransferPut(tncp);
        } catch (Exception ex) {
            //fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoPostTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoPostTransferGet() throws Exception {
        System.out.println("undoPostTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.undoPostTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoPostTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoPostTransferPut() throws Exception {
        System.out.println("undoPostTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.undoPostTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupPostTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupPostTransferGet() throws Exception {
        System.out.println("cleanupPostTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.cleanupPostTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupPostTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupPostTransferPut() throws Exception {
        System.out.println("cleanupPostTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.cleanupPostTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupPreTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupPreTransferGet() throws Exception {
        System.out.println("cleanupPreTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.cleanupPreTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupPreTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupPreTransferPut() throws Exception {
        System.out.println("cleanupPreTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.cleanupPreTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupRawCommands() throws Exception {
        System.out.println("cleanupRawCommands");

        try {
            TransferNamesAndCommands tncr = null;
            instance.cleanupRawCommands(tncr);
        } catch (Exception ex) {
            fail("No exception is expected - no action");
        }

    }

    /**
     * Test of cleanupTransferGet method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupTransferGet() throws Exception {
        System.out.println("cleanupTransferGet");

        try {
            TransferNamesAndCommands tncg = instance.getResolvedNamesForGet();
            instance.cleanupTransferGet(tncg);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of cleanupTransferPut method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testCleanupTransferPut() throws Exception {
        System.out.println("cleanupTransferPut");

        try {
            TransferNamesAndCommands tncp = instance.getResolvedNamesForPut();
            instance.cleanupTransferPut(tncp);
        } catch (Exception ex) {
            fail("No exception is expected - no action up on config");
        }

    }

    /**
     * Test of undoRawCommands method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testUndoRawCommands() throws Exception {
        System.out.println("undoRawCommands");

        try {
            TransferNamesAndCommands tncr = null;
            instance.undoRawCommands(tncr);
        } catch (Exception ex) {
            fail("No exception is expected - no action");
        }

    }

    /**
     * Test of get method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testGet() throws Exception {
        System.out.println("get");

        try {
            TransferNamesAndCommands rn4get = instance.getResolvedNamesForGet();
            if ((rn4get.getTargetDirectoryName() != null && rn4get.getTargetDirectoryName().trim().length() > 0) || (rn4get.getTargetFileName() != null && rn4get.getTargetFileName().trim().length() > 0)) {
                // if the server is configured such that 
                // the target dir and/or file is not empty
                // the following assert might not be holding
                // so bail out here - instead of failing the test
                return;
            }
            instance.get();
            fail("An exception is expected - incomplete configuration - no target location");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Test of getIfExists method, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testGetIfExists() throws Exception {
        System.out.println("getIfExists");

        try {
            instance.getIfExists();
        } catch (Exception ex) {
            ex.printStackTrace();
            fail("No exception is expected - get only when exists");
        }

    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileClientImpl.
     */
    public void testFtpFileClientImpl() throws Exception {
        System.out.println("FtpFileClientImpl");

        assertNotNull(new FtpFileClientImpl());
        assertNotNull(new FtpFileClientImpl(ftp));
        try {
            new FtpFileClientImpl(null);
            fail("An exception is expected - invalid input - null");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        try {
            new FtpFileClientImpl(new FtpInterface());
        } catch (Exception ex) {
            fail("No exception is expected - initialization error");
        }

    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
}
