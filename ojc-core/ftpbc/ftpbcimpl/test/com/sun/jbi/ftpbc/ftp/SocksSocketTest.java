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
 * @(#)SocksSocketTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.InputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketImplFactory;
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
public class SocksSocketTest extends TestCase {
    SocksSocket instance;
    
    public SocksSocketTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // null Socks -- not depend on live Socks server
        instance = new SocksSocket(null, "www.sun.com", 80);
    }
    
    protected void tearDown() throws Exception {
        try {
            instance.close();
        } catch (Exception e) {
            ;
        }
        
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SocksSocketTest.class);
        
        return suite;
    }
    
    /**
     * Test of setSocketImplFactory method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetSocketImplFactory() throws Exception {
        System.out.println("setSocketImplFactory");
        
        SocketImplFactory factory = null;
        
        SocksSocket.setSocketImplFactory(factory);
        
    }
    
    /**
     * Test of close method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        
        instance.close();
        
    }
    
    /**
     * Test of getInetAddress method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetInetAddress() throws Exception {
        System.out.println("getInetAddress");
        
        InetAddress result = instance.getInetAddress();
        assertNotNull(result);
        
    }
    
    /**
     * Test of getInputStream method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetInputStream() throws Exception {
        System.out.println("getInputStream");
        
        InputStream result = instance.getInputStream();
        assertNotNull(result);
        
    }
    
    /**
     * Test of getLocalAddress method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetLocalAddress() throws Exception {
        System.out.println("getLocalAddress");
        
        InetAddress result = instance.getLocalAddress();
        assertNotNull(result);
        
    }
    
    /**
     * Test of getLocalPort method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetLocalPort() throws Exception {
        System.out.println("getLocalPort");
        
        int result = instance.getLocalPort();
        assertTrue(result > 0);
        
    }
    
    /**
     * Test of getOutputStream method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetOutputStream() throws Exception {
        System.out.println("getOutputStream");
        
        assertNotNull(instance.getOutputStream());
        
    }
    
    /**
     * Test of getPort method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetPort() throws Exception {
        System.out.println("getPort");
        
        int expResult = 80;
        int result = instance.getPort();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getReceiveBufferSize method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetReceiveBufferSize() throws Exception {
        System.out.println("getReceiveBufferSize");
        
        int expResult = 8192;
        int result = instance.getReceiveBufferSize();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSendBufferSize method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetSendBufferSize() throws Exception {
        System.out.println("getSendBufferSize");
        
        int expResult = 8192;
        int result = instance.getSendBufferSize();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSoLinger method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetSoLinger() throws Exception {
        System.out.println("getSoLinger");
        
        int expResult = -1;
        int result = instance.getSoLinger();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSoTimeout method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetSoTimeout() throws Exception {
        System.out.println("getSoTimeout");
        
        int expResult = 0;
        int result = instance.getSoTimeout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTcpNoDelay method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetTcpNoDelay() throws Exception {
        System.out.println("getTcpNoDelay");
        
        boolean expResult = false;
        boolean result = instance.getTcpNoDelay();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setReceiveBufferSize method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetReceiveBufferSize() throws Exception {
        System.out.println("setReceiveBufferSize");
        
        int size = 1024;
        instance.setReceiveBufferSize(size);
        assertEquals(size, instance.getReceiveBufferSize());
        
        try {
            size = 0;
            instance.setReceiveBufferSize(size);
            fail("An exception is expected - 0 size is invalid");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            size = -1;
            instance.setReceiveBufferSize(size);
            fail("An exception is expected - negative size is invalid");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of setSendBufferSize method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetSendBufferSize() throws Exception {
        System.out.println("setSendBufferSize");
        
        int size = 1024;
        instance.setSendBufferSize(size);
        assertEquals(size, instance.getSendBufferSize());
        
        try {
            size = 0;
            instance.setSendBufferSize(size);
            fail("An exception is expected - 0 size is invalid");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            size = -1;
            instance.setSendBufferSize(size);
            fail("An exception is expected - negative size is invalid");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of setSoLinger method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetSoLinger() throws Exception {
        System.out.println("setSoLinger");
        
        boolean on = true;
        int linger = 0;
        instance.setSoLinger(on, linger);
        assertEquals(linger, instance.getSoLinger());
        
        linger = 10;
        instance.setSoLinger(on, linger);
        assertEquals(linger, instance.getSoLinger());
        
        try {
            linger = -1;
            instance.setSoLinger(on, linger);
            fail("An exception is expected - invalid value");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        // false
        on = false;
        linger = 0;
        instance.setSoLinger(on, linger);
        assertEquals("Always return -1 when it is disabled", -1, instance.getSoLinger());
        
        linger = 10;
        instance.setSoLinger(on, linger);
        assertEquals("Always return -1 when it is disabled", -1, instance.getSoLinger());
        
        linger = -10;
        instance.setSoLinger(on, linger);
        assertEquals("Always return -1 when it is disabled", -1, instance.getSoLinger());
        
    }
    
    /**
     * Test of setSoTimeout method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetSoTimeout() throws Exception {
        System.out.println("setSoTimeout");
        
        int timeout = 0;
        instance.setSoTimeout(timeout);
        assertEquals(timeout, instance.getSoTimeout());
        
        timeout = 100;
        instance.setSoTimeout(timeout);
        assertEquals(timeout, instance.getSoTimeout());
        
    }
    
    /**
     * Test of setTcpNoDelay method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetTcpNoDelay() throws Exception {
        System.out.println("setTcpNoDelay");
        
        boolean on = true;
        instance.setTcpNoDelay(on);
        assertEquals(on, instance.getTcpNoDelay());
        
    }
    
    /**
     * Test of toString method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testToString() throws Exception {
        System.out.println("toString");
        
        String expResult = "";
        String result = instance.toString();
        assertNotNull(result);
        assertFalse(expResult.equals(result));
        
    }
    
    /**
     * Test of getKeepAlive method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetKeepAlive() throws Exception {
        System.out.println("getKeepAlive");
        
        boolean expResult = false;
        boolean result = instance.getKeepAlive();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setKeepAlive method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSetKeepAlive() throws Exception {
        System.out.println("setKeepAlive");
        
        boolean on = true;
        instance.setKeepAlive(on);
        assertEquals(on, instance.getKeepAlive());
        
    }
    
    /**
     * Test of shutdownInput method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testShutdownInput() throws Exception {
        System.out.println("shutdownInput");
        
        instance.shutdownInput();
        
    }
    
    /**
     * Test of shutdownOutput method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testShutdownOutput() throws Exception {
        System.out.println("shutdownOutput");
        
        instance.shutdownOutput();
        
    }
    
    /**
     * Test of main method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testMain() throws Exception {
        System.out.println("main");
        
        String[] args = null;
        
        SocksSocket.main(args);
        
    }
    
    /**
     * Test of getSocks method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetSocks() throws Exception {
        System.out.println("getSocks");
        
        Socks expResult = null;
        Socks result = instance.getSocks();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksList method, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testGetSocksList() throws Exception {
        System.out.println("getSocksList");
        
        SocksChain expResult = null;
        SocksChain result = instance.getSocksList();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.SocksSocket.
     */
    public void testSocksSocket() throws Exception {
        System.out.println("SocksSocket");

        SocksChain chain = null;
        Socks socks = null;
        
        SocksSocket ss = new SocksSocket(InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21);
        assertNotNull(ss);
        ss.close();
        
        ss = new SocksSocket(FtpTprops.FTP_TEST_HOST, 21);
        assertNotNull(ss);
        ss.close();
        
        ss = new SocksSocket(InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, chain);
        assertNotNull(ss);
        ss.close();
        
        ss = new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21);
        assertNotNull(ss);
        ss.close();
        
        ss = new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21);
        assertNotNull(ss);
        ss.close();
        
        ss = new SocksSocket(FtpTprops.FTP_TEST_HOST, 21, chain);
        assertNotNull(ss);
        ss.close();
        
        try {
            int localPort = 4567;
            
            // the local port 4567, 4568, etc may be in use in test env, 
            // so following constructors may or may not succeed.
            ss = new SocksSocket(InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, InetAddress.getLocalHost(), localPort);
            assertNotNull(ss);
            ss.close();

            ss = new SocksSocket(FtpTprops.FTP_TEST_HOST, 21, InetAddress.getLocalHost(), localPort);
            assertNotNull(ss);
            ss.close();

            ss = new SocksSocket(InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, InetAddress.getLocalHost(), localPort, chain);
            assertNotNull(ss);
            ss.close();

            ss = new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, InetAddress.getLocalHost(), localPort);
            assertNotNull(ss);
            ss.close();

            ss = new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21, InetAddress.getLocalHost(), localPort);
            assertNotNull(ss);
            ss.close();

            ss = new SocksSocket(FtpTprops.FTP_TEST_HOST, 21, InetAddress.getLocalHost(), localPort, chain);
            assertNotNull(ss);
            ss.close();

        } catch (Exception e) {
            // the local port 4567, 4568, etc may be in use in test env, 
            // so those tests may or may not succeed.
            e.printStackTrace();
            System.out.println("SocksSocket: Local Port is in use under test environment - it is ok for test.");
        }
    }
    
    public void testMore_WithWrongSocksServer() throws Exception {
        System.out.println("testMore_WithLiveSocksServer");

        Socks socks = new Socks("WrongSocksHost", 1080, "username", "password");
        try {
            new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21);
            fail("An exception is expected - wrong socks server");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21);
            fail("An exception is expected - wrong socks server");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        int localPort = 4570;
        try {
            new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, InetAddress.getLocalHost(), localPort);
            fail("An exception is expected - wrong socks server");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21, InetAddress.getLocalHost(), localPort);
            fail("An exception is expected - wrong socks server");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public void testMore_WithLiveSocksServer() throws Exception {
        System.out.println("testMore_WithLiveSocksServer");

        String socksHost = "eurus.stc.com";
        int socksPort = 1080;
        
        // check 
        try {
            new Socket(socksHost, socksPort).close();
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("testMore_WithLiveSocksServer: no live tests because no Socks is running on eurus.stc.com:1080");
            return;
        }
        
        System.out.println("testMore_WithLiveSocksServer: doing live tests because Socks is running on eurus.stc.com:1080");

        Socks socks = new Socks(socksHost, socksPort, "username", "password");
        
        SocksSocket ss = null;
        byte[] bytesIn = new byte[1024];
        int len = 0;
        
        // test 1
        ss = new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21);
        assertNotNull(ss);
        ss.setSoTimeout(1000);
        len = ss.getInputStream().read(bytesIn);
        assertTrue(len > 0);
        System.out.println("Test 1: data gone through Socks is: " + new String(bytesIn).trim());
        ss.close();
        
        // test 2
        ss = new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21);
        assertNotNull(ss);
        ss.setSoTimeout(1000);
        len = ss.getInputStream().read(bytesIn);
        assertTrue(len > 0);
        System.out.println("Test 2: data gone through Socks is: " + new String(bytesIn).trim());
        ss.close();
        
        // test 3
        int localPort = 4570;
        try {
            // for this constructor with local port, it is possible to fail if the local port is in use
            ss = new SocksSocket(socks, InetAddress.getByName(FtpTprops.FTP_TEST_HOST), 21, InetAddress.getLocalHost(), localPort);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Test 3: not performed because local port is in use");
            return;
        }
        
        assertNotNull(ss);
        ss.setSoTimeout(1000);
        len = ss.getInputStream().read(bytesIn);
        assertTrue(len > 0);
        System.out.println("Test 3: data gone through Socks is: " + new String(bytesIn).trim());
        ss.close();
        
        // test 4
        try {
            // for this constructor with local port, it is possible to fail if the local port is in use
            ss = new SocksSocket(socks, FtpTprops.FTP_TEST_HOST, 21, InetAddress.getLocalHost(), localPort);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Test 4: not performed because local port is in use");
            return;
        }
        
        assertNotNull(ss);
        ss.setSoTimeout(1000);
        len = ss.getInputStream().read(bytesIn);
        assertTrue(len > 0);
        System.out.println("Test 4: data gone through Socks is: " + new String(bytesIn).trim());
        ss.close();
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
