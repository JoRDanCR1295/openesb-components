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
 * @(#)SocksSocketFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
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
public class SocksSocketFactoryTest extends TestCase {
    SocksSocketFactory instance;
    
    public SocksSocketFactoryTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new SocksSocketFactory();
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SocksSocketFactoryTest.class);
        
        return suite;
    }
    
    /**
     * Test of createSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateSocket_host_port() throws Exception {
        System.out.println("createSocket_host_port");
        
        String host = FtpTprops.FTP_TEST_HOST;
        int port = 21;
        Socket result = instance.createSocket(host, port);
        assertNotNull(result);
        result.close();
        
        host = "localhost";
        port = 12345;
        try {
            result = instance.createSocket(host, port);
            fail("An exception is expected - port 12345 is not opened on localhost");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of createSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateSocket_address_port() throws Exception {
        System.out.println("createSocket_address_port");
        
        InetAddress address = InetAddress.getByName(FtpTprops.FTP_TEST_HOST);
        int port = 21;
        Socket result = instance.createSocket(address, port);
        assertNotNull(result);
        result.close();
        
        address = InetAddress.getByName("localhost");
        port = 12345;
        try {
            result = instance.createSocket(address, port);
            fail("An exception is expected - port 12345 is not opened on localhost");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of createSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateSocket_host_port_localAddr_localPort() throws Exception {
        System.out.println("createSocket_host_port_localAddr_localPort");
        
        String host = FtpTprops.FTP_TEST_HOST;
        int port = 21;
        InetAddress localAddr = InetAddress.getLocalHost();
        int localPort = 3456;
        
        try {
            Socket result = instance.createSocket(host, port, localAddr, localPort);
            assertNotNull(result);
            result.close();
            
            host = "localhost";
            port = 12345;
            try {
                result = instance.createSocket(host, port, localAddr, localPort);
                fail("An exception is expected - port 12345 is not opened on localhost");
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            System.out.println("createSocket_host_port_localAddr_localPort: Local Port is in use under test environment - it is ok for test.");
        }
        
    }
    
    /**
     * Test of createSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateSocket_address_port_localAddr_localPort() throws Exception {
        System.out.println("createSocket_address_port_localAddr_localPort");
        
        InetAddress address = InetAddress.getByName(FtpTprops.FTP_TEST_HOST);
        int port = 21;
        InetAddress localAddr = InetAddress.getLocalHost();
        int localPort = 3457;
        
        try {
            Socket result = instance.createSocket(address, port, localAddr, localPort);
            assertNotNull(result);
            result.close();
            
            address = InetAddress.getByName("localhost");
            port = 12345;
            try {
                result = instance.createSocket(address, port, localAddr, localPort);
                fail("An exception is expected - port 12345 is not opened on localhost");
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            System.out.println("createSocket_address_port_localAddr_localPort: Local Port is in use under test environment - it is ok for test.");
        }
        
    }
    
    /**
     * Test of createServerSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateServerSocket_port() throws Exception {
        System.out.println("createServerSocket_port");
        
        int localPort = 0;
        ServerSocket result = instance.createServerSocket(localPort);
        assertNotNull(result);
        result.close();
        
    }
    
    /**
     * Test of createServerSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateServerSocket_port_backlog() throws Exception {
        System.out.println("createServerSocket_port_backlog");
        
        int localPort = 0;
        int backlog = 50;
        ServerSocket result = instance.createServerSocket(localPort, backlog);
        assertNotNull(result);
        result.close();
        
    }
    
    /**
     * Test of createServerSocket method, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testCreateServerSocket_port_backlog_bindAddr() throws Exception {
        System.out.println("createServerSocket_port_backlog_bindAddr");
        
        int localPort = 0;
        int backlog = 50;
        InetAddress bindAddr = InetAddress.getLocalHost();
        ServerSocket result = instance.createServerSocket(localPort, backlog, bindAddr);
        assertNotNull(result);
        result.close();
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.SocksSocketFactory.
     */
    public void testSocksSocketFactory() throws Exception {
        System.out.println("SocksSocketFactory");
        
        assertNotNull(new SocksSocketFactory(new Socks(null, null, null)));
        assertNotNull(new SocksSocketFactory(new SocksChain()));
        assertNotNull(new SocksSocketFactory());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
