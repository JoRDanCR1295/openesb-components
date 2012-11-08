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
 * @(#)SocksServerSocketTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

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
public class SocksServerSocketTest extends TestCase {
    SocksServerSocket instance;
    
    public SocksServerSocketTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // null Socks -- not depend on live Socks server
        instance = new SocksServerSocket(null, 0);
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
        TestSuite suite = new TestSuite(SocksServerSocketTest.class);
        
        return suite;
    }
    
    /**
     * Test of setSocketFactory method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testSetSocketFactory() throws Exception {
        System.out.println("setSocketFactory");
        
        SocketImplFactory factory = null;
        SocksServerSocket.setSocketFactory(factory);

    }
    
    /**
     * Test of accept method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testAccept() throws Exception {
        System.out.println("accept");
        try {
            instance.setSoTimeout(100); // to avoid infinite blocking
            Socket result = instance.accept();
            fail("An exception is expected - timeout to accept");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of close method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        
        instance.close();
        
    }
    
    /**
     * Test of getInetAddress method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testGetInetAddress() throws Exception {
        System.out.println("getInetAddress");
        
        assertNotNull(instance.getInetAddress());
        
    }
    
    /**
     * Test of getLocalPort method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testGetLocalPort() throws Exception {
        System.out.println("getLocalPort");
        
        int result = instance.getLocalPort();
        assertTrue(result > 0);
        
    }
    
    /**
     * Test of getSoTimeout method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testGetSoTimeout() throws Exception {
        System.out.println("getSoTimeout");
        
        int expResult = 0;
        int result = instance.getSoTimeout();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoTimeout method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testSetSoTimeout() throws Exception {
        System.out.println("setSoTimeout");
        
        int timeout = 0;
        instance.setSoTimeout(timeout);
        assertEquals(timeout, instance.getSoTimeout());
        
        timeout = 50;
        instance.setSoTimeout(timeout);
        assertEquals(timeout, instance.getSoTimeout());
        
    }
    
    /**
     * Test of toString method, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testToString() throws Exception {
        System.out.println("toString");
        
        assertNotNull(instance.toString());
        assertFalse("".equals(instance.toString()));
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.SocksServerSocket.
     */
    public void testSocksServerSocket() throws Exception {
        System.out.println("SocksServerSocket");
        
        int localPort = 0;
        
        SocksServerSocket sss = new SocksServerSocket(localPort);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(new Socks(null, null, null), localPort);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(localPort, 50);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(new Socks(null, null, null), localPort, 50);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(localPort, 50, InetAddress.getLocalHost());
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(new Socks(null, null, null), localPort, 50, InetAddress.getLocalHost());
        assertNotNull(sss);
        sss.close();
        
        // null
        sss = new SocksServerSocket(null, localPort);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(null, localPort, 50);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(localPort, 50, null);
        assertNotNull(sss);
        sss.close();
        
        sss = new SocksServerSocket(null, localPort, 50, null);
        assertNotNull(sss);
        sss.close();
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
