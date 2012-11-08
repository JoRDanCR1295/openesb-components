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
 * @(#)SocksTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

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
public class SocksTest extends TestCase {
    Socks instance;
    
    public SocksTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new Socks("myhost", 1080, "user", "password", Socks.VERSION_UNKNOWN);
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SocksTest.class);
        
        return suite;
    }
    
    /**
     * Test of getSocksHost method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testGetSocksHost() {
        System.out.println("getSocksHost");
        
        String expResult = "myhost";
        String result = instance.getSocksHost();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksPassword method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testGetSocksPassword() {
        System.out.println("getSocksPassword");
        
        String expResult = "password";
        String result = instance.getSocksPassword();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksPort method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testGetSocksPort() {
        System.out.println("getSocksPort");
        
        int expResult = 1080;
        int result = instance.getSocksPort();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksUser method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testGetSocksUser() {
        System.out.println("getSocksUser");
        
        String expResult = "user";
        String result = instance.getSocksUser();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSocksVersion method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testGetSocksVersion() {
        System.out.println("getSocksVersion");
        
        int expResult = Socks.VERSION_UNKNOWN;
        int result = instance.getSocksVersion();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSocksVersion method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testSetSocksVersion() throws Exception {
        System.out.println("setSocksVersion");
        
        int newSocksVersion = Socks.VERSION_5;
        instance.setSocksVersion(newSocksVersion);
        assertEquals(newSocksVersion, instance.getSocksVersion());
        
    }
    
    /**
     * Test of equals method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testEquals() {
        System.out.println("equals");
        
        Object obj = null;
        boolean expResult = false;
        boolean result = instance.equals(obj);
        assertEquals(expResult, result);
        
        obj = "a string object";
        expResult = false;
        result = instance.equals(obj);
        assertEquals(expResult, result);
        
        obj = new Socks("myhost", 1080, "user", "password", -1);
        expResult = true;
        result = instance.equals(obj);
        assertEquals("Only host/port are checked for equals", expResult, result);
        
        
        obj = new Socks("myhost", 1080, "user2", "password2", -1);
        expResult = true;
        result = instance.equals(obj);
        assertEquals("Only host/port are checked for equals", expResult, result);
        
        
        obj = new Socks("myhost", "user", "password");
        expResult = true;
        result = instance.equals(obj);
        assertEquals("Only host/port are checked for equals", expResult, result);
        
        
        obj = new Socks("myhost", 1080, "user-any", "password", 5);
        expResult = true;
        result = instance.equals(obj);
        assertEquals("Only host/port are checked for equals", expResult, result);
        
    }
    
    /**
     * Test of resolveHostName method, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testResolveHostName() {
        System.out.println("resolveHostName");
        
        String hostName = "any";
        
        String expResult = "any";
        String result = Socks.resolveHostName(hostName);
        assertEquals("static method is called", expResult, result);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.Socks.
     */
    public void testSocks() throws Exception {
        System.out.println("Socks");
        
        assertNotNull(new Socks("host1", "user1", "pass1"));
        assertNotNull(new Socks("host1", "user1", "pass1", 5));
        assertNotNull(new Socks("host1", 1080, "user1", "pass1"));
        assertNotNull(new Socks("host1", 1080, "user1", "pass1", 5));
        
        // empty string
        assertNotNull(new Socks("", "", ""));
        assertNotNull(new Socks("", "", "", 5));
        assertNotNull(new Socks("", 1080, "", ""));
        assertNotNull(new Socks("", 1080, "", "", 5));
        
        // null
        assertNotNull(new Socks(null, null, null));
        assertNotNull(new Socks(null, null, null, 5));
        assertNotNull(new Socks(null, 1080, null, null));
        assertNotNull(new Socks(null, 1080, null, null, 5));
        
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
