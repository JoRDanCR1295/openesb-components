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
 * @(#)SocksChainTest.java 
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
public class SocksChainTest extends TestCase {
    SocksChain instance;
    
    public SocksChainTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        Socks[] socksList = new Socks[2];
        socksList[0] = new Socks("host1", 1080, "user1", "password1", 5);
        socksList[1] = new Socks("host2", 1080, "user2", "password2", 5);
        instance = new SocksChain(socksList);
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SocksChainTest.class);
        
        return suite;
    }
    
    /**
     * Test of add method, of class com.sun.jbi.ftpbc.ftp.SocksChain.
     */
    public void testAdd() throws Exception {
        System.out.println("add");
        Socks socks = new Socks("host2", 1080, "user2", "password2");
        try {
            instance.add(socks);
            fail("An exception is expected - host2/1080 existed in list already");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        socks = new Socks("host3", 1080, "user3", "password3");
        boolean expResult = true;
        boolean result = instance.add(socks);
        assertEquals(expResult, result);
        assertEquals(3, instance.chainSize());
        
    }
    
    /**
     * Test of chainSize method, of class com.sun.jbi.ftpbc.ftp.SocksChain.
     */
    public void testChainSize() throws Exception {
        System.out.println("chainSize");
        
        int expResult = 2;
        int result = instance.chainSize();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of get method, of class com.sun.jbi.ftpbc.ftp.SocksChain.
     */
    public void testGet() throws Exception {
        System.out.println("get");
        
        assertNotNull(instance.get(0));
        assertNotNull(instance.get(1));
        try {
            assertNotNull(instance.get(2));
            fail("An exception is expected - out of index - only has 2 socks in the list");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.SocksChain.
     */
    public void testSocksChain() throws Exception {
        System.out.println("SocksChain");
        
        Socks[] socksList = new Socks[2];
        socksList[0] = new Socks(null, null, null);
        socksList[1] = new Socks(null, null, null);
        
        assertNotNull(new SocksChain());
        assertNotNull(new SocksChain(socksList));
        assertNotNull("initial list can be null", new SocksChain(null));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
