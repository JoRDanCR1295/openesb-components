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
 * @(#)ScSCryptTest.java 
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
public class ScSCryptTest extends TestCase {
    NonEmptyScEncrypt.ScSCrypt instance;
    
    public ScSCryptTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new NonEmptyScEncrypt.ScSCrypt("username");
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(ScSCryptTest.class);
        
        return suite;
    }
    
    /**
     * Test of getChain method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testGetChain() throws Exception {
        System.out.println("getChain");
        
        char expResult = (char) 0x72; //NonEmptyScEncrypt.SC_M_CRYPT_CHAIN_INIT;
        char result = instance.getChain();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getEndSpace method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testGetEndSpace() throws Exception {
        System.out.println("getEndSpace");
        
        int expResult = 16;
        int result = instance.getEndSpace();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getCurrKey method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testGetCurrKey() throws Exception {
        System.out.println("getCurrKey");
        
        int expResult = 0;
        int result = instance.getCurrKey();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getKeySpace method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testGetKeySpace() throws Exception {
        System.out.println("getKeySpace");
        
        assertNotNull(instance.getKeySpace());
        assertTrue(instance.getKeySpace().length > 0);
        
    }
    
    /**
     * Test of extend method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testExtend() throws Exception {
        System.out.println("extend");
        
        instance.extend();
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.ScSCrypt.
     */
    public void testNonEmptyScEncryptScSCrypt() throws Exception {
        System.out.println("NonEmptyScEncrypt.ScSCrypt");
        
        assertNotNull(new NonEmptyScEncrypt.ScSCrypt("a key"));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
