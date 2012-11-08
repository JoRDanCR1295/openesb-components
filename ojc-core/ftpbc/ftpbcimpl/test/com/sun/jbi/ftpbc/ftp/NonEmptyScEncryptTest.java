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
 * @(#)NonEmptyScEncryptTest.java 
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
public class NonEmptyScEncryptTest extends TestCase {
    
    public NonEmptyScEncryptTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
    }
    
    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(NonEmptyScEncryptTest.class);
        
        return suite;
    }
    
    /**
     * Test of main method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.
     */
    public void testMain() throws Exception {
        System.out.println("main");
        
        String[] args = null;
        try {
            NonEmptyScEncrypt.main(args);
            fail("An exception is expected - invalid usage");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of decrypt method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.
     */
    public void testDecrypt() throws Exception {
        System.out.println("decrypt");
        
        String key = "username";
        String data = "plain-password";
        
        String expResult = "";
        String result = NonEmptyScEncrypt.encrypt(key, data);
        System.out.println("encrypt: " + result);
        
        expResult = NonEmptyScEncrypt.decrypt(key, result);
        System.out.println("decrypt: " + expResult);
        assertEquals(expResult, data);
        
    }
    
    /**
     * Test of encrypt method, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.
     */
    public void testEncrypt() throws Exception {
        System.out.println("encrypt");
        
        String key = "username";
        String data = "plain-password";
        
        String expResult = "";
        String result = NonEmptyScEncrypt.encrypt(key, data);
        System.out.println("encrypt: " + result);
        assertNotNull(result);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.NonEmptyScEncrypt.
     */
    public void testNonEmptyScEncrypt() throws Exception {
        System.out.println("NonEmptyScEncrypt");
        
        assertNotNull(new NonEmptyScEncrypt());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
