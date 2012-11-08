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
 * @(#)FtpFileStateTest.java 
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
public class FtpFileStateTest extends TestCase {
    FtpFileState instance;
    
    public FtpFileStateTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpFileState();
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileStateTest.class);
        
        return suite;
    }

    /**
     * Test of getSequenceNo method, of class com.sun.jbi.ftpbc.ftp.FtpFileState.
     */
    public void testGetSequenceNo() throws Exception {
        System.out.println("getSequenceNo");
        
        assertEquals(10, instance.getSequenceNo().length);
        
    }

    /**
     * Test of getSequenceNo method, of class com.sun.jbi.ftpbc.ftp.FtpFileState.
     */
    public void testGetSequenceNo_int() throws Exception {
        System.out.println("getSequenceNo_int");
        
        long expResult = -1;
        for (int i = 0; i < instance.getSequenceNo().length; i++) {
            long result = instance.getSequenceNo(i);
            assertEquals(expResult, result);
        }
        
    }

    /**
     * Test of setSequenceNo method, of class com.sun.jbi.ftpbc.ftp.FtpFileState.
     */
    public void testSetSequenceNo() throws Exception {
        System.out.println("setSequenceNo");
        
        long [] newSequenceNo = new long[10];
        instance.setSequenceNo(newSequenceNo);
        assertEquals(newSequenceNo, instance.getSequenceNo());
        
    }

    /**
     * Test of setSequenceNo method, of class com.sun.jbi.ftpbc.ftp.FtpFileState.
     */
    public void testSetSequenceNo_int() throws Exception {
        System.out.println("setSequenceNo_int");
        
        long newSequenceNo = 123;
        for (int i = 0; i < instance.getSequenceNo().length; i++) {
            instance.setSequenceNo(i, newSequenceNo);
            assertEquals(newSequenceNo, instance.getSequenceNo(i));
        }
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileState.
     */
    public void testFtpFileState() throws Exception {
        System.out.println("FtpFileState");
        
        assertNotNull(new FtpFileState());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
