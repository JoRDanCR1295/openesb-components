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
 * @(#)FtpHeuristicsPropertiesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.IOException;
import java.io.InputStream;
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
public class FtpHeuristicsPropertiesTest extends TestCase {
    FtpHeuristicsProperties instance;
    
    public FtpHeuristicsPropertiesTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpHeuristicsProperties("UNIX");
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpHeuristicsPropertiesTest.class);
        
        return suite;
    }

    /**
     * Test of loadConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsProperties.
     */
    public void testLoadConfiguration() throws Exception {
        System.out.println("loadConfiguration");
        
        InputStream fin = null;
        try {
            instance.loadConfiguration(fin);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of main method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsProperties.
     */
    public void testMain() throws Exception {
        System.out.println("main");
        
        String[] args = null;
        
        FtpHeuristicsProperties.main(args);
        
    }

    /**
     * Test of load method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsProperties.
     */
    public void testLoad() throws Exception {
        System.out.println("load");
        
        String resName = "not.existed.file";
        try {
            instance.load(resName);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsProperties.
     */
    public void testFtpHeuristicsProperties() throws Exception {
        System.out.println("FtpHeuristicsProperties");
        
        assertNotNull(new FtpHeuristicsProperties("UNIX"));
        assertNotNull("not check style yet, so wrong style is ok", new FtpHeuristicsProperties("WrongStyle"));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
