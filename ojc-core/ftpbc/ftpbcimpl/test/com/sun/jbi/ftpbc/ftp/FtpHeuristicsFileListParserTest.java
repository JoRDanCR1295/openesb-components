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
 * @(#)FtpHeuristicsFileListParserTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.io.InputStream;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.commons.net.ftp.FTPFile;

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
public class FtpHeuristicsFileListParserTest extends TestCase {
    FtpHeuristicsFileListParser instance;
    
    public FtpHeuristicsFileListParserTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpHeuristicsFileListParser();
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpHeuristicsFileListParserTest.class);
        
        return suite;
    }
    
    /**
     * Test of parseFileList method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFileListParser.
     */
    public void testParseFileList_in() throws Exception {
        System.out.println("parseFileList_in");
        
        InputStream in = null;
        FTPFile[] expResult = null;
        FTPFile[] result;
        try {
            result = instance.parseFileList(in);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of parseFileList method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFileListParser.
     */
    public void testParseFileList_in_encoding() throws Exception {
        System.out.println("parseFileList_in_encoding");
        
        InputStream in = null;
        String encoding = "iso-8859-1";
        FTPFile[] expResult = null;
        FTPFile[] result;
        try {
            result = instance.parseFileList(in, encoding);
            fail("An exception is expected - invalid input");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFileListParser.
     */
    public void testFtpHeuristicsFileListParser() throws Exception {
        System.out.println("FtpHeuristicsFileListParser");
        
        assertNotNull(new FtpHeuristicsFileListParser());
        assertNotNull(new FtpHeuristicsFileListParser(new FtpHeuristics()));
        assertNotNull(new FtpHeuristicsFileListParser("UNIX"));
        try {
            assertNotNull(new FtpHeuristicsFileListParser("WrongStyle"));
            fail("An exception is expected - invalid dir listing style");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
