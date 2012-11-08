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
 * @(#)FtpDirRegExpTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.ftp;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
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
public class FtpDirRegExpTest extends TestCase {

    String dir1 = "\\Products\\Windows";
    String dir2 = "\\Products\\peropsys";
    String dir3 = "\\PSS\\Tools";
    String dir4 = "\\peropsys\\windows";
    String dir5 = "\\deskapps\\games";
    ArrayList result = new ArrayList();
    FtpDirRegExp instance;
    FtpInterface ftp;

    public FtpDirRegExpTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        try {
            ftp = new FtpInterface();
            Properties props = new Properties();
            props.put("FTP/Host Name", "ftp.microsoft.com");
            props.put("FTP/Directory Listing Style", /*"UNIX"*/ "NT 4.0");
            props.put("General Settings/Connection Mode", "Automatic");
            ftp.initialize(props);

            ftp.getProvider().setUseRegexAsMatcher(true);
            instance = new FtpDirRegExp("/not/existed/dir/pattern", ftp.getProvider());
        } catch (Exception e) {
            e.printStackTrace();
            this.tearDown();
        }
    }

    protected void tearDown() throws Exception {
        try {
            ftp.reset();
            if (ftp.getClient() != null) {
                ftp.getClient().close();
            }
        } catch (Exception e) {
            ;
        }
        ftp = null;
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpDirRegExpTest.class);

        return suite;
    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_0() throws Exception {
        System.out.println("getDirs_0");

        try {
            ArrayList expResult = new ArrayList();
            result = instance.getDirs();
            assertEquals("An empty ArrayList is returned for nothing-match", expResult, result);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("This test testGetDirs_0 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_1() throws Exception {
        System.out.println("getDirs_1");

        try {
            // one and only one "/Products/Windows"
            instance = new FtpDirRegExp(dir1, ftp.getProvider());
            result = instance.getDirs();
            assertEquals(1, result.size());
            assertEquals(dir1, result.get(0).toString());
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("This test testGetDirs_1 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_2() throws Exception {
        System.out.println("getDirs_2");

//        try {
//            // one and only one "/Products/peropsys"
//            instance = new FtpDirRegExp(dir2, ftp.getProvider());
//            result = instance.getDirs();
//            assertEquals(1, result.size());
//            assertEquals(dir2, result.get(0).toString());
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_2 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_3() throws Exception {
        System.out.println("getDirs_3");

//        try {
//            // one and only one "/PSS/Tools"
//            instance = new FtpDirRegExp(dir3, ftp.getProvider());
//            result = instance.getDirs();
//            assertEquals(1, result.size());
//            assertEquals(dir3, result.get(0).toString());
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_3 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_4() throws Exception {
        System.out.println("getDirs_4");

//        try {
//            // one and only one "/peropsys/windows"
//            instance = new FtpDirRegExp(dir4, ftp.getProvider());
//            result = instance.getDirs();
//            assertEquals(1, result.size());
//            assertEquals(dir4, result.get(0).toString());
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_4 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_5() throws Exception {
        System.out.println("getDirs_5");

//        try {
//            // one and only one "/deskapps/games"
//            instance = new FtpDirRegExp(dir5, ftp.getProvider());
//            result = instance.getDirs();
//            assertEquals(1, result.size());
//            assertEquals(dir5, result.get(0).toString());
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_5 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_m1() throws Exception {
        System.out.println("getDirs_m1");

//        try {
//            // "/^P/s$" will match 
//            //          "/Products/Windows"     dir1
//            //          "/Products/peropsys"    dir2
//            //          "/PSS/Tools"            dir3
//            // but not match 
//            //          "/peropsys/windows"     dir4
//            //          "/deskapps/games"       dir5
//            instance = new FtpDirRegExp("/^P.*/.*s$", ftp.getProvider());
//            result = instance.getDirs();
//			printResult(result);
//            assertTrue(3 <= result.size());
//            assertTrue(result.contains(dir1));
//            assertTrue(result.contains(dir2));
//            assertTrue(result.contains(dir3));
//            assertTrue(!result.contains(dir4));
//            assertTrue(!result.contains(dir5));
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_m1 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_m2() throws Exception {
        System.out.println("getDirs_m2");

//        try {
//            // "/s$/s$" will match 
//            //          "/Products/Windows"     dir1
//            //          "/Products/peropsys"    dir2
//            //          "/peropsys/windows"     dir4
//            //          "/deskapps/games"       dir5
//            // but not match 
//            //          "/PSS/Tools"            dir3
//            instance = new FtpDirRegExp("/.*s$/.*s$", ftp.getProvider());
//            result = instance.getDirs();
//			printResult(result);
//            assertTrue(4 <= result.size());
//            assertTrue(result.contains(dir1));
//            assertTrue(result.contains(dir2));
//            assertTrue(!result.contains(dir3));
//            assertTrue(result.contains(dir4));
//            assertTrue(result.contains(dir5));
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_m2 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_m3() throws Exception {
        System.out.println("getDirs_m3");

//        try {
//            // "/.+/o.s$" will match 
//            //          "/Products/Windows"     dir1
//            //          "/PSS/Tools"            dir3
//            //          "/peropsys/windows"     dir4
//            // but not match 
//            //          "/Products/peropsys"    dir2
//            //          "/deskapps/games"       dir5
//            instance = new FtpDirRegExp("/.+/.*o.s$", ftp.getProvider());
//            result = instance.getDirs();
//			printResult(result);
//            assertTrue(3 <= result.size());
//            assertTrue(result.contains(dir1));
//            assertTrue(!result.contains(dir2));
//            assertTrue(result.contains(dir3));
//            assertTrue(result.contains(dir4));
//            assertTrue(!result.contains(dir5));
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_m3 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_m4() throws Exception {
        System.out.println("getDirs_m4");

//        try {
//            // "/[Pp]/s" will match all
//            //          "/Products/Windows"     dir1
//            //          "/Products/peropsys"    dir2
//            //          "/PSS/Tools"            dir3
//            //          "/peropsys/windows"     dir4
//            //          "/deskapps/games"       dir5
//            instance = new FtpDirRegExp("/.*[Pp].*/.*s.*", ftp.getProvider());
//            result = instance.getDirs();
//			printResult(result);
//            assertTrue(5 <= result.size());
//            assertTrue(result.contains(dir1));
//            assertTrue(result.contains(dir2));
//            assertTrue(result.contains(dir3));
//            assertTrue(result.contains(dir4));
//            assertTrue(result.contains(dir5));
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_m4 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of getDirs method, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testGetDirs_m5() throws Exception {
        System.out.println("getDirs_m5");

//        try {
//            // "/o/s" will match 
//            //          "/Products/Windows"     dir1
//            //          "/Products/peropsys"    dir2
//            //          "/peropsys/windows"     dir4
//            // but not match 
//            //          "/PSS/Tools"            dir3
//            //          "/deskapps/games"       dir5
//            instance = new FtpDirRegExp("/.*o.*/.*s.*", ftp.getProvider());
//            result = instance.getDirs();
//			printResult(result);
//			assertTrue(3 <= result.size());
//            assertTrue(result.contains(dir1));
//            assertTrue(result.contains(dir2));
//            assertTrue(!result.contains(dir3));
//            assertTrue(result.contains(dir4));
//            assertTrue(!result.contains(dir5));
//        } catch (Exception e) {
//            e.printStackTrace();
//            System.out.println("This test testGetDirs_m5 may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
//        }

    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpDirRegExp.
     */
    public void testFtpDirRegExp() throws Exception {
        System.out.println("FtpDirRegExp");

        try {
            assertNotNull(new FtpDirRegExp("A pattern", new FtpFileProviderImpl()));
            assertNotNull(new FtpDirRegExp(null, new FtpFileProviderImpl()));
            assertNotNull(new FtpDirRegExp("A pattern", null));
            assertNotNull(new FtpDirRegExp(null, null));
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("This test testFtpDirRegExp may succeed or fail depending on the connection status and site structure on ftp.microsoft.com as for 10/31/2006.");
        }

    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }

    private void printResult(List result) {
        System.out.println("result size =" + result.size());
        if (result != null && result.size() > 0) {
            for (int i = 0; i < result.size(); i++) {
                System.out.println("result[" + i + "]=" + result.get(i));
            }
        }
    }
}
