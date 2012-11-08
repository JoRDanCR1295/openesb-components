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
 * @(#)FtpHeuristicsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

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
public class FtpHeuristicsTest extends TestCase {
    FtpHeuristics instance;
    
    public FtpHeuristicsTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpHeuristics();
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpHeuristicsTest.class);
        
        return suite;
    }
    
    /**
     * Test of getAbsPathDelims method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetAbsPathDelims() throws Exception {
        System.out.println("getAbsPathDelims");
        
        String expResult = "///";
        String result = instance.getAbsPathDelims();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getAbsPathEnvelope method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetAbsPathEnvelope() throws Exception {
        System.out.println("getAbsPathEnvelope");
        
        String expResult = "";
        String result = instance.getAbsPathEnvelope();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getDirectoryRegex method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetDirectoryRegex() throws Exception {
        System.out.println("getDirectoryRegex");
        
        assertNotNull(instance.getDirectoryRegex());
        
    }
    
    /**
     * Test of getDirListingStyle method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetDirListingStyle() throws Exception {
        System.out.println("getDirListingStyle");
        
        String expResult = "UNIX";
        String result = instance.getDirListingStyle();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileExtLength method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileExtLength() throws Exception {
        System.out.println("getFileExtLength");
        
        int expResult = 0;
        int result = instance.getFileExtLength();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileExtPosn method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileExtPosn() throws Exception {
        System.out.println("getFileExtPosn");
        
        int expResult = 0;
        int result = instance.getFileExtPosn();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileLinkRegex method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileLinkRegex() throws Exception {
        System.out.println("getFileLinkRegex");
        
        assertNotNull(instance.getFileLinkRegex());
        
    }
    
    /**
     * Test of getFileNameLength method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileNameLength() throws Exception {
        System.out.println("getFileNameLength");
        
        int expResult = 0;
        int result = instance.getFileNameLength();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileNamePosn method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileNamePosn() throws Exception {
        System.out.println("getFileNamePosn");
        
        int expResult = 9;
        int result = instance.getFileNamePosn();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileSizeLength method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileSizeLength() throws Exception {
        System.out.println("getFileSizeLength");
        
        int expResult = 0;
        int result = instance.getFileSizeLength();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getFileSizePosn method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetFileSizePosn() throws Exception {
        System.out.println("getFileSizePosn");
        
        int expResult = 5;
        int result = instance.getFileSizePosn();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getHeaderSkipLines method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetHeaderSkipLines() throws Exception {
        System.out.println("getHeaderSkipLines");
        
        int expResult = 1;
        int result = instance.getHeaderSkipLines();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getHeaderSkipRegex method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetHeaderSkipRegex() throws Exception {
        System.out.println("getHeaderSkipRegex");
        
        assertNotNull(instance.getHeaderSkipRegex());
        
    }
    
    /**
     * Test of getLinkSymbolRegex method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetLinkSymbolRegex() throws Exception {
        System.out.println("getLinkSymbolRegex");
        
        assertNotNull(instance.getLinkSymbolRegex());
        
    }
    
    /**
     * Test of getListLineFormat method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetListLineFormat() throws Exception {
        System.out.println("getListLineFormat");
        
        assertNotNull(instance.getListLineFormat());
        
    }
    
    /**
     * Test of getProps method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetProps() throws Exception {
        System.out.println("getProps");
        
        assertNotNull(instance.getProps());
        
    }
    
    /**
     * Test of getSupportCmdMask method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetSupportCmdMask() throws Exception {
        System.out.println("getSupportCmdMask");
        
        assertNotNull(instance.getSupportCmdMask());
        
    }
    
    /**
     * Test of getTrailerSkipLines method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetTrailerSkipLines() throws Exception {
        System.out.println("getTrailerSkipLines");
        
        int expResult = 0;
        int result = instance.getTrailerSkipLines();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getTrailerSkipRegex method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetTrailerSkipRegex() throws Exception {
        System.out.println("getTrailerSkipRegex");
        
        String expResult = "";
        String result = instance.getTrailerSkipRegex();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getValidFileLineMinPosn method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetValidFileLineMinPosn() throws Exception {
        System.out.println("getValidFileLineMinPosn");
        
        int expResult = 9;
        int result = instance.getValidFileLineMinPosn();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isCdBeforeListing method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsCdBeforeListing() throws Exception {
        System.out.println("isCdBeforeListing");
        
        boolean expResult = false;
        boolean result = instance.isCdBeforeListing();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isDirRequireTerminator method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsDirRequireTerminator() throws Exception {
        System.out.println("isDirRequireTerminator");
        
        boolean expResult = false;
        boolean result = instance.isDirRequireTerminator();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isFileLinkRealData method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsFileLinkRealData() throws Exception {
        System.out.println("isFileLinkRealData");
        
        boolean expResult = true;
        boolean result = instance.isFileLinkRealData();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isFileNameIsLastObject method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsFileNameIsLastObject() throws Exception {
        System.out.println("isFileNameIsLastObject");
        
        boolean expResult = true;
        boolean result = instance.isFileNameIsLastObject();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isFileSizeVerifiable method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsFileSizeVerifiable() throws Exception {
        System.out.println("isFileSizeVerifiable");
        
        boolean expResult = true;
        boolean result = instance.isFileSizeVerifiable();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isListDirYieldsAbsPath method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsListDirYieldsAbsPath() throws Exception {
        System.out.println("isListDirYieldsAbsPath");
        
        boolean expResult = false;
        boolean result = instance.isListDirYieldsAbsPath();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of buildFullFileName method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testBuildFullFileName() throws Exception {
        System.out.println("buildFullFileName");
        
        String dirName = "";
        String baseFileName = "";
        String expResult = null;
        String result = instance.buildFullFileName(dirName, baseFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getBaseFileName method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetBaseFileName() throws Exception {
        System.out.println("getBaseFileName");
        
        String fullFileName = "";
        String expResult = null;
        String result = instance.getBaseFileName(fullFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getEndingFileDelimiter method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetEndingFileDelimiter() throws Exception {
        System.out.println("getEndingFileDelimiter");
        
        String expResult = "";
        String result = instance.getEndingFileDelimiter();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getInitDirDelimiter method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetInitDirDelimiter() throws Exception {
        System.out.println("getInitDirDelimiter");
        
        String expResult = "/";
        String result = instance.getInitDirDelimiter();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getInitFileDelimiter method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetInitFileDelimiter() throws Exception {
        System.out.println("getInitFileDelimiter");
        
        String expResult = "/";
        String result = instance.getInitFileDelimiter();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getInterDirDelimiter method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetInterDirDelimiter() throws Exception {
        System.out.println("getInterDirDelimiter");
        
        String expResult = "/";
        String result = instance.getInterDirDelimiter();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isBlankDelimited method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsBlankDelimited() throws Exception {
        System.out.println("isBlankDelimited");
        
        boolean expResult = true;
        boolean result = instance.isBlankDelimited();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isFixedFormat method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testIsFixedFormat() throws Exception {
        System.out.println("isFixedFormat");
        
        boolean expResult = false;
        boolean result = instance.isFixedFormat();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of parseALine method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testParseALine() throws Exception {
        System.out.println("parseALine");
        
        String aLine = "";
        FtpHeuristicsFile expResult = null;
        FtpHeuristicsFile result = instance.parseALine(aLine);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of skipLines method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testSkipLines() throws Exception {
        System.out.println("skipLines");
        
        String[] lines = null;
        String[] expResult = null;
        String[] result = instance.skipLines(lines);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getDirectoryName method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testGetDirectoryName() throws Exception {
        System.out.println("getDirectoryName");
        
        String fullFileName = "";
        String expResult = null;
        String result = instance.getDirectoryName(fullFileName);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of escapeRegExpChars method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testEscapeRegExpChars() throws Exception {
        System.out.println("escapeRegExpChars");
        
        String input = "";
        String expResult = "";
        String result = instance.escapeRegExpChars(input);
        assertEquals(expResult, result);
        
        input = null;
        expResult = null;
        result = instance.escapeRegExpChars(input);
        assertEquals(expResult, result);
        
        input = "*[]()|+{}:.^$?\\";
        expResult = "\\*\\[\\]\\(\\)\\|\\+\\{\\}\\:\\.\\^\\$\\?\\\\";
        result = instance.escapeRegExpChars(input);
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpHeuristics.
     */
    public void testFtpHeuristics() throws Exception {
        System.out.println("FtpHeuristics");
        
        assertNotNull("default one will be used", new FtpHeuristics());
        assertNotNull(new FtpHeuristics("UNIX"));
        try {
            assertNotNull(new FtpHeuristics("abc", "/"));
            fail("An exception is expected - invalid user defined heuristics");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
