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
 * @(#)LocalFileConfigurationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;

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
public class LocalFileConfigurationTest extends TestCase {
    
    public LocalFileConfigurationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileConfigurationTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        Properties props = null;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.initialize(props);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of reset method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testReset() throws Exception {
        System.out.println("reset");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.reset();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getOID method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetOID() {
        System.out.println("getOID");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getOID();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getExternalName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetExternalName() {
        System.out.println("getExternalName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getExternalName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of validateBasics method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testValidateBasics() throws Exception {
        System.out.println("validateBasics");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.validateBasics();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTransactionType method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetTransactionType() {
        System.out.println("getTransactionType");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getTransactionType();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getResumeReadingEnabled method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetResumeReadingEnabled() {
        System.out.println("getResumeReadingEnabled");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getResumeReadingEnabled();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setResumeReadingEnabled method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetResumeReadingEnabled() {
        System.out.println("setResumeReadingEnabled");
        
        boolean resumeReadingEnabled = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setResumeReadingEnabled(resumeReadingEnabled);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getStatePersistenceBaseLocation method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetStatePersistenceBaseLocation() {
        System.out.println("getStatePersistenceBaseLocation");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getStatePersistenceBaseLocation();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTargetDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetTargetDirectoryName() {
        System.out.println("getTargetDirectoryName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getTargetDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setTargetDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetTargetDirectoryName() throws Exception {
        System.out.println("setTargetDirectoryName");
        
        String targetDirectoryName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setTargetDirectoryName(targetDirectoryName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTargetDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetTargetDirectoryNameIsPattern() {
        System.out.println("getTargetDirectoryNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getTargetDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setTargetDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetTargetDirectoryNameIsPattern() throws Exception {
        System.out.println("setTargetDirectoryNameIsPattern");
        
        boolean targetDirectoryNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setTargetDirectoryNameIsPattern(targetDirectoryNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTargetFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetTargetFileName() {
        System.out.println("getTargetFileName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getTargetFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setTargetFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetTargetFileName() throws Exception {
        System.out.println("setTargetFileName");
        
        String targetFileName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setTargetFileName(targetFileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getTargetFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetTargetFileNameIsPattern() {
        System.out.println("getTargetFileNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getTargetFileNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setTargetFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetTargetFileNameIsPattern() throws Exception {
        System.out.println("setTargetFileNameIsPattern");
        
        boolean targetFileNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setTargetFileNameIsPattern(targetFileNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getAppend method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetAppend() {
        System.out.println("getAppend");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getAppend();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setAppend method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetAppend() {
        System.out.println("setAppend");
        
        boolean append = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setAppend(append);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPreTransferCommand() {
        System.out.println("getPreTransferCommand");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPreTransferCommand();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPreTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPreTransferCommand() {
        System.out.println("setPreTransferCommand");
        
        String preTransferCommand = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPreTransferCommand(preTransferCommand);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPreDirectoryName() {
        System.out.println("getPreDirectoryName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPreDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPreDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPreDirectoryName() {
        System.out.println("setPreDirectoryName");
        
        String preDirectoryName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPreDirectoryName(preDirectoryName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPreDirectoryNameIsPattern() {
        System.out.println("getPreDirectoryNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPreDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPreDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPreDirectoryNameIsPattern() {
        System.out.println("setPreDirectoryNameIsPattern");
        
        boolean preDirectoryNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPreDirectoryNameIsPattern(preDirectoryNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPreFileName() {
        System.out.println("getPreFileName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPreFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPreFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPreFileName() {
        System.out.println("setPreFileName");
        
        String preFileName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPreFileName(preFileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPreFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPreFileNameIsPattern() {
        System.out.println("getPreFileNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPreFileNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPreFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPreFileNameIsPattern() {
        System.out.println("setPreFileNameIsPattern");
        
        boolean preFileNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPreFileNameIsPattern(preFileNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPostTransferCommand() {
        System.out.println("getPostTransferCommand");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPostTransferCommand();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPostTransferCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPostTransferCommand() {
        System.out.println("setPostTransferCommand");
        
        String postTransferCommand = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPostTransferCommand(postTransferCommand);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPostDirectoryName() {
        System.out.println("getPostDirectoryName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPostDirectoryName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPostDirectoryName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPostDirectoryName() {
        System.out.println("setPostDirectoryName");
        
        String PostDirectoryName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPostDirectoryName(PostDirectoryName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPostDirectoryNameIsPattern() {
        System.out.println("getPostDirectoryNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPostDirectoryNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPostDirectoryNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPostDirectoryNameIsPattern() {
        System.out.println("setPostDirectoryNameIsPattern");
        
        boolean PostDirectoryNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPostDirectoryNameIsPattern(PostDirectoryNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPostFileName() {
        System.out.println("getPostFileName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getPostFileName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPostFileName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPostFileName() {
        System.out.println("setPostFileName");
        
        String PostFileName = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPostFileName(PostFileName);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPostFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPostFileNameIsPattern() {
        System.out.println("getPostFileNameIsPattern");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPostFileNameIsPattern();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPostFileNameIsPattern method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPostFileNameIsPattern() {
        System.out.println("setPostFileNameIsPattern");
        
        boolean PostFileNameIsPattern = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPostFileNameIsPattern(PostFileNameIsPattern);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getStartingSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetStartingSequenceNumber() {
        System.out.println("getStartingSequenceNumber");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        int expResult = 0;
        int result = instance.getStartingSequenceNumber();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setStartingSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetStartingSequenceNumber() {
        System.out.println("setStartingSequenceNumber");
        
        int startingSequenceNumber = 0;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setStartingSequenceNumber(startingSequenceNumber);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getMaxSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetMaxSequenceNumber() {
        System.out.println("getMaxSequenceNumber");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        int expResult = 0;
        int result = instance.getMaxSequenceNumber();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setMaxSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetMaxSequenceNumber() {
        System.out.println("setMaxSequenceNumber");
        
        int maxSequenceNumber = 0;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setMaxSequenceNumber(maxSequenceNumber);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getDynamicConfiguration method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetDynamicConfiguration() {
        System.out.println("getDynamicConfiguration");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        LocalFileConfiguration expResult = null;
        LocalFileConfiguration result = instance.getDynamicConfiguration();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPublishStatusRecordOnSuccess method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPublishStatusRecordOnSuccess() {
        System.out.println("getPublishStatusRecordOnSuccess");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPublishStatusRecordOnSuccess();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPublishStatusRecordOnSuccess method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPublishStatusRecordOnSuccess() {
        System.out.println("setPublishStatusRecordOnSuccess");
        
        boolean newPublishStatusRecordOnSuccess = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPublishStatusRecordOnSuccess(newPublishStatusRecordOnSuccess);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPublishStatusRecordOnError method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetPublishStatusRecordOnError() {
        System.out.println("getPublishStatusRecordOnError");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getPublishStatusRecordOnError();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPublishStatusRecordOnError method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetPublishStatusRecordOnError() {
        System.out.println("setPublishStatusRecordOnError");
        
        boolean newPublishStatusRecordOnError = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setPublishStatusRecordOnError(newPublishStatusRecordOnError);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getIncludeOrderRecordInErrorRecord method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetIncludeOrderRecordInErrorRecord() {
        System.out.println("getIncludeOrderRecordInErrorRecord");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getIncludeOrderRecordInErrorRecord();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setIncludeOrderRecordInErrorRecord method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetIncludeOrderRecordInErrorRecord() {
        System.out.println("setIncludeOrderRecordInErrorRecord");
        
        boolean newIncludeOrderRecordInErrorRecord = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setIncludeOrderRecordInErrorRecord(newIncludeOrderRecordInErrorRecord);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getIncludePayloadInErrorRecord method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetIncludePayloadInErrorRecord() {
        System.out.println("getIncludePayloadInErrorRecord");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getIncludePayloadInErrorRecord();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setIncludePayloadInErrorRecord method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetIncludePayloadInErrorRecord() {
        System.out.println("setIncludePayloadInErrorRecord");
        
        boolean newIncludePayloadInErrorRecord = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setIncludePayloadInErrorRecord(newIncludePayloadInErrorRecord);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getActionOnMalformedCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetActionOnMalformedCommand() {
        System.out.println("getActionOnMalformedCommand");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getActionOnMalformedCommand();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setActionOnMalformedCommand method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetActionOnMalformedCommand() {
        System.out.println("setActionOnMalformedCommand");
        
        String newActionOnMalformedCommand = "";
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setActionOnMalformedCommand(newActionOnMalformedCommand);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getClientClassName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetClientClassName() {
        System.out.println("getClientClassName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getClientClassName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getProviderClassName method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetProviderClassName() {
        System.out.println("getProviderClassName");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getProviderClassName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getExtensionPropertiesFile method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetExtensionPropertiesFile() {
        System.out.println("getExtensionPropertiesFile");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.getExtensionPropertiesFile();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getExtensionProperties method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetExtensionProperties() {
        System.out.println("getExtensionProperties");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        Properties expResult = null;
        Properties result = instance.getExtensionProperties();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of toString method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testToString() {
        System.out.println("toString");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        String expResult = "";
        String result = instance.toString();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setSynchronized method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testSetSynchronized() {
        System.out.println("setSynchronized");
        
        boolean b = true;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.setSynchronized(b);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getSynchronized method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testGetSynchronized() {
        System.out.println("getSynchronized");
        
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        boolean expResult = true;
        boolean result = instance.getSynchronized();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of registerConfigChangeListener method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testRegisterConfigChangeListener() {
        System.out.println("registerConfigChangeListener");
        
        ConfigChangeListener l = null;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.registerConfigChangeListener(l);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of unregisterConfigChangeListener method, of class com.sun.jbi.batchext.local.LocalFileConfiguration.
     */
    public void testUnregisterConfigChangeListener() {
        System.out.println("unregisterConfigChangeListener");
        
        ConfigChangeListener l = null;
        LocalFileConfiguration instance = new LocalFileConfiguration();
        
        instance.unregisterConfigChangeListener(l);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
