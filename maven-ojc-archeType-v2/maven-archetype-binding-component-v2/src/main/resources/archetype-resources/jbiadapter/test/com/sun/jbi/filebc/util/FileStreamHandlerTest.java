#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})FileStreamHandlerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import junit.framework.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PushbackInputStream;

/**
 *
 * @author sweng
 */
public class FileStreamHandlerTest extends TestCase {

    FileStreamHandler instance;

    public FileStreamHandlerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getAllContentsAsBytes method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testGetAllContentsAsBytesCase1() throws Exception {
        System.out.println("Testing getAllContentsAsBytes case 1");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "SingleRecord.dat");
        FileInputStream aStream = new FileInputStream(inputFile);

        // tests the scenario where:
        // 1. single record per file
        // 2. maxRecordSize not defined
        // 3. dropEOL option not defined
        instance = new FileStreamHandler(false,
                new byte[]{},
                -1,
                inputFile.length());

        byte[] result = instance.getAllContentsAsBytes(aStream);
        assertTrue("This is a test of Single_Record_Per_File scenario${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "This is a test of Single_Record_Per_File scenario${symbol_escape}n".equals(new String(result)));

        System.out.println("Successfully tested getAllContentsByBytes case 1");
    }

    public void testGetAllContentsAsBytesCase2() throws Exception {
        System.out.println("Testing getAllContentsAsBytes case 2");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "SingleRecord.dat");
        InputStream aStream = new FileInputStream(inputFile);

        //  test the scenario where:
        //  1. single record per file
        //  2. maxRecord defined and less than file size
        //  3. dropEOL option defined but no EOR should be found
        instance = new FileStreamHandler(true,
                new byte[]{},
                18,
                inputFile.length());
        byte[] result = instance.getAllContentsAsBytes(aStream);
        String expResult = "This is a test of ";
        System.out.println("Result is [" + result + "]");
        assertTrue(expResult.equals(new String(result)));


        System.out.println("Successfully tested getAllContentsByBytes case 2");
    }

    public void testGetAllContentsAsBytesCase3() throws Exception {
        System.out.println("Testing getAllContentsAsBytes case 3");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "SingleRecord.dat");
        InputStream aStream = new FileInputStream(inputFile);

        //  test the scenario where:
        //  1. single record per file
        //  2. maxRecord not defined
        //  3. dropEOL option defined
        instance = new FileStreamHandler(true,
                new byte[]{},
                -1,
                inputFile.length());
        byte[] result = instance.getAllContentsAsBytes(aStream);
        String expResult = "This is a test of Single_Record_Per_File scenario";
        System.out.println("Result is [" + result + "]");
        assertTrue(expResult.equals(new String(result)));


        System.out.println("Successfully tested getAllContentsByBytes case 3");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase1() throws Exception {
        System.out.println("Testing readNextRecord case 1");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "FixedLengthRecords.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        String expResult;
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined with fixed record read size
        //  3. dropEOL option not defined
        //  4. record delimiter not defined 

        // this case test reading fixed-length multiple records
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{},
                (long) 8,
                inputFile.length());
        expResult = "aaaaaaaa";
        result = instance.readNextRecord();

        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the second record
        expResult = "bbbbbbbb";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the last record
        expResult = "cccccccc";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));


        // test using instance.hasMoreRecords() method for remaining data
        int remainingDataCount = 0;
        while (instance.hasMoreRecords()) {
            result = instance.readNextRecord();
            if (result != null) {
                remainingDataCount++;
            }
        }
        if ("cccccccc".equals(new String(result))) {
            fail("Failed to retrieve remaining data records...");
        }
        //assertTrue(result.length < 6);  // last record has only 6 bytes
        //assertTrue(remainingDataCount > 3);

        System.out.println("Successfully tested readNextRecord case 1");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase2() throws Exception {
        System.out.println("Testing readNextRecord case 2");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "FixedLengthRecords.dat");
        PushbackInputStream aStream = new PushbackInputStream(new FileInputStream(inputFile));
        String expResult;
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined with fixed record read size
        //  3. dropEOL option defined as true
        //  4. record delimiter not defined (not needed)
        //  5. use the alternative constructor and methods
        instance = new FileStreamHandler(true,
                new byte[]{},
                (long) 8,
                inputFile.length());


        expResult = "aaaaaaaa";
        result = instance.readNextRecord(aStream);

        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the second record
        expResult = "bbbbbbbb";
        result = instance.readNextRecord(aStream);
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the last record
        expResult = "cccccccc";
        result = instance.readNextRecord(aStream);
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));


        // test using instance.hasMoreRecords() method for remaining data
        int remainingDataCount = 0;
        while (instance.hasMoreRecords(aStream)) {
            result = instance.readNextRecord(aStream);
            if (result != null) {
                remainingDataCount++;
            }
        }
        if ("cccccccc".equals(new String(result))) {
            fail("Failed to retrieve remaining data records...");
        }

        //assertTrue(result.length == 0);
        //assertTrue(remainingDataCount == 3);

        System.out.println("Successfully tested readNextRecord case 2");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase3() throws Exception {
        System.out.println("Testing readNextRecord case 3");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord not defined
        //  3. dropEOL option not defined
        //  4. record delimiter defined 


        // read the first record
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                -1,
                inputFile.length());
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("This is record number1${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "This is record number1${symbol_escape}n${symbol_escape}n".equals(new String(result)));

        // read the second record
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("${symbol_escape}r${symbol_escape}nThis is record number 2".equals(new String(result)) ||
                "${symbol_escape}nThis is record number 2".equals(new String(result)));

        // read the last record
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("This is record number3${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "This is record number3${symbol_escape}n${symbol_escape}n".equals(new String(result)));

        System.out.println("Successfully tested readNextRecord case 3");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase4() throws Exception {
        System.out.println("Testing readNextRecord case 4");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        //String expResult; 
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined
        //  3. dropEOL option not defined
        //  4. record delimiter defined 

        // tests the scenario where maxRecordSize 
        // is less than the actual data size
        // read the first record
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                22,
                inputFile.length());
        //expResult = "This is record number1";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("This is record number1".equals(new String(result)));

        // read the second record
        //expResult = "${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "${symbol_escape}n${symbol_escape}n".equals(new String(result)));


        // read the last record
        //expResult = "${symbol_escape}r${symbol_escape}nThis is record numbe";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("${symbol_escape}r${symbol_escape}nThis is record numbe".equals(new String(result)) ||
                "${symbol_escape}nThis is record number".equals(new String(result)));

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined
        //  3. dropEOL option defined to be true
        //  4. record delimiter defined 
        //  5.  tests the scenario where maxRecordSize 
        //      is larger than the actual data size 
        aStream = new FileInputStream(inputFile);
        instance = new FileStreamHandler(aStream,
                true,
                new byte[]{'|', '|', '|'},
                50,
                inputFile.length());

        // read the first record
        //expResult = "This is record number1${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("This is record number1${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "This is record number1${symbol_escape}n${symbol_escape}n".equals(new String(result)));

        // read the second record
        //expResult = "${symbol_escape}r${symbol_escape}nThis is record number 2";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("${symbol_escape}r${symbol_escape}nThis is record number 2".equals(new String(result)) ||
                "${symbol_escape}nThis is record number 2".equals(new String(result)));

        // read the last record
        //expResult = "This is record number3${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("This is record number3${symbol_escape}r${symbol_escape}n${symbol_escape}r${symbol_escape}n".equals(new String(result)) ||
                "This is record number3${symbol_escape}n${symbol_escape}n".equals(new String(result)));

        System.out.println("Successfully tested readNextRecord case 4");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase5() throws Exception {
        System.out.println("Testing readNextRecord case 5");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords2.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        String expResult;
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined
        //  3. dropEOL option not defined
        //  4. record delimiter defined 

        // tests the scenario where partial EOR 
        // bytes are present in the data records

        // read the first record
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                22,
                inputFile.length());
        expResult = "This |is record number";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the second record
        expResult = "1";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));


        //expResult = "${symbol_escape}r${symbol_escape}nThis is re||cord num";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue("${symbol_escape}r${symbol_escape}nThis is re||cord num".equals(new String(result)) ||
                "${symbol_escape}nThis is re||cord numb".equals(new String(result)));

//        expResult = "ber 2";
//        result = instance.readNextRecord();
//        if (result == null) {
//            fail("Unexpected IO error has occurred");
//        }
//        assertTrue(expResult.equals(new String(result)));

        System.out.println("Successfully tested readNextRecord case 5");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase6() throws Exception {
        System.out.println("Testing readNextRecord case 6");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords3.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        String expResult;
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined
        //  3. dropEOL defined
        //  4. record delimiter defined 

        // tests the scenario where maxRecordSize 
        // is less than the actual data size
        // read the first record
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'&', '&'},
                10,
                inputFile.length());
        expResult = "This is re";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the second record
        expResult = "cord numbe";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));


        expResult = "r 1";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        //expResult = "2ndRecord${symbol_escape}r";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue((new String(result).indexOf("2ndRecord") == 0));

        System.out.println("Successfully tested readNextRecord case 6");
    }

    /**
     * Test of readNextRecord method, of class com.sun.jbi.filebc.util.FileStreamHandler.
     */
    public void testReadNextRecordCase7() throws Exception {
        System.out.println("Testing readNextRecord case 7");

        File inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords4.dat");
        FileInputStream aStream = new FileInputStream(inputFile);
        String expResult;
        byte[] result;

        //  test the scenario where:
        //  1. multiple records per file
        //  2. maxRecord defined
        //  3. dropEOL defined
        //  4. record delimiter defined 

        // tests the scenario max record size is 
        // less than actual data size and partial
        // EOR sequence is found at the boundary of 
        // read buffers

        // test the scenario when EOR is found
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                10,
                inputFile.length());
        expResult = "aaaaaaa";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // read the second record
        expResult = "bbbbbb";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));


        expResult = "cccccccccc";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));



        // test the scenario where EOR is not found
        inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords5.dat");
        aStream = new FileInputStream(inputFile);
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                10,
                inputFile.length());
        expResult = "aaaaaaa||a";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "bbbbbb";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "cccccccccc";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "ccccccccc";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // test the scenario where EOR is not found
        // but partial EOR sequence at max record size limit
        inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords6.dat");
        aStream = new FileInputStream(inputFile);
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '|', '|'},
                10,
                inputFile.length());
        expResult = "aaaaaaaaa|";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "aabbbbbb";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "cccccccccc";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        // test the scenario partial EOR is embedded in data
        inputFile = new File("test/com/sun/jbi/filebc/util/testInput", "MultipleRecords7.dat");
        aStream = new FileInputStream(inputFile);
        instance = new FileStreamHandler(aStream,
                false,
                new byte[]{'|', '&', '^', '${symbol_dollar}'},
                100,
                inputFile.length());
        expResult = "This |&is record |&^number 1";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "This is record number2";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "This is rec&&ord number3333333333333333333333333333333333333333333333";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        expResult = "This is the last record";
        result = instance.readNextRecord();
        if (result == null) {
            fail("Unexpected IO error has occurred");
        }
        assertTrue(expResult.equals(new String(result)));

        System.out.println("Successfully tested readNextRecord case 7");
    }
}
