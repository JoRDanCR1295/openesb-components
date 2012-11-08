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
 * GzipEncoderImplTest.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.encoding;

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.custommonkey.xmlunit.XMLTestCase;
import org.custommonkey.xmlunit.XMLUnit;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.zip.DataFormatException;


/**
 * Tests the GZIP Encoding implementation.  This class test both the
 * encoding and the decoding of xml data provided in
 * src/test/unit/resources/AWSIM_UNCLASS_v4.xml
 * @author Phillip Anderson, panderson@gestalt-llc.com
 */
public class GzipEncoderImplTest extends XMLTestCase {
    private static Log log = LogFactory.getLog(GzipEncoderImplTest.class);
    public static final String compressableFilename = "/test.xml";
    private GzipEncoderImpl impl;
    private String compressable;

    public GzipEncoderImplTest(String whichTest) {
        super(whichTest);

        XMLUnit.setIgnoreWhitespace(true);

        if ((null == compressable) || compressable.equals("")) {
            compressable = getXmlInput(compressableFilename);
        }
    }

    /**
     * Runs before every test in the test suite.
     * @throws Exception
     */
    protected void setUp() throws Exception {
        super.setUp();
        impl = (GzipEncoderImpl) EncoderFactory.getInstance(EncoderFactory.GZIP);
    }

    /**
     * Runs after every test in the test suite.
     * @throws Exception
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        impl = null;
    }

    /**
     * Creates a test suite a=specifying what tests to run and in what order.
     * @return Test suite of tests to be run.
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();

        suite.addTest(new GzipEncoderImplTest("testConversionToString"));
        suite.addTest(new GzipEncoderImplTest("testEncode"));
        suite.addTest(new GzipEncoderImplTest("testDecode"));
        suite.addTest(new GzipEncoderImplTest("testDecodeBadData"));

        return suite;
    }

    /**
     * This explicitly tests converting the encoded byte array
     * to a String to demonstrate that users of this API should
     * not do that because they will get an exception with certain data.
     * @throws Exception
     */
    public void testConversionToString() throws Exception {
        String simple = "<james/>";
        String complex = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            + "<UOB version=\"0.5\">" + "<ForceStructureInformation/>"
            + "<Relationships count=\"1\">"
            + "<Relationship type=\"Scenario\">"
            + "<Description>UOB Data extracted SGS Workstation</Description>"
            + "<Assigned>" + "<UnitNode UIC=\"LOC000178\">"
            + "<ChildUnitNode UIC=\"SQU000376\"/>" + "</UnitNode>"
            + "</Assigned>" + "</Relationship>" + "</Relationships>" + "</UOB>";

        byte[] encoded = impl.encode(simple.getBytes());

        try {
            byte[] decoded = impl.decode(encoded);
            assertXMLEqual(simple, new String(decoded));
        } catch (Exception e) {
            fail("Simple data should not return an Exception: " + e);
        }

        encoded = impl.encode(complex.getBytes());

        try {
            impl.decode(new String(encoded).getBytes());
        } catch (Throwable t) {
            log.debug("Received expected error: " + t);
            return;
        }

        if (System.getProperty("os.name").equals("Mac OS X")) {
            log.debug("TEST DOES NOT FAIL ON MAC OS X - Think Different, dude.");
            return;
        }

        fail("Encoded String should return an Error");
    }

    /**
     * Tests the encoding of the xml data read from the file.  Makes sure that
     * the objec tsent to the encode method is not the same as the one returned,
     * that the returned object is a different size and that the content of the
     * returned object is not the same as the original.
     */
    public void testEncode() {
        log.info("testCompress()");

        try {
            byte[] compressed = impl.encode(compressable.getBytes());

            log.info("compressable.length = " + compressable.getBytes().length);
            log.info("  compressed.length = " + compressed.length);
            assertNotSame("Compressed Object is the same as the original object!",
                compressable, compressed);
            assertTrue("Length of the before object is the same as the after, ws expecting them to be different!",
                compressable.getBytes().length != compressed.length);
            assertTrue("Compressed object is equal to the original object!",
                !compressable.equals(compressed));
        } catch (Exception e) {
            fail("Exception occurred while running tests: " + e.toString());
        }
    }

    /**
     * Tests the decoding of encoded xml data.  Insures that the decoded object
     * is not the same object as the original object, that it is not the same
     * object as the encoded object, that is is a different length than the
     * encoded object, that the decoded object is the same length as the original
     * object, that the content of the decoded object is different from the encoded
     * object content, and that the content of the decoded object is the same as
     * the content of the original object.
     */
    public void testDecode() {
        log.info("testUncompress()");

        try {
            byte[] compressed = impl.encode(compressable.getBytes());
            log.info("compressable.length = " + compressable.getBytes().length);
            log.info("  compressed.length = " + compressed.length);

            byte[] uncompressed = impl.decode(compressed);

            log.info("uncompressed.length = " + uncompressed.length);
            assertNotSame("Uncompressed Object is the same as the original object!",
                compressable, uncompressed);
            assertNotSame("Uncompressed Object is the same as the compressed object!",
                compressed, uncompressed);
            assertTrue("Length of the before object is the same as the after, ws expecting them to be different!",
                uncompressed.length != compressed.length);

            assertEquals("Length of uncompressed object is not the same as the original object!",
                compressable.getBytes().length, uncompressed.length);
            assertTrue("Uncompressed object is equal to the compressed object!",
                !uncompressed.equals(compressed));
            assertXMLEqual("uncompressed not same as original", compressable,
                new String(uncompressed));
        } catch (Exception e) {
            fail("Exception occurred while running tests: " + e.toString());
        }
    }

    /**
     * Test to make sure that bad data, non encoded XML data, being sent to the
     * decode method will cause an exception.
     */
    public void testDecodeBadData() {
        log.info("testDecodeBadData()");

        try {
            impl.decode("BadData".getBytes());
            fail("Expected an exception to be thrown from decode method.  "
                + "Data should NOT have been in an acceptable format!");
        } catch (Exception e) {
        }
    }

    /**
     * Reads a xml file into a String.
     * @param fileName String containing the name of the file to load.
     * @return String containing the data read in from the xml file.
     */
    private String getXmlInput(String fileName) {
        String xml = null;

        try {
            URL url = this.getClass().getResource(fileName);
            DataInputStream in = new DataInputStream(
                    new FileInputStream(new File(url.getFile())));
            BufferedReader reader = new BufferedReader(new InputStreamReader(in));

            try {
                StringBuffer sb = new StringBuffer();
                String str;

                while ((str = reader.readLine()) != null) {
                    sb = sb.append(str.trim());
                }

                xml = sb.toString();
            } finally {
                reader.close();
                in.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
            xml = "";
        }

        return xml;
    }
}
