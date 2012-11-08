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
 * @(#)CustomEncoderTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom;

import com.sun.encoder.tools.xml.SchemaLocationAttributes;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.StringWriter;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestResult;
import junit.framework.TestSuite;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;
import com.sun.encoder.util.UnicodeFile;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A simple test suite for unit testing UD1 encoder.
 *
 * @author Jun Xu
 * @since 6.0
 */
public class CustomEncoderTest extends TestSuite {

    //fields:
    //    test case type ("E" for encode or "D" for decode)
    //    test case name
    //    expected result ("T" for succeed and "F" for fail)
    //    schema file
    //    root element qualified name
    //    input file
    //    output file (optional)
    //    expected output file (optional)
    private static final int NUM_OF_FIELDS = 8;
    private static final int CASE_TYPE = 0;
    private static final int CASE_NAME = 1;
    private static final int EXPECTED_RESULT = 2;
    private static final int SCHEMA_FILE = 3;
    private static final int ROOT_ELEMENT = 4;
    private static final int INPUT_FILE = 5;
    private static final int OUTPUT_FILE = 6;
    private static final int EXPECTED_OUTPUT = 7;
    static Logger sLog = Logger.getLogger(CustomEncoderTest.class.getName());

    /** Creates a new instance of HL7EncoderTest
     * @param testName
     */
    public CustomEncoderTest(String testName) {
        super(testName);
    }

    /**
     * suite method automatically generated by JUnit module
     * @return
     */
    public static Test suite() {
        TestSuite suite =
                new CustomEncoderTest(
                    "UD1 Encoder Test Suite");
        LineNumberReader lnReader = null;
        try {
            lnReader =
                new LineNumberReader(
                    new BufferedReader(
                        new InputStreamReader(
                            new FileInputStream("test/testcases.txt"),
                                UnicodeFile.ENC)));
            String line;
            while ((line = lnReader.readLine()) != null) {
                line = line.trim();
                if (line.length() == 0 || line.charAt(0) == '#') {
                    continue;
                }
                StringTokenizer st = new StringTokenizer(line, ",");
                if (st.countTokens() != NUM_OF_FIELDS) {
                    throw new RuntimeException("Invalid test case entry. count="
                        + st.countTokens() + ", line=" + line);
                }
                String tokens[] = new String[NUM_OF_FIELDS];
                for (int i = 0; i < NUM_OF_FIELDS; i++) {
                    tokens[i] = st.nextToken();
                }
                boolean isPostiveTest = true;
                if ("F".equalsIgnoreCase(tokens[EXPECTED_RESULT].trim())) {
                    isPostiveTest = false;
                }
                QName rootElement;
                String rootElementName = tokens[ROOT_ELEMENT].trim();
                if (rootElementName.charAt(0) == '{') {
                    int pos = rootElementName.indexOf('}');
                    rootElement =
                        new QName(rootElementName.substring(1, pos),
                            rootElementName.substring(pos + 1));
                } else {
                    rootElement = new QName(rootElementName);
                }
                File outputFile = null;
                tokens[OUTPUT_FILE] = tokens[OUTPUT_FILE].trim();
                if (tokens[OUTPUT_FILE].length() > 0) {
                    outputFile = new File(tokens[OUTPUT_FILE]);
                }
                File expectedFile = null;
                tokens[EXPECTED_OUTPUT] = tokens[EXPECTED_OUTPUT].trim();
                if (tokens[EXPECTED_OUTPUT].length() > 0) {
                    expectedFile = new File(tokens[EXPECTED_OUTPUT]);
                }
                if ("D".equalsIgnoreCase(tokens[CASE_TYPE])) {
                    //"D"-Decode test case
                    suite.addTest(
                        new DecodeTest(
                            tokens[CASE_NAME].trim(),
                            isPostiveTest,
                            new File(tokens[SCHEMA_FILE].trim()),
                            rootElement,
                            new File(tokens[INPUT_FILE].trim()),
                            outputFile,
                            expectedFile) {
                                @Override
                                public void runTest() {
                                    testDecode();
                                }
                            });
                } else if ("E".equalsIgnoreCase(tokens[CASE_TYPE])) {
                    //"E"-Encode test case
                    suite.addTest(
                        new EncodeTest(
                            tokens[CASE_NAME].trim(),
                            isPostiveTest,
                            new File(tokens[SCHEMA_FILE].trim()),
                            rootElement,
                            new File(tokens[INPUT_FILE].trim()),
                            outputFile,
                            expectedFile) {
                                @Override
                                public void runTest() {
                                    testEncode();
                                }
                            });
                } else {
                    //"P"-Passthru test case
                    suite.addTest(
                        new PassThruTest(
                            tokens[CASE_NAME].trim(),
                            isPostiveTest,
                            new File(tokens[SCHEMA_FILE].trim()),
                            rootElement,
                            new File(tokens[INPUT_FILE].trim()),
                            outputFile,
                            expectedFile) {
                            @Override
                            public void runTest() {
                                testPassThru();
                            }
                        });
                }
            }
            return suite;
        } catch (IOException e) {
            throw new RuntimeException("Failed to load test cases.", e);
        } finally {
            if (lnReader != null) {
                try {
                    lnReader.close();
                } catch (Exception e) {
                    //Ignore
                }
            }
        }
    }

    public static String[] getExpectedXMLContent(File expectedFile,
            String schemaLocation, String testName)
            throws IOException {
        String[] expectedContent = null;
        if (expectedFile != null) {
            expectedContent = UnicodeFile.getTexts(expectedFile.getPath());
            if (expectedContent.length > 1 && sLog.isLoggable(Level.INFO)) {
                sLog.info("There are " + expectedContent.length
                        + " expected files for decode: " + testName);
            }
            if (schemaLocation != null) {
                // replace ${SCHEMA_LOCATION} in expected contents
                // with actual mSchemaLocation.
                for (int i = 0; i < expectedContent.length; i++) {
                    expectedContent[i] = expectedContent[i].replaceAll(
                            "\\$\\{SCHEMA_LOCATION\\}", schemaLocation);
                }
            }
        }
        // remove "/bld" in expected contents if present
        if (expectedContent != null) {
            for (int i = 0; i < expectedContent.length; i++) {
                if (expectedContent[i] != null) {
                    expectedContent[i] = expectedContent[i].replaceAll("/bld/", "/").trim();
                }
            }
        }
        return expectedContent;
    }

    /**
     * Decode test.
     */
    public static class DecodeTest extends TestCase {

        private final boolean mIsPositiveTest;
        private final File mSchemaFile;
        private final QName mRootElement;
        private final File mInputFile;
        private final File mOutputFile;
        private final File mExpectedFile;

        private String mSchemaLocation = null;

        public DecodeTest(String testName, boolean isPositiveTest,
                File schemaFile, QName rootElement, File inputFile,
                File outputFile, File expectedFile) {
            super(testName);
            mIsPositiveTest = isPositiveTest;
            mSchemaFile = schemaFile;
            mRootElement = rootElement;
            mInputFile = inputFile;
            mOutputFile = outputFile;
            mExpectedFile = expectedFile;
        }

        @Override
        protected void setUp()
            throws Exception {
            SchemaLocationAttributes schemaLocationAttr =
                new SchemaLocationAttributes(mRootElement.getNamespaceURI(),
                    mSchemaFile.toURL());
            if (schemaLocationAttr.getLength() > 0) {
                mSchemaLocation = schemaLocationAttr.getValue(0);
            }
        }

        @Override
        protected void tearDown() throws Exception {
        }

        public void testDecode() {

            sLog.info("testDecode --> [" + getName() + "]");
            EncoderFactory factory;
            // expected content
            String[] expected = null;
            try {
                expected = getExpectedXMLContent(mExpectedFile,
                        mSchemaLocation, getName());

                factory = EncoderFactory.newInstance();
                EncoderType type = factory.makeType(CustomEncoderProvider.STYLE_ID);
                MetaRef metaRef =
                        factory.makeMeta(mSchemaFile.getAbsolutePath(),
                            mRootElement);
                Encoder encoder = factory.newEncoder(type, metaRef);
                // default to decode from bytes
                Source source = encoder.decodeFromBytes(loadBytes(mInputFile));
//                Source source = encoder.decodeFromString(UnicodeFile.getText(mInputFile));
                StringWriter swResult = new StringWriter();
                StreamResult streamResult = new StreamResult(swResult);
                Transformer transformer =
                    TransformerFactory.newInstance().newTransformer();
                //transformer.setOutputProperty(OutputKeys.METHOD, "xml");
                //transformer.setOutputProperty("{http://xml.apache.org/xalan}line-separator"," ");
                transformer.transform(source, streamResult);
                String resultGot = swResult.toString();
                if (mOutputFile != null) {
                    UnicodeFile.setText(mOutputFile, resultGot);
                }
                // remove "/bld" in result if present
                if (resultGot != null) {
                    resultGot = resultGot.replaceAll("/bld/", "/").trim();
                }
                
                boolean matchFound = false;
                for (int i = 0; i < expected.length; i++) {
                    if (expected[i] != null && expected[i].equals(resultGot)) {
                        matchFound = true;
                        break;
                    }
                }
                if (mIsPositiveTest) {
                    if (!matchFound) {
                        for (int i = 0; i < expected.length; i++) {
                            assertEquals("Check result with expected #" + i
                                + ": " + getName(), expected[i], resultGot);
                        }
                    }
                } else {
                    if (matchFound) {
                        fail("Expecting difference but got same: "
                                + getName());
                    }
                }
            } catch (EncoderConfigurationException e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (FileNotFoundException e) {
                fail(getName() + " - " + e.toString());
            } catch (IOException e) {
                fail(getName() + " - " + e.toString());
            } catch (TransformerConfigurationException e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (TransformerFactoryConfigurationError e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (EncoderException e) {
                if (mIsPositiveTest) {
                    fail(getName() + " - " + e.toString());
                } else {
                    // handle negative test with expected exception message
                    if (expected != null && expected.length > 0 && expected[0] != null) {
                        assertEquals("Check " + e.getClass().getName()
                                + " with expected error message: "
                                + getName(), expected[0], e.getMessage());
                    }
                    return;
                }
            } catch (TransformerException e) {
                if (mIsPositiveTest) {
                    fail(getName() + " - " + e.toString());
                } else {
                    // handle negative test with expected exception message
                    if (expected != null && expected.length > 0 && expected[0] != null) {
                        assertEquals("Check " + e.getClass().getName()
                                + " with expected error message: "
                                + getName(), expected[0], e.getMessage());
                    }
                    return;
                }
            }
            if (!mIsPositiveTest) {
                fail("Expecting failure but no error occurred: "
                        + getName());
            }
        }
    }

    /**
     * Encode test.
     */
    public static class EncodeTest extends TestCase {

        private final boolean mIsPositiveTest;
        private final File mSchemaFile;
        private final QName mRootElement;
        private final File mInputFile;
        private final File mOutputFile;
        private final File mExpectedFile;

        public EncodeTest(String testName, boolean isPositiveTest,
                File schemaFile, QName rootElement, File inputFile,
                File outputFile, File expectedFile) {
            super(testName);
            mIsPositiveTest = isPositiveTest;
            mSchemaFile = schemaFile;
            mRootElement = rootElement;
            mInputFile = inputFile;
            mOutputFile = outputFile;
            mExpectedFile = expectedFile;
        }

        @Override
        protected void setUp() throws Exception {
        }

        @Override
        protected void tearDown() throws Exception {
        }

        public void testEncode() {

            sLog.info("testEncode --> [" + getName() + "]");
            EncoderFactory factory;
            byte[] expected = null;
            try {
                factory = EncoderFactory.newInstance();
                EncoderType type = factory.makeType(CustomEncoderProvider.STYLE_ID);
                MetaRef metaRef = factory.makeMeta(
                    mSchemaFile.getAbsolutePath(), mRootElement);
                Encoder encoder = factory.newEncoder(type, metaRef);
                StreamSource streamSource = new StreamSource(mInputFile);
                byte[] resultGot = encoder.encodeToBytes(streamSource);
                //String resultGot = encoder.encodeToString(streamSource);
                if (mOutputFile != null) {
                    writeBytes(resultGot, mOutputFile);
                    //UnicodeFile.setText(mOutputFile, resultGot);
                }
                if (mExpectedFile != null) {
                    expected = loadBytes(mExpectedFile);
                    //String expected = UnicodeFile.getText(mExpectedFile);
                    if (mIsPositiveTest) {
                        //assertEquals("Check result with expected: " + getName(), expected, resultGot);
                        assertByteArrayEquals("Check result with expected: "
                            + getName(), expected, resultGot);
                    } else {
                        if (checkByteArrayEquals(expected, resultGot)) {
                        //if (expected.equals(resultGot)) {
                            fail("Expecting difference but got same: "
                                    + getName());
                        }
                    }
                }
            } catch (EncoderConfigurationException e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (FileNotFoundException e) {
                fail(getName() + " - " + e.toString());
            } catch (IOException e) {
                fail(getName() + " - " + e.toString());
            } catch (TransformerFactoryConfigurationError e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (EncoderException e) {
                if (mIsPositiveTest) {
                    fail(getName() + " - " + e.toString());
                } else {
                    if (expected != null) {
                        assertEquals("Check " + e.getClass().getName()
                                + " with expected error message: "
                                + getName(), expected, e.getMessage());
                    }
                    return;
                }
            }
            if (!mIsPositiveTest) {
                fail("Expecting failure but no error occurred: "
                        + getName());
            }
        }

        void assertByteArrayEquals(String msg, byte[] arr1, byte[] arr2) {
            if (arr1.length != arr2.length) {
                fail(msg + ": byte array length not equals. length1="
                    + arr1.length + ", length2=" + arr2.length);
            }
            for (int i = 0; i < arr1.length; i++) {
                assertEquals(msg, arr1[i], arr2[i]);
            }
        }

        boolean checkByteArrayEquals(byte[] arr1, byte[] arr2) {
            if (arr1.length != arr2.length) {
                return false;
            }
            for (int i = 0; i < arr1.length; i++) {
                if (arr1[i] != arr2[i]) {
                    return false;
                }
            }
            return true;
        }
    }

    public static class PassThruTest extends TestCase {

        private final boolean mIsPositiveTest;
        private final File mSchemaFile;
        private final QName mRootElement;
        private final File mInputFile;
        private final File mOutputFile;
        private final File mExpectedFile;

        /**
         *
         * @param testName
         * @param isPositiveTest
         * @param schemaFile
         * @param rootElement
         * @param inputFile
         * @param outputFile
         * @param expectedFile
         */
        public PassThruTest(String testName, boolean isPositiveTest,
                File schemaFile, QName rootElement, File inputFile,
                File outputFile, File expectedFile) {
            super(testName);
            mIsPositiveTest = isPositiveTest;
            mSchemaFile = schemaFile;
            mRootElement = rootElement;
            mInputFile = inputFile;
            mOutputFile = outputFile;
            mExpectedFile = expectedFile;
        }

        @Override
        protected void setUp() throws Exception {
            super.setUp();
        }

        @Override
        protected void tearDown() throws Exception {
            super.tearDown();
        }

        /**
         *
         */
        public void testPassThru() {

            sLog.info("testPassThru --> [" + getName() + "]");
            String expected = null;
            try {
                String inputString = UnicodeFile.getText(mInputFile);
                // expected defaults to input string
                expected = inputString;
                if (mExpectedFile != null) {
                    expected = UnicodeFile.getText(mExpectedFile);
                }

                EncoderFactory encoderFactory = EncoderFactory.newInstance();
                EncoderType encoderType = encoderFactory.makeType(CustomEncoderProvider.STYLE_ID);
                MetaRef metaRef = encoderFactory.makeMeta(
                    mSchemaFile.getAbsolutePath(), mRootElement);
                Encoder encoder = encoderFactory.newEncoder(encoderType, metaRef);
                // decode
                Source source = encoder.decodeFromString(inputString);
                /*
                StringWriter stringWriter = new StringWriter();
                StreamResult streamResult = new StreamResult(stringWriter);
                Transformer transformer =
                    TransformerFactory.newInstance().newTransformer();
                transformer.transform(source, streamResult);
                */
                // encode
                String resultGot = encoder.encodeToString(source);
                if (mOutputFile != null) {
                    UnicodeFile.setText(mOutputFile, resultGot);
                }
                if (expected != null) {
                    if (mIsPositiveTest) {
                        assertEquals("Check result with expected: "
                            + getName(), expected, resultGot);
                    } else {
                        if (expected.equals(resultGot)) {
                            fail("Expecting difference but got same: "
                                + getName());
                        }
                    }
                }
            } catch (EncoderConfigurationException e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (FileNotFoundException e) {
                fail(getName() + " - " + e.toString());
            } catch (IOException e) {
                fail(getName() + " - " + e.toString());
            } catch (EncoderException e) {
                if (mIsPositiveTest) {
                    fail(getName() + " - " + e.toString());
                } else {
                    // handle negative test with expected exception message
                    if (expected != null) {
                        assertEquals("Check " + e.getClass().getName()
                                + " with expected error message: "
                                + getName(), expected, e.getMessage());
                    }
                    return;
                }
            }

        }
    }

    /**
     * Loads a byte array from a file.
     * @param binFile a binary File object
     * @return a byte array loaded from a file.
     * @throws java.io.IOException -
     */
    public static byte[] loadBytes(File binFile)
        throws IOException {
        byte[] bytes = new byte[(int) binFile.length()];
        InputStream is = null;
        try {
            is = new FileInputStream(binFile);
            is.read(bytes);
        } finally {
            if (is != null) {
                is.close();
            }
        }
        return bytes;
    }

    /**
     * Writes a byte array to a file.
     * @param bytes a byte array to be written to a file.
     * @param binFile a target binary file.
     * @throws java.io.IOException -
     */
    public static void writeBytes(byte[] bytes, File binFile)
        throws IOException {
        OutputStream os = null;
        try {
            os = new FileOutputStream(binFile);
            os.write(bytes);
        } finally {
            if (os != null) {
                os.close();
            }
        }
    }

    public static void main(String arg[]) {
        CustomEncoderTest.suite().run(new TestResult());
    }
}