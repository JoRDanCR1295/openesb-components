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
 * @(#)Ssc2CustomTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.converter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.StringWriter;
import java.util.HashSet;
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

import org.apache.xmlbeans.XmlException;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.SchemaLocationAttributes;
import com.sun.encoder.util.UnicodeFile;

import java.util.logging.Level;
import java.util.logging.Logger;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestResult;
import junit.framework.TestSuite;

/**
 * A simple test suite to test SSC to Custom Encoder Converter.
 *
 * @author Jun Xu
 * @since 6.0
 */
public class Ssc2CustomTest extends TestSuite {

    //fields:
    //    test case type ("E" for encode or "D" for decode)
    //    test case name
    //    expected result ("T" for succeed and "F" for fail)
    //    ssc file
    //    schema file
    //    root element qualified name
    //    input file
    //    output file (optional)
    //    expected output file (optional)
    private static final int NUM_OF_FIELDS = 9;
    private static final int CASE_TYPE = 0;
    private static final int CASE_NAME = 1;
    private static final int EXPECTED_RESULT = 2;
    private static final int SSC_FILE = 3;
    private static final int SCHEMA_FILE = 4;
    private static final int ROOT_ELEMENT = 5;
    private static final int INPUT_FILE = 6;
    private static final int OUTPUT_FILE = 7;
    private static final int EXPECTED_OUTPUT = 8;
    static final String ENC_STYLE = "customencoder-1.0";
    static Logger sLog = Logger.getLogger(Ssc2CustomTest.class.getName());

    /**
     * Creates a new instance of Ssc2CustomTest.
     * @param testName
     */
    public Ssc2CustomTest(String testName) {
        super(testName);
    }

    /**
     * suite method automatically generated by JUnit module
     * @return Test
     */
    public static Test suite() {
        TestSuite suite =
            new Ssc2CustomTest(
                "SSC to Custom Encoder Converter Test Suite");
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
                    throw new RuntimeException(
                        "Invalid test case entry. count=" + st.countTokens()
                            + ", line=" + line);
                }
                String tokens[] = new String[NUM_OF_FIELDS];
                for (int i = 0; i < NUM_OF_FIELDS; i++) {
                    tokens[i] = st.nextToken();
                }
                boolean isPositiveTest = true;
                if ("F".equalsIgnoreCase(tokens[EXPECTED_RESULT].trim())) {
                    isPositiveTest = false;
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
                    //decode test case
                    suite.addTest(
                        new DecodeTest(
                            tokens[CASE_NAME].trim(),
                            isPositiveTest,
                            new File(tokens[SSC_FILE].trim()),
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
                } else {
                    //encode test case
                    suite.addTest(
                        new EncodeTest(
                            tokens[CASE_NAME].trim(),
                            isPositiveTest,
                            new File(tokens[SSC_FILE].trim()),
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
        // remove "/bld" in expected content if present
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
        private final File mSscFile;
        private final File mSchemaFile;
        private final QName mRootElement;
        private final File mInputFile;
        private final File mOutputFile;
        private final File mExpectedFile;

        private String mSchemaLocation = null;

        public DecodeTest(String testName, boolean isPositiveTest,
                File sscFile, File schemaFile, QName rootElement,
                File inputFile, File outputFile, File expectedFile) {
            super(testName);
            mIsPositiveTest = isPositiveTest;
            mSscFile = sscFile;
            mSchemaFile = schemaFile;
            mRootElement = rootElement;
            mInputFile = inputFile;
            mOutputFile = outputFile;
            mExpectedFile = expectedFile;
        }

        @Override
        protected void setUp() throws Exception {
            SchemaLocationAttributes schemaLocationAttr =
                new SchemaLocationAttributes(
                    mRootElement.getNamespaceURI(), mSchemaFile.toURL());
            if (schemaLocationAttr.getLength() > 0) {
                mSchemaLocation = schemaLocationAttr.getValue(0);
            }
        }

        @Override
        protected void tearDown() throws Exception {
        }

        public void testDecode() {

            EncoderFactory factory;
            String[] expected = null;
            try {
                expected = getExpectedXMLContent(mExpectedFile,
                        mSchemaLocation, getName());

                Ssc2Custom.convert(
                    mSscFile.getParentFile(),
                    mSchemaFile.getParentFile(),
                    new String[]{"templates"},
                    new String[]{mSscFile.getName()},
                    mRootElement.getNamespaceURI(),
                    false,
                    new StringBuffer(),
                    new HashSet<String>(),
                    null);
                factory = EncoderFactory.newInstance();
                EncoderType type = factory.makeType(ENC_STYLE);
                MetaRef metaRef = factory.makeMeta(
                    mSchemaFile.getAbsolutePath(), mRootElement);
                Encoder encoder = factory.newEncoder(type, metaRef);
                Source source = encoder.decodeFromString(
                    UnicodeFile.getText(mInputFile));
                StringWriter swResult = new StringWriter();
                StreamResult streamResult = new StreamResult(swResult);
                Transformer transformer =
                    TransformerFactory.newInstance().newTransformer();
                transformer.transform(source, streamResult);
                String resultGot = swResult.toString();
                if (mOutputFile != null) {
                    UnicodeFile.setText(mOutputFile, resultGot);
                }
                resultGot = resultGot.replaceAll("/bld/", "/").trim();
                boolean matchFound = false;
                for (int i = 0; i < expected.length; i++) {
                    if (expected[i].equals(resultGot)) {
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
            } catch (XmlException e) {
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
        private final File mSscFile;
        private final File mSchemaFile;
        private final QName mRootElement;
        private final File mInputFile;
        private final File mOutputFile;
        private final File mExpectedFile;

        public EncodeTest(String testName, boolean isPositiveTest,
                File sscFile, File schemaFile, QName rootElement,
                File inputFile, File outputFile, File expectedFile) {
            super(testName);
            mIsPositiveTest = isPositiveTest;
            mSscFile = sscFile;
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

        /**
         * Test encode operation.
         */
        public void testEncode() {

            EncoderFactory factory;
            try {
                Ssc2Custom.convert(
                    mSscFile.getParentFile(),
                    mSchemaFile.getParentFile(),
                    new String[]{"templates"},
                    new String[]{mSscFile.getName()},
                    mRootElement.getNamespaceURI(),
                    false,
                    new StringBuffer(),
                    new HashSet<String>(),
                    null);
                factory = EncoderFactory.newInstance();
                EncoderType type = factory.makeType(ENC_STYLE);
                MetaRef metaRef =
                    factory.makeMeta(mSchemaFile.getAbsolutePath(),
                        mRootElement);
                Encoder encoder = factory.newEncoder(type, metaRef);
                StreamSource streamSource = new StreamSource(mInputFile);
                String resultGot = encoder.encodeToString(streamSource);
                if (mOutputFile != null) {
                    UnicodeFile.setText(mOutputFile, resultGot);
                }
                if (mExpectedFile != null) {
                    String expected = UnicodeFile.getText(mExpectedFile);
                    if (mIsPositiveTest) {
                        assertEquals("Check result with expecte: "
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
            } catch (EncoderException e) {
                if (mIsPositiveTest) {
                    System.err.print(getName() + " - ");
                    e.printStackTrace();
                    fail(getName() + " - " + e.toString());
                } else {
                    return;
                }
            } catch (IOException e) {
                fail(getName() + " - " + e.toString());
            } catch (TransformerFactoryConfigurationError e) {
                //fatal error
                throw new RuntimeException(e);
            } catch (XmlException e) {
                //fatal error
                throw new RuntimeException(e);
            }
            if (!mIsPositiveTest) {
                fail("Expecting failure but no error occurred: "
                        + getName());
            }
        }
    }

    public static void main(String arg[]) {
        Ssc2CustomTest.suite().run(new TestResult());
    }
}