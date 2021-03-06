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
 * @(#)UnitTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.util.StringTokenizer;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xmlbeans.XmlException;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.sun.xml.transform.sware.SwareDOMImplementation;
import com.sun.xml.transform.sware.schema.SwareSchema;

import junit.framework.*;

/**
 * JUnit test suite for the schema aware DOM reordering transformer.
 *
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class UnitTest extends TestSuite {
    
    public UnitTest(String testName) {
        super(testName);
    }

    /**
     * suite method automatically generated by JUnit module
     */
    public static Test suite() {
        TestSuite suite =
                new UnitTest(
                "Schema Aware DOM Reordering Transformer Test Suite");
        LineNumberReader lnReader = null;
        try {
            lnReader =
                    new LineNumberReader(
                    new BufferedReader(
                    new InputStreamReader(
                    new FileInputStream("test/data/testcases.txt"), "UTF-8")));
            String line;
            while ((line = lnReader.readLine()) != null) {
                line = line.trim();
                if (line.length() == 0 || line.charAt(0) == '#') {
                    continue;
                }
                StringTokenizer st = new StringTokenizer(line, ",");
                if (st.countTokens() != 5) {
                    throw new RuntimeException(
                            "Invalid test case entry: " + line);
                }
                String tokens[] = new String[5];
                for (int i = 0; i < 5; i++) {
                    tokens[i] = st.nextToken();
                }
                if ("N".equals(tokens[0])) {
                    //Negative test
                    suite.addTest(new NTestCastor(tokens[1], new File(tokens[2]),
                            new File(tokens[3]), new File(tokens[4])) {
                        public void runTest() {
                            testSorting();
                        }
                    });
                    suite.addTest(new NTestXmlBeans(tokens[1], new File(tokens[2]),
                            new File(tokens[3]), new File(tokens[4])) {
                        public void runTest() {
                            testSorting();
                        }
                    });
                } else {
                    //Positive test
                    suite.addTest(new PTestCastor(tokens[1], new File(tokens[2]),
                            new File(tokens[3]), new File(tokens[4])) {
                        public void runTest() {
                            testSorting();
                        }
                    });
                    suite.addTest(new PTestXmlBeans(tokens[1], new File(tokens[2]),
                            new File(tokens[3]), new File(tokens[4])) {
                        public void runTest() {
                            testSorting();
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

    /**
     * Positive test.
     */
    public static abstract class PTest extends TestCase {

        private final File mSchemaFile;
        private final File mInputFile;
        private final File mOutputFile;
        
        public PTest(String testName, File schemaFile,
                File inputFile, File outputFile) {
            super(testName);
            mSchemaFile = schemaFile;
            mInputFile = inputFile;
            mOutputFile = outputFile;
        }

        protected void setUp() throws Exception {
        }

        protected void tearDown() throws Exception {
        }
        
        protected abstract void doWork(Unit unit)
            throws TransformerException, SAXException, Exception;

        public void testSorting() {

            Unit testUnit =
                new Unit(mSchemaFile, mInputFile, mOutputFile);
            try {
                doWork(testUnit);
            } catch (TransformerException e) {
                assertTrue(e.toString()
                        + ", input file: "
                        + mInputFile.toString(),
                            false);
            } catch (SAXException e) {
                assertTrue(e.toString()
                        + "input file: "
                        + mInputFile.toString(),
                            false);
            } catch (Exception e) {
                throw new RuntimeException(
                        "Unexpected failure.", e);
            }
            
            /* Test multiple threads scenario
            final SwareSchema swareSchema;
            try {
                swareSchema = Unit.loadSchema(mSchemaFile);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            final SwareDOMImplementation swareDOMImpl;
            try {
                swareDOMImpl = Unit.getSwareDOMImplementation();
            } catch (ParserConfigurationException e) {
                throw new RuntimeException(e);
            }

            for (int i = 0; i < 50; i++) {
                new Thread(String.valueOf(i)) {
                    public void run() {
                        Unit testUnit =
                            new Unit(
                                    mSchemaFile,
                                    mInputFile,
                                    new File(mOutputFile.toString()
                                            + this.getName()),
                                    swareDOMImpl,
                                    swareSchema);
                        try {
                            testUnit.testSorting();
                        } catch (TransformerException e) {
                            assertTrue(e.toString()
                                    + ", input file: "
                                    + mInputFile.toString(),
                                        false);
                        } catch (SAXException e) {
                            assertTrue(e.toString()
                                    + "input file: "
                                    + mInputFile.toString(),
                                        false);
                        } catch (Exception e) {
                            throw new RuntimeException(
                                    "Unexpected failure.", e);
                        }
                    }
                }.start();
            }
            */
        }
    }

    /**
     * Negative test.
     */
    public static abstract class NTest extends TestCase {

        private final File mSchemaFile;
        private final File mInputFile;
        private final File mOutputFile;
        
        public NTest(String testName, File schemaFile,
                File inputFile, File outputFile) {
            super(testName);
            mSchemaFile = schemaFile;
            mInputFile = inputFile;
            mOutputFile = outputFile;
        }

        protected void setUp() throws Exception {
        }

        protected void tearDown() throws Exception {
        }

        protected abstract void doWork(Unit unit)
            throws TransformerException, SAXParseException, Exception;
        
        public void testSorting() {
            
            Unit testUnit = new Unit(mSchemaFile, mInputFile, mOutputFile);
            try {
                doWork(testUnit);
            } catch (TransformerException e) {
                //This is good since it is negative test
                return;
            } catch (SAXParseException e) {
                //This is good since it is negative test
                return;
            } catch (Exception e) {
                throw new RuntimeException("Unexpected failure.", e);
            }
            //This is bad since it is negative test
            assertTrue("Expecting test case to fail but it didn't.", false);
            
            /* Test multiple threads scenario
            final SwareSchema swareSchema;
            try {
                swareSchema = Unit.loadSchema(mSchemaFile);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            final SwareDOMImplementation swareDOMImpl;
            try {
                swareDOMImpl = Unit.getSwareDOMImplementation();
            } catch (ParserConfigurationException e) {
                throw new RuntimeException(e);
            }

            for (int i = 0; i < 50; i++) {
                new Thread(String.valueOf(i)) {
                    public void run() {
                        Unit testUnit =
                            new Unit(
                                    mSchemaFile,
                                    mInputFile,
                                    new File(mOutputFile.toString()
                                            + this.getName()),
                                    swareDOMImpl,
                                    swareSchema);
                        try {
                            testUnit.testSorting();
                        } catch (TransformerException e) {
                            //This is good since it is negative test
                            return;
                        } catch (SAXParseException e) {
                            //This is good since it is negative test
                            return;
                        } catch (Exception e) {
                            throw new RuntimeException(
                                    "Unexpected failure.", e);
                        }
                        //This is bad since it is negative test
                        assertTrue(
                                "Expecting test case to fail but it didn't.", false);
                    }
                }.start();
            }
            */
        }
    }
    
    public static class PTestCastor extends PTest {

        public PTestCastor(String testName, File schemaFile, File inputFile,
                File outputFile) {
            super(testName, schemaFile, inputFile, outputFile);
        }

        @Override
        protected void doWork(Unit unit) throws TransformerException, SAXException,
                Exception {
            unit.testSortingCastor();
        }
    }
    
    public static class NTestCastor extends NTest {

        public NTestCastor(String testName, File schemaFile, File inputFile,
                File outputFile) {
            super(testName, schemaFile, inputFile, outputFile);
        }

        @Override
        protected void doWork(Unit unit) throws TransformerException,
                SAXParseException, Exception {
            unit.testSortingCastor();
        }
    }
    
    public static class PTestXmlBeans extends PTest {

        public PTestXmlBeans(String testName, File schemaFile, File inputFile,
                File outputFile) {
            super(testName, schemaFile, inputFile, outputFile);
        }

        @Override
        protected void doWork(Unit unit) throws TransformerException, SAXException,
                Exception {
            unit.testSortingXmlBeans();
        }
    }
    
    public static class NTestXmlBeans extends NTest {

        public NTestXmlBeans(String testName, File schemaFile, File inputFile,
                File outputFile) {
            super(testName, schemaFile, inputFile, outputFile);
        }

        @Override
        protected void doWork(Unit unit) throws TransformerException,
                SAXParseException, Exception {
            unit.testSortingXmlBeans();
        }
    }
    
    public static void main(String arg[]) {
        TestResult result = new TestResult();
        UnitTest.suite().run(result);
        System.out.println("total: " + result.runCount());
        System.out.println("error: " + result.errorCount());
        System.out.println("failure: " + result.failureCount());
    }
}
