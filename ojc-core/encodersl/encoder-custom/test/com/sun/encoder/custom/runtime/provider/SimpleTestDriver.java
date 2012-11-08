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
 * @(#)SimpleTestDriver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime.provider;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamSource;

import org.apache.xmlbeans.XmlException;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;
import com.sun.encoder.custom.CustomEncoderProvider;
import com.sun.encoder.runtime.provider.SimpleContentHandler;
import com.sun.encoder.util.UnicodeFile;

/**
 * A test driver for doing simple benchmark test.
 *  
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public class SimpleTestDriver {
    
    //prepare data for benchmark
    private static final String mStringData;
    private static final byte[] mBytesData;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append("segment1|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10.1&field10.2&field10.3&field10.4&field10.5\n");
        sb.append("segment2|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10.1&field10.2&field10.3&field10.4&field10.5\n");
        sb.append("segment3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10.1&field10.2&field10.3&field10.4&field10.5\n");
        sb.append("segment4|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10\n");
        sb.append("segment5|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10\n");
        sb.append("segment6|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|sub1~sub2~sub3^sub1~sub2~sub3^sub1~sub2~sub3|field4|field5|field6|field7|field8|field9|field10\n");
        sb.append("segment7component1component20123456789component301234567890123456789component401234567890123456789component50123456789component1component20123456789component301234567890123456789component401234567890123456789component50123456789field301230123456789\n");
        sb.append("segment8component1component20123456789component301234567890123456789component401234567890123456789component50123456789component1component20123456789component301234567890123456789component401234567890123456789component50123456789field301230123456789\n");
        sb.append("segment9component1component20123456789component301234567890123456789component401234567890123456789component50123456789component1component20123456789component301234567890123456789component401234567890123456789component50123456789field301230123456789\n");
        mStringData = sb.toString();
        try {
            mBytesData = mStringData.getBytes("UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }
    
    private final Encoder mEncoder;
    
    public SimpleTestDriver(Encoder encoder) {
        mEncoder = encoder;
    }
    
    public void testDecodeFromString(String stringData, int loops, boolean doPrint)
            throws XmlException, IOException, SAXException, EncoderException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        final int finalLoops = loops;
        int i;
        SAXSource result;
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            result = (SAXSource) mEncoder.decodeFromString(mStringData);
            result.getXMLReader().setContentHandler(handler);
            result.getXMLReader().parse(result.getInputSource());
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }
    
    public void testDecodeFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws XmlException, IOException, SAXException, EncoderException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        final int finalLoops = loops;
        int i;
        SAXSource result;
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            result = (SAXSource) mEncoder.decodeFromBytes(mBytesData);
            result.getXMLReader().setContentHandler(handler);
            result.getXMLReader().parse(result.getInputSource());
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    public void testEncodeToString(String stringData,
            int loops, boolean doPrint)
        throws XmlException, IOException, TransformerFactoryConfigurationError,
        TransformerException, EncoderException {
        System.out.println("source data size = " + stringData.length());
        
        System.out.println("Prepare DOM Source ...");
        DOMSource domSource = getDOMData(stringData);
        
        final int finalLoops = loops;
        int i;
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        SAXResult saxResult;
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            mEncoder.encodeToString(domSource);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        //Either visiting DOM tree or transforming is very costy, we need to
        //exclude the cost.  We use the SimpleContentHandler to run it this
        //time.  SimpleContentHandler almost does nothing when doPrint is false.
        saxResult = new SAXResult(
                new SimpleContentHandler(false));
        System.out.println("Estimate cost ...");
        startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            transformer.transform(domSource, saxResult);
        }
        endedAt = System.currentTimeMillis();
        long elapsed2 = endedAt - startedAt;
        if (elapsed2 < 0) {
            elapsed2 = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        elapsed = elapsed - elapsed2;
        String resultString = mEncoder.encodeToString(domSource);
        if (doPrint) {
            System.out.println(resultString);
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }

        System.out.println("result data size (chars) = "
                + resultString.length());
        System.out.println("Characters/ms: "
                + i * resultString.length() / elapsed);
    }
    
    public void testEncodeToBytes(byte[] bytes, int loops, boolean doPrint)
        throws XmlException, IOException, TransformerFactoryConfigurationError,
        TransformerException, EncoderException {
        System.out.println("source data size (bytes) = " + bytes.length);
        
        System.out.println("Prepare DOM Source ...");
        DOMSource domSource = getDOMData(bytes);
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        SAXResult saxResult;
        for (i = 0; i < finalLoops; i++) {
            mEncoder.encodeToBytes(domSource);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        //Either visiting DOM tree or transforming is very costy, we need to
        //exclude the cost.  We use the SimpleContentHandler to run it this
        //time.  SimpleContentHandler almost does nothing when doPrint is false.
        saxResult = new SAXResult(
                new SimpleContentHandler(false));
        System.out.println("Estimate cost ...");
        startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            transformer.transform(domSource, saxResult);
        }
        endedAt = System.currentTimeMillis();
        long elapsed2 = endedAt - startedAt;
        if (elapsed2 < 0) {
            elapsed2 = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        elapsed = elapsed - elapsed2;
        byte[] resultBytes = mEncoder.encodeToBytes(domSource);
        if (doPrint) {
            System.out.println(new String(resultBytes, "UTF-8"));
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("result data size (bytes) = " + resultBytes.length);
        System.out.println("result bytes/ms: "
                + i * resultBytes.length / elapsed);
    }
    
    private DOMSource getDOMData(String stringData)
            throws TransformerFactoryConfigurationError, TransformerException {
        StreamSource streamSource = new StreamSource(
                new StringReader(stringData));
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        DOMResult domResult = new DOMResult();
        transformer.transform(streamSource, domResult);
        return new DOMSource(domResult.getNode());
    }
    
    private DOMSource getDOMData(byte[] bytesData)
            throws TransformerFactoryConfigurationError, TransformerException {
        StreamSource streamSource = new StreamSource(
                new ByteArrayInputStream(bytesData));
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        DOMResult domResult = new DOMResult();
        transformer.transform(streamSource, domResult);
        return new DOMSource(domResult.getNode());
    }
    
    public static void main(String[] args) {
        
        String testWhat = "ds";
        int loops = 1;
        boolean printResult = false;
        File testDir =
            new File(System.getProperty("ENCODER_SHAREDLIBRARY") + "custom/test");
        String inputFileName = null;
        String schemaFileName = null;
        String schemaURLString = null;
        String rootElementName = null;
        for (int i = 0; args != null && i < args.length; i++) {
            if ("-t".equals(args[i])) {
                //test what?
                if (i + 1 < args.length) {
                    testWhat = args[i + 1];
                }
            } else if ("-d".equals(args[i])) {
                //testing directory
                if (i + 1 < args.length) {
                    testDir = new File(args[i + 1]);
                }
            } else if ("-l".equals(args[i])) {
                //number of loops
                if (i + 1 < args.length) {
                    loops = Integer.parseInt(args[i + 1]);
                }
            } else if ("-p".equals(args[i])) {
                //print result?
                printResult = true; 
            } else if ("-i".equals(args[i])) {
                //input file
                if (i + 1 < args.length) {
                    inputFileName = args[i + 1];
                }
            } else if ("-s".equals(args[i])) {
                //schema file using file path
                if (i + 1 < args.length) {
                    schemaFileName = args[i + 1];
                }
            } else if ("-u".equals(args[i])) {
                //schema file using URL
                if (i + 1 < args.length) {
                    schemaURLString = args[i + 1];
                }
            } else if ("-r".equals(args[i])) {
                //root element
                if (i + 1 < args.length) {
                    rootElementName = args[i + 1];
                }
            } else if ("-help".equals(args[i]) || "/?".equals(args[i])) {
                System.out.println("Usage: java SimplePerfTest "
                        + "[-d <testing dir>] "
                        + "[-t <test what, value=(ls, lb, ds, db, es or eb)] "
                        + "[-l <number of loops>] "
                        + "[-p]");
                System.exit(1);
            }
        }
        
        try {
            System.out.println("Load input data ...");
            String stringData = mStringData;
            byte[] bytesData = mBytesData;
            if (inputFileName != null) {
                stringData = UnicodeFile.getText(
                        new File(testDir, inputFileName));
                bytesData = stringData.getBytes("UTF-8");
            }
            
            System.out.println("Prepare XSD ...");
            File schemaFile = null;
            URL schemaURL = null;
            if (schemaFileName == null && schemaURLString == null) {
                schemaFile = new File(testDir, "data/benchmark.xsd");
            } else if (schemaFileName != null) {
                schemaFile = new File(testDir, schemaFileName);
            } else {
                schemaURL = new URL(schemaURLString);
            }
            QName qName;
            if (rootElementName == null) {
                qName = new QName("http://xml.netbeans.org/schema/CustomEncoderBenchmark", "root");
            } else {
                if (rootElementName.charAt(0) == '{') {
                    int pos = rootElementName.indexOf('}');
                    qName =
                        new QName(rootElementName.substring(1, pos),
                                rootElementName.substring(pos + 1));
                } else {
                    qName = new QName(rootElementName);
                }
            }
            EncoderFactory factory = EncoderFactory.newInstance();
            EncoderType type = factory.makeType(CustomEncoderProvider.STYLE_ID);
            MetaRef metaRef;
            if (schemaFile != null) {
                metaRef = factory.makeMeta(schemaFile.getAbsolutePath(), qName);
            } else {
                metaRef = factory.makeMeta(schemaURL, qName);
            }
            Encoder encoder = factory.newEncoder(type, metaRef);
            SimpleTestDriver perfTest = new SimpleTestDriver(encoder);
            if ("ds".equals(testWhat)) {
                perfTest.testDecodeFromString(stringData, loops, printResult);
            } else if ("db".equals(testWhat)) {
                perfTest.testDecodeFromBytes(bytesData, loops, printResult);
            } else if ("es".equals(testWhat)) {
                perfTest.testEncodeToString(stringData, loops, printResult);
            } else if ("eb".equals(testWhat)) {
                perfTest.testEncodeToBytes(bytesData, loops, printResult);
            } else {
                System.out.println("Usage: java SimplePerfTest "
                        + "[-d <testing dir>] "
                        + "[-t <test what, value=(ls, lb, ds, db, es or eb)] "
                        + "[-l <number of loops>] "
                        + "[-p]");
                System.exit(1);
            }
        } catch (XmlException e) {
            e.printStackTrace();
            System.exit(2);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(3);
        } catch (TransformerFactoryConfigurationError e) {
            e.printStackTrace();
            System.exit(4);
        } catch (SAXException e) {
            e.printStackTrace();
            System.exit(5);
        } catch (TransformerException e) {
            e.printStackTrace();
            System.exit(6);
        } catch (EncoderConfigurationException e) {
            e.printStackTrace();
            System.exit(7);
        } catch (EncoderException e) {
            e.printStackTrace();
            System.exit(8);
        }
        System.exit(0);
    }
}
