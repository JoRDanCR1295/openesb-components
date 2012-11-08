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
 * @(#)TestRuntime.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
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

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.coco.CocoEncoderProvider;

/**
 * This class facilitates simple functional and performance testing on
 * COBOL Copybook encoder.
 * 
 * @author Jun Xu
 */
public class TestRuntime {
    
    private final QName mTopElementName;
    private final URL mSchemaLocation;
    
    public TestRuntime(URL schemaLocation, QName topElementName) {
        mTopElementName = topElementName;
        mSchemaLocation = schemaLocation;
    }
    
    public void testDecodeFromString(String stringData, int loops,
            boolean doPrint)
            throws IOException, SAXException {
        System.out.println("data size (characters) = " + stringData.length());
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        System.out.println("Look up encoder ...");
        Encoder encoder;
        try {
            EncoderFactory factory = EncoderFactory.newInstance();
            encoder =
                factory.newEncoder(
                        factory.makeType(CocoEncoderProvider.STYLE_ID),
                        factory.makeMeta(mSchemaLocation, mTopElementName));
        } catch (EncoderConfigurationException e1) {
            e1.printStackTrace();
            return;
        }
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        SAXSource source; 
        for (i = 0; i < finalLoops; i++) {
            try {
                source = (SAXSource) encoder.decodeFromString(stringData);
                source.getXMLReader().setContentHandler(handler);
                source.getXMLReader().parse(source.getInputSource());
            } catch (EncoderException e) {
                e.printStackTrace();
                break;
            }
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
            throws IOException, SAXException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        System.out.println("Look up encoder ...");
        Encoder encoder;
        try {
            EncoderFactory factory = EncoderFactory.newInstance();
            encoder =
                factory.newEncoder(
                        factory.makeType(CocoEncoderProvider.STYLE_ID),
                        factory.makeMeta(mSchemaLocation, mTopElementName));
        } catch (EncoderConfigurationException e1) {
            e1.printStackTrace();
            return;
        }
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        SAXSource source; 
        for (i = 0; i < finalLoops; i++) {
            try {
                source = (SAXSource) encoder.decodeFromBytes(bytes);
                source.getXMLReader().setContentHandler(handler);
                source.getXMLReader().parse(source.getInputSource());
            } catch (EncoderException e) {
                e.printStackTrace();
                break;
            }
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
    
    public byte[] testEncodeToBytes(String sourceData, int loops,
            boolean doPrint)
            throws IOException, TransformerFactoryConfigurationError,
                    TransformerException, EncoderException {
        System.out.println("source data size (characters) = "
                + sourceData.length());
        
        System.out.println("Prepare DOM Source ...");
        DOMSource domSource = getDOMData(sourceData);
        
        System.out.println("Look up encoder ...");
        Encoder encoder;
        try {
            EncoderFactory factory = EncoderFactory.newInstance();
            encoder =
                factory.newEncoder(
                        factory.makeType(CocoEncoderProvider.STYLE_ID),
                        factory.makeMeta(mSchemaLocation, mTopElementName));
        } catch (EncoderConfigurationException e1) {
            e1.printStackTrace();
            return null;
        }
        
        final int finalLoops = loops;
        int i;
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            encoder.encodeToBytes(domSource);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        //Either visiting DOM tree or transforming is very costy, we need to
        //exclude the cost.  We use the SimpleContentHandler to run it this
        //time.  SimpleContentHandler almost does nothing when doPrint is false.
        SAXResult saxResult = new SAXResult(
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
        byte[] resultBytes = encoder.encodeToBytes(domSource);
        if (doPrint) {
            StringBuffer buf = new StringBuffer();
            HexDump.dump(resultBytes, buf, 0);
            System.out.println(buf.toString());
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("result data size (bytes) = " + resultBytes.length);
        System.out.println("result bytes/ms: "
                + i * resultBytes.length / elapsed);
        return resultBytes;
    }

    private static byte[] getBytes(File file) throws IOException {
        FileInputStream fis = new FileInputStream(file);
        byte[] bytes = new byte[(int) file.length()];
        fis.read(bytes);
        return bytes;
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

    class SimpleContentHandler implements ContentHandler {
        
        private final boolean mDoPrint;
        
        int mIndent = 0;
        boolean mHasChar = false;
        boolean mIsOpen = false;
        
        SimpleContentHandler(boolean doPrint) {
            mDoPrint = doPrint;
        }
        
        public void setDocumentLocator(Locator locator) {
            
        }

        public void startDocument() throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.print(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        }

        public void endDocument() throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.println();
        }

        public void startPrefixMapping(String prefix, String uri)
                throws SAXException {
        }

        public void endPrefixMapping(String prefix)
                throws SAXException {
        }

        public final void startElement(String uri,
                String localName, String qName, Attributes atts)
                throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.println();
            if (mIsOpen) {
                mIndent++;
            }
            printIndent();
            System.out.print("<" + localName
                    + " xmlns=\"" + uri + "\"");
            for (int i = 0; i < atts.getLength(); i++) {
                System.out.print(" " + atts.getQName(i));
                System.out.print("=\"");
                System.out.print(atts.getValue(i));
                System.out.print("\"");
            }
            System.out.print(">");
            mHasChar = false;
            mIsOpen = true;
        }

        public final void endElement(String uri, String localName,
                String qName) throws SAXException {
            if (!mDoPrint) {
                return;
            }
            if (mHasChar) {
                System.out.print("</" + localName + ">");
                mHasChar = false;
            } else {
                System.out.println();
                mIndent--;
                printIndent();
                System.out.print("</" + localName + ">");
            }
            mIsOpen = false;
        }

        public final void characters(char[] ch, int start,
                int length) throws SAXException {
            if (!mDoPrint) {
                return;
            }
            String chars = new String(ch, start, length);
            chars = chars.replace("&", "&amp;");
            System.out.print(chars);
            if (chars.length() > 0) {
                mHasChar = true;
            }
        }

        public void ignorableWhitespace(char[] ch, int start,
                int length) throws SAXException {
        }

        public void processingInstruction(String target,
                String data) throws SAXException {
        }

        public void skippedEntity(String name)
                throws SAXException {
        }
        
        private void printIndent() {
            for (int i = 0; i < mIndent; i++) {
                System.out.print("    ");
            }
        }
    }
    
    private static class JarEntityResolver implements EntityResolver {

        public InputSource resolveEntity(String publicId, String systemId)
                throws SAXException, IOException {
            InputStream in = new URL(systemId).openStream();
            InputSource inputSource = new InputSource(in);
            inputSource.setPublicId(publicId);
            inputSource.setSystemId(systemId);
            return inputSource;
        }
    }
    
    public static void main(String[] args) {
        
        String testWhat = "db";
        int loops = 1;
        boolean printResult = false;
        File testDir =
            new File("test");
        String inputFileName = null;
        String outputFileName = null;
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
            } else if ("-o".equals(args[i])) {
                //output file
                if (i + 1 < args.length) {
                    outputFileName = args[i + 1];
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
            byte[] bytesData = new byte[0];
            if (inputFileName != null) {
                bytesData = getBytes(new File(testDir, inputFileName));
//                Writer writer = 
//                    new OutputStreamWriter(
//                            new FileOutputStream(
//                                    new File(testDir, inputFileName)),
//                                    "Cp037");
//                writer.write(stringData);
//                writer.flush();
//                writer.close();
            }
            
            System.out.println("Prepare XSD ...");
            File schemaFile = null;
            URL schemaURL = null;
            if (schemaFileName == null && schemaURLString == null) {
                System.err.println("No schema file specified.");
                return;
            }
            if (schemaFileName != null) {
                schemaFile = new File(testDir, schemaFileName);
            } else {
                schemaURL = new URL(schemaURLString);
            }
            if (rootElementName == null) {
                System.err.println("No top element specified.");
                return;
            }
            QName qName;
            if (rootElementName.charAt(0) == '{') {
                int pos = rootElementName.indexOf('}');
                qName =
                    new QName(rootElementName.substring(1, pos),
                            rootElementName.substring(pos + 1));
            } else {
                qName = new QName(rootElementName);
            }
            TestRuntime perfTest;
            if (schemaURL != null) {
                perfTest = new TestRuntime(schemaURL, qName);
            } else {
                perfTest = new TestRuntime(schemaFile.toURL(), qName);
            }
            if ("db".equals(testWhat)) {
                perfTest.testDecodeFromBytes(bytesData, loops, printResult);
            } else if ("eb".equals(testWhat)) {
                byte[] result = perfTest.testEncodeToBytes(
                        new String(bytesData, "UTF-8"), loops, printResult);
                if (result != null && outputFileName != null) {
                    OutputStream out =
                        new FileOutputStream(
                                new File(testDir, outputFileName));
                    out.write(result);
                    out.close();
                }
            } else {
                System.out.println("Usage: java SimplePerfTest "
                        + "[-d <testing dir>] "
                        + "[-t <test what, value=(db or eb)] "
                        + "[-l <number of loops>] "
                        + "[-p]");
                System.exit(1);
            }
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
        } catch (EncoderException e) {
            e.printStackTrace();
            System.exit(7);
        }
        System.exit(0);
    }
}
