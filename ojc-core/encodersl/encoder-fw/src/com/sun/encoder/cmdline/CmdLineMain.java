/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.encoder.cmdline;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.EncoderProperties;
import com.sun.encoder.EncoderType;
import com.sun.encoder.util.UnicodeFile;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
public class CmdLineMain {

    /**
     * @param args the command line arguments
     * @throws Exception
     */
    public static void process(
            boolean toDecode,
            String xsdFilename,
            String inputFilename,
            String outputFilename,
            String encodingStyle,
            String rootQName,
            String loggerPkgName,
            String logFilename,
            String logLevel)
            throws Exception  {

        System.out.println("toDecode=" + toDecode);
        System.out.println("xsdFilename=" + xsdFilename);
        File metaFile = new File(xsdFilename);
        //Input data
        System.out.println("inputFilename=" + inputFilename);
        File inputFile = new File(inputFilename);
        //Output data
        System.out.println("outputFilename=" + outputFilename);
        File outputFile = new File(outputFilename);

        String predecodeCoding = "";
        String postencodeCoding = "";
        boolean charBased = false;

        System.out.println("rootQName=" + rootQName);
        int closing = -1;
        if (rootQName.length() < 3 || rootQName.charAt(0) != '{'
            || ((closing = rootQName.indexOf("}")) < 0) ) {
            System.out.println("Root QName must be in format of {uri}rootName");
            System.exit(1);
        }
        String uri = rootQName.substring(1, closing);
        String rootName = rootQName.substring(closing + 1);
        QName rootElement = new QName(uri, rootName);

        Logger logger = Logger.getLogger(loggerPkgName);
        System.out.println("logFilename=" + logFilename);
        System.out.println("logLevel=" + logLevel);
        if ("FINER".equals(logLevel)) {
            logger.setLevel(Level.FINER);
        } else if ("FINEST".equals(logLevel)) {
            logger.setLevel(Level.FINEST);
        } else {
            logger.setLevel(Level.FINE);
        }
        Handler logHandler = new FileHandler(logFilename, false);
        LogFormatter formatter = new LogFormatter();
        logHandler.setFormatter(formatter);
        logger.addHandler(logHandler);

        long beg, end;
        System.out.println("--BEGIN--" + new java.util.Date());
        beg = System.currentTimeMillis();

        //Get the encoder factory instance
        EncoderFactory encoderFactory = EncoderFactory.newInstance();
        //Get the encoder type instance using an encoding style
        EncoderType encoderType = encoderFactory.makeType(encodingStyle);

        if (toDecode) {
            decode(encoderType, metaFile, rootElement, inputFile,
                outputFile, predecodeCoding, charBased);
        } else {
            encode(encoderType, metaFile, rootElement,
                inputFile, outputFile, postencodeCoding, charBased);
        }
        System.out.println("--END--" + new java.util.Date());
        end = System.currentTimeMillis();
        System.out.println("Done. Used [" + (end - beg) + "] ms.");
    }

    public static File decode(EncoderType type, File metaFile, QName rootElement,
            File inputFile, File outputFile, String predecodeCoding,
            boolean charBased) throws EncoderException, IOException,
                    TransformerConfigurationException, TransformerException,
                    EncoderConfigurationException {
        Encoder encoder = null;
        Source decodedXML = null;
        Writer writer = null;

        encoder = getEncoder(type, metaFile, rootElement);
        EncoderProperties properties = new EncoderProperties();
        if (predecodeCoding.length() > 0) {
            properties.setPreDecodeCharCoding(predecodeCoding);
        }
        if (charBased) {
            decodedXML = encoder.decodeFromString(UnicodeFile.getText(inputFile), properties);
        } else {
            decodedXML = encoder.decodeFromBytes(loadBytes(inputFile), properties);
        }
        writer = new OutputStreamWriter(new FileOutputStream(outputFile), "UTF-8"); //NOI18N

        StreamResult sResult = new StreamResult(writer);
        TransformerFactory tf = TransformerFactory.newInstance();
        tf.newTransformer().transform(decodedXML, sResult);


        if (!encoder.dispose()) {
            throw new EncoderException("Disposal of the encoder failed.");
        }

        return outputFile;
    }

    public static File encode(EncoderType type, File metaFile, QName rootElement,
            File xmlFile, File outputFile, String postencodeCoding,
            boolean charBased) throws EncoderException, IOException,
                    ParserConfigurationException, SAXException,
                    EncoderConfigurationException {

        Encoder encoder = getEncoder(type, metaFile, rootElement);
        EncoderProperties properties = new EncoderProperties();
        if (postencodeCoding.length() > 0) {
            properties.setPostEncodeCharCoding(postencodeCoding);
        }
        Source decodedXML;
        decodedXML = new DOMSource(loadDocument(xmlFile), xmlFile.toString());

        //Does the encoding
        if (charBased) {
            String encodedResult =
                    encoder.encodeToString(decodedXML, properties);
            UnicodeFile.setText(outputFile, encodedResult);
        } else {
            byte[] encodedResult =
                    encoder.encodeToBytes(decodedXML, properties);
            //Writes the encoded result to a file
            writeBytes(encodedResult, outputFile);
        }

        return null;
    }

    public static Encoder getEncoder(EncoderType type, File xsdFile, QName rootElement)
            throws FileNotFoundException, EncoderConfigurationException {
        if (xsdFile == null || !xsdFile.exists()) {
            throw new java.io.FileNotFoundException("No xsd file.");
        }
        String metaPath = xsdFile.getAbsolutePath();
        EncoderFactory factory = null;
        Encoder encoder = null;
        factory = EncoderFactory.newInstance();
        encoder = factory.newEncoder(type, factory.makeMeta(metaPath, rootElement));
        return encoder;
    }

    /**
     * Writes a byte array to a file
     */
    public static void writeBytes(byte[] bytes, File binFile) throws IOException {
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

    /**
     * Loads a DOM document from a file
     */
    public static Document loadDocument(File docFile)
            throws ParserConfigurationException, SAXException, IOException {
        DocumentBuilderFactory domFactory
            = DocumentBuilderFactory.newInstance();
        domFactory.setNamespaceAware(true);
        DocumentBuilder builder = domFactory.newDocumentBuilder();
        return builder.parse(docFile);
    }

    /**
     * Loads a byte array from a file
     */
    public static byte[] loadBytes(File binFile) throws IOException {
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
}
