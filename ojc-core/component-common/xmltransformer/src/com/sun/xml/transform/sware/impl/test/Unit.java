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
 * @(#)Unit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import com.sun.xml.transform.sware.SwareDOMImplementation;
import com.sun.xml.transform.sware.SwareDOMImplementationFactory;
import com.sun.xml.transform.sware.schema.ImplementationType;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * A simple testing class that does following testing work:
 * <p>     1. Loads an XML schema
 * <p>     2. Loads an input XML into a DOM document
 * <p>     3. Applies schema aware DOM sorting to the document (The feature
 *            to be tested)
 * <p>     4. Spits out the sorted DOM document to an output file
 * <p>     5. Uses a validating SAX parser to validate the output file making
 *            sure that the sorting is correct
 *            
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.6 $
 */
public class Unit {

    private final File mSchemaFile;
    private final File mInputFile;
    private final File mOutputFile;
    private SwareDOMImplementation mSwareDOMImpl;

    /**
     * Constructs from a schema file, an input file and an output file.
     * 
     * @param schemaFile a schema file
     * @param inputFile a input file
     * @param outputFile a output file (name)
     */
    public Unit(File schemaFile, File inputFile, File outputFile) {
        this(schemaFile, inputFile, outputFile, null);
    }
    
    /**
     * Constructs from a schema file, an input file and an output file.
     * 
     * @param schemaFile a schema file
     * @param inputFile a input file
     * @param outputFile a output file (name)
     * @param swareDOMImpl a Sware DOM implementation
     */
    public Unit(File schemaFile, File inputFile, File outputFile,
            SwareDOMImplementation swareDOMImpl) {
        mSchemaFile = schemaFile;
        mInputFile = inputFile;
        mOutputFile = outputFile;
        mSwareDOMImpl = swareDOMImpl;
    }
    
    /**
     * Does the testing work.
     * 
     * @throws TransformerException
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public void testSortingCastor()
            throws TransformerException, IOException,
            ParserConfigurationException, SAXException {
        
        if (mSwareDOMImpl == null) {
            mSwareDOMImpl = getSwareDOMImplementation();
        }
        SwareTypeSystem typeSystem;
        typeSystem = loadTypeSystemCastor(mSchemaFile);
        Transformer tfm =
            mSwareDOMImpl.createReorderTransformer(typeSystem, null);
        
//        tfm.setParameter(
//                SwareDOMImplementation.Parameters.PRE_CHECKING, false);
//        tfm.setParameter(
//                SwareDOMImplementation.Parameters.IN_PLACE, false);
        
        DOMSource source =
            new DOMSource(loadDocument(mInputFile).getDocumentElement());
        DOMResult result = new DOMResult();
        
        System.out.println("Start ...");
        long startTime = System.currentTimeMillis();
        tfm.transform(source, result);
        long endTime = System.currentTimeMillis();
        System.out.println("Finished.");
        System.out.println("Time: " + (endTime - startTime) + " ms.");
        
        source = new DOMSource(result.getNode());
        Writer writer =
            new OutputStreamWriter(new FileOutputStream(mOutputFile), "UTF-8");
        StreamResult sResult = new StreamResult(writer);
        TransformerFactory tf = TransformerFactory.newInstance();
        try {
            tf.newTransformer().transform(source, sResult);
        } finally {
            writer.close();
        }
        
        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(true);
        SAXParser parser = factory.newSAXParser();
        parser.setProperty(
            "http://java.sun.com/xml/jaxp/properties/schemaLanguage",
            "http://www.w3.org/2001/XMLSchema"
        );
        parser.setProperty(
                "http://java.sun.com/xml/jaxp/properties/schemaSource",
                mSchemaFile.toURL().toExternalForm()
        );
        XMLReader reader = parser.getXMLReader();
        reader.setErrorHandler(new ErrorHandler() {
            public void warning(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
            public void error(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
            public void fatalError(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
        });
        Reader inputReader =
            new BufferedReader(
                new InputStreamReader(
                    new FileInputStream(mOutputFile), "UTF-8"));
        InputSource inputSource = new InputSource(inputReader);
        try {
            reader.parse(inputSource);
        } finally {
            inputReader.close();
        }
    }
    
    /**
     * Does the testing work.
     * 
     * @throws TransformerException
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws XmlException 
     */
    public void testSortingXmlBeans()
            throws TransformerException, IOException,
            ParserConfigurationException, SAXException, XmlException {
        
        if (mSwareDOMImpl == null) {
            mSwareDOMImpl = getSwareDOMImplementation();
        }
        SwareTypeSystem typeSystem;
        typeSystem = loadTypeSystemXmlBeans(mSchemaFile);
        Transformer tfm =
            mSwareDOMImpl.createReorderTransformer(typeSystem, null);
        
//        tfm.setParameter(
//                SwareDOMImplementation.Parameters.PRE_CHECKING, false);
//        tfm.setParameter(
//                SwareDOMImplementation.Parameters.IN_PLACE, false);
        
        DOMSource source =
            new DOMSource(loadDocument(mInputFile).getDocumentElement());
        DOMResult result = new DOMResult();
        
        System.out.println("Start ...");
        long startTime = System.currentTimeMillis();
        tfm.transform(source, result);
        long endTime = System.currentTimeMillis();
        System.out.println("Finished.");
        System.out.println("Time: " + (endTime - startTime) + " ms.");
        
        source = new DOMSource(result.getNode());
        Writer writer =
            new OutputStreamWriter(new FileOutputStream(mOutputFile), "UTF-8");
        StreamResult sResult = new StreamResult(writer);
        TransformerFactory tf = TransformerFactory.newInstance();
        try {
            tf.newTransformer().transform(source, sResult);
        } finally {
            writer.close();
        }
        
        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(true);
        SAXParser parser = factory.newSAXParser();
        parser.setProperty(
            "http://java.sun.com/xml/jaxp/properties/schemaLanguage",
            "http://www.w3.org/2001/XMLSchema"
        );
        parser.setProperty(
                "http://java.sun.com/xml/jaxp/properties/schemaSource",
                mSchemaFile.toURL().toExternalForm()
        );
        XMLReader reader = parser.getXMLReader();
        reader.setErrorHandler(new ErrorHandler() {
            public void warning(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
            public void error(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
            public void fatalError(SAXParseException exception)
                    throws SAXException {
                throw exception;
            }
        });
        Reader inputReader =
            new BufferedReader(
                new InputStreamReader(
                    new FileInputStream(mOutputFile), "UTF-8"));
        InputSource inputSource = new InputSource(inputReader);
        try {
            reader.parse(inputSource);
        } finally {
            inputReader.close();
        }
    }
    
    public void testValidation() {
        
        SchemaFactory factory = 
            SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
        
        try {
            File schemaLocation = new File("/opt/xml/docbook/xsd/docbook.xsd");
            javax.xml.validation.Schema schema = factory.newSchema(mSchemaFile);
        
            Validator validator = schema.newValidator();
            
            DOMSource source = new DOMSource(loadDocument(mInputFile));
        
            long start = System.currentTimeMillis();
            validator.validate(source);
            long end = System.currentTimeMillis();
            System.out.println(mInputFile.toString() + " is valid. time = " + (end - start));
        } catch (SAXException ex) {
            System.out.println(mInputFile.toString()
                    + " is not valid because ");
            System.out.println(ex.getMessage());
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }  
    }
    
    /**
     * Gets the schema aware DOM implementation.
     * 
     * @return an instance of SwareDOMImplementation
     * @throws ParserConfigurationException
     */
    public static SwareDOMImplementation getSwareDOMImplementation()
            throws ParserConfigurationException {
        DocumentBuilderFactory domFactory 
            = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = domFactory.newDocumentBuilder();
        DOMImplementation domImpl = builder.getDOMImplementation();
        return SwareDOMImplementationFactory.newSwareDOMImplementation(domImpl);
    }
    
    /**
     * Loads a schema.
     * 
     * @param schFile the schema file
     * @return an instance of SwareSchema
     * @throws IOException
     */
    public static SwareTypeSystem loadTypeSystemCastor(File schFile)
    throws IOException {
            SchemaReader reader = new SchemaReader(schFile, "UTF-8");
            Schema castorSchema = reader.read();
            Set<Schema> tsData = new HashSet<Schema>();
            tsData.add(castorSchema);
            return SwareTypeSystem.Factory.newSchemaTypeSystem(
                    ImplementationType.CASTOR, tsData);
    }
    
    /**
     * Loads a schema.
     * 
     * @param schFile the schema file
     * @return an instance of SwareSchema
     * @throws IOException 
     * @throws XmlException 
     * @throws IOException
     * @throws XmlException 
     */
    public static SwareTypeSystem loadTypeSystemXmlBeans(File schFile)
    throws XmlException, IOException {
        SchemaDocument schemaDoc = SchemaDocument.Factory.parse(schFile);
        XmlOptions options = new XmlOptions();
        options.setCompileDownloadUrls();
        SchemaTypeSystem ts =
            XmlBeans.compileXsd(
                    new SchemaDocument[]{schemaDoc},
                    XmlBeans.getContextTypeLoader(),
                    options);
        SchemaTypeLoader loader =
            XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{XmlBeans.getContextTypeLoader(),
                            ts});
        return SwareTypeSystem.Factory.newSchemaTypeSystem(
                ImplementationType.XMLBEANS, loader);
    }
    
    /**
     * Loads an XML file into DOM document.
     * 
     * @param docFile the XML file
     * @return an instance of DOM Document
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     */
    private Document loadDocument(File docFile)
            throws ParserConfigurationException, SAXException, IOException {
        DocumentBuilderFactory domFactory 
            = DocumentBuilderFactory.newInstance();
        domFactory.setNamespaceAware(true);
        DocumentBuilder builder = domFactory.newDocumentBuilder();
        return builder.parse(docFile);
    }
    
    public static void main(String[] args) {
        String sFileName = null;
        String iFileName = null;
        String oFileName = null;
        for (int i = 0; args != null && i < args.length; i++) {
            if ("-s".equals(args[i]) && i < args.length - 1) {
                sFileName = args[i + 1];
            } else if ("-i".equals(args[i]) && i < args.length - 1) {
                iFileName = args[i + 1];
            } else if ("-o".equals(args[i]) && i < args.length - 1) {
                oFileName = args[i + 1];
            }
        }
        if (sFileName == null) {
            System.err.println("Missing schema file name."
                    + " Usage: Unit -s <schema file>"
                    + " -i <input file> -o <output file>");
            return;
        }
        if (iFileName == null) {
            System.err.println("Missing input file name."
                    + " Usage: Unit -s <schema file>"
                    + " -i <input file> -o <output file>");
            return;
        }
        if (oFileName == null) {
            System.err.println("Missing output file name."
                    + " Usage: Unit -s <schema file>"
                    + " -i <input file> -o <output file>");
            return;
        }
        
        Unit ts =
            new Unit(new File(sFileName),
                new File(iFileName), new File(oFileName));
        
        try {
            ts.testSortingXmlBeans();
        } catch (Exception all) {
            all.printStackTrace();
        }
    }
}
