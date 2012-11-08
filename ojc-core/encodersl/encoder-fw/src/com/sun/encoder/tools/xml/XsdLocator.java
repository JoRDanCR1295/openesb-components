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
 * @(#)XsdLocator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.tools.xml;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * A utility class that facilitates locating an XSD based on certain criteria.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public final class XsdLocator {

    private static final QName XSD_ELEMENT_NAME =
        new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "element");
    
    private static final QName NAME_ATTR_NAME = new QName("name");
    
    /**
     * Finds an XSD containing a specific element declaration by looking
     * through all XSDs under one specified directory (if the <code>dir</code>
     * parameter points to a directory) or by looking up the specified XSD
     * (if the <code>dir</code> parameter points to a file).  Only files
     * directly under the directory are searched, no recursive search performed.
     * 
     * @param dir the directory containing the XSDs or the XSD file to be
     *              searched
     * @param name the qualified name of the element declaration
     * @return The file that contains the element declaration,
     *          or <code>null</code> if none of the XSD files contains the
     *          element declaration.
     * @throws XmlException XML exception thrown by XmlBeans
     * @throws IOException IO exception thrown by SAX parser or XmlBeans
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public static File findXsdByElement(File dir, QName name)
    throws XmlException, IOException, ParserConfigurationException,
    SAXException {
        ArrayList<File> fileList = new ArrayList<File>();
        if (!dir.isDirectory()) {
            fileList.add(dir);
            return findXsdByElement(fileList, name);
        }
        File[] files = dir.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.endsWith(".xsd");
            }
        });
        if (files == null) {
            return null;
        }
        for (int i = 0; i < files.length; i++) {
            if (files[i].isFile()) {
                fileList.add(files[i]);
            }
        }
        return findXsdByElement(fileList, name);
    }
    
    /**
     * Finds the XSD containing a specific element declaration by looking
     * through a list XSD files.
     * 
     * @param xsdFileList a list of XSD files
     * @param name the element declaration to be looked for
     * @return The file that contains the element declaration,
     *          or <code>null</code> if none of the XSD files contains the
     *          element declaration.
     * @throws XmlException XML exception thrown by XmlBeans
     * @throws IOException IO exception thrown by SAX parser or XmlBeans
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public static File findXsdByElement(List<File> xsdFileList, QName name)
    throws XmlException, IOException, ParserConfigurationException,
    SAXException {
        if (xsdFileList == null || xsdFileList.size() == 0) {
            return null;
        }
        for (File file : xsdFileList) {
            if (containsElement(file, name)) {
                return file;
            }
        }
        return null;
    }
    
    /**
     * Checks if the XSD file contains the named element declaration.
     * 
     * @param xsdFile the XSD file
     * @param name the qualified name if the element declaration to be looked up
     * @return <code>true</code> if the XSD file contains the element
     *              declaration
     * @throws IOException 
     * @throws XmlException 
     * @throws SAXException 
     * @throws ParserConfigurationException 
     */
    public static boolean containsElement(File xsdFile, QName name)
    throws XmlException, IOException, ParserConfigurationException,
    SAXException {
        if (!sameNamespace(getPartialSchame(xsdFile).getTargetNamespace(),
                name.getNamespaceURI())) {
            return false;
        }
        SchemaDocument schemaDoc = SchemaDocument.Factory.parse(xsdFile);
        Schema schema = schemaDoc.getSchema();
        if (schema == null) {
            return false;
        }
        XmlCursor cursor = schema.newCursor();
        try {
            boolean cursorMoved = cursor.toChild(XSD_ELEMENT_NAME);
            if (!cursorMoved) {
                return false;
            }
            String elemName;
            final String expected = name.getLocalPart(); 
            while (cursorMoved) {
                elemName = cursor.getAttributeText(NAME_ATTR_NAME);
                if (expected.equals(elemName)) {
                    return true;
                }
                cursorMoved = cursor.toNextSibling(XSD_ELEMENT_NAME);
            }
            return false;
        } finally {
            cursor.dispose();
        }
//XPath way but need Saxon 8.1.1
//        StringBuilder sb = new StringBuilder();
//        sb.append("declare namespace xs='");
//        sb.append(XMLConstants.W3C_XML_SCHEMA_NS_URI);
//        sb.append("' ./xs:element[@name='");
//        sb.append(name.getLocalPart());
//        sb.append("']");
//        XmlObject[] result = schema.selectPath(sb.toString());
//        return result != null && result.length > 0;
    }
    
    public static boolean sameNamespace(String ns1, String ns2) {
        ns1 = (ns1 == null ? "" : ns1);
        ns2 = (ns2 == null ? "" : ns2);
        return ns1.equals(ns2);
    }
    
    /**
     * Loads partial schema from an XSD file.
     * 
     * @param xsdFile the XSD file
     * @return An instance of <code>PartialSchema</code>
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     */
    private static PartialSchema getPartialSchame(File xsdFile)
    throws ParserConfigurationException, SAXException, IOException {
        return PartialSchema.loadPartialSchema(xsdFile);
    }
    
    /**
     * The instance of this class represents limited schema information for
     * an XML schema so loading such a partial schema is faster than loading
     * the full schema. 
     */
    private static class PartialSchema {
        
        private String _targetNamespace;
        
        public static PartialSchema loadPartialSchema(File xsdFile)
        throws ParserConfigurationException, SAXException, IOException {
            SAXParserFactory factory = SAXParserFactory.newInstance();
            factory.setNamespaceAware(true);
            SAXParser parser = factory.newSAXParser();
            PartialSchema schema = new PartialSchema();
            PartialSchemaHandler handler = new PartialSchemaHandler(schema);
            try {
                parser.parse(xsdFile, handler);
            } catch (SAXException e) {
                if (!(e instanceof StopParsingException)) {
                    throw e;
                }
            }
            return schema;
        }
        
        private PartialSchema() {
            //Suppress the default constructor
        }
        
        public String getTargetNamespace() {
            return _targetNamespace;
        }
    }
    
    /**
     * Partial schema handler to get partial schema information.
     */
    private static class PartialSchemaHandler extends DefaultHandler {
        
        private final PartialSchema _schema;
        
        public PartialSchemaHandler(PartialSchema schema) {
            _schema = schema;
        }

        @Override
        public void startElement(String uri, String localName, String name,
                Attributes attributes) throws SAXException {
            if(localName.equals("schema")) {
                for(int i = 0; i < attributes.getLength(); i++) {
                    String attrQName = attributes.getQName(i); 
                    if(attrQName.equals("targetNamespace")) {
                        _schema._targetNamespace = attributes.getValue(i);
                        break;
                    }
                }
                throw new StopParsingException();
            }
        }
    }

    /**
     * An exception used to stop parsing.
     */
    private static class StopParsingException extends SAXException {
        private static final long serialVersionUID = 1L;

        public StopParsingException() {
            super();
        }

        public StopParsingException(Exception e) {
            super(e);
        }

        public StopParsingException(String message, Exception e) {
            super(message, e);
        }

        public StopParsingException(String message) {
            super(message);
        }
    }
//
//    /**
//     * Trivial test.
//     * 
//     * @param argv
//     */
//    public static void main(String[] argv) {
//        File dir =
//            new File("C:/incoming/HL7_DFT_Example/src");
//        try {
//            long start = System.currentTimeMillis();
//            File xsdFile = null;
//            for (int i = 0; i < 10; i++) {
//                xsdFile =
//                    findXsdByElement(dir,
//                            new QName("urn:hl7-org:v2xml", "DFT_P03"));
//            }
//            long end = System.currentTimeMillis();
//            System.out.println("The XSD is: " + xsdFile);
//            System.out.println("Time: " + (end - start));
//        } catch (XmlException e) {
//            e.printStackTrace();
//        } catch (IOException e) {
//            e.printStackTrace();
//        } catch (ParserConfigurationException e) {
//            e.printStackTrace();
//        } catch (SAXException e) {
//            e.printStackTrace();
//        }
//    }
}
