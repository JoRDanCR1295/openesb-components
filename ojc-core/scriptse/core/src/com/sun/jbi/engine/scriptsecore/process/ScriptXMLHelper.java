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
 * @(#)ScriptXMLHelper.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scriptsecore.process;

import org.w3c.dom.Document;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

/**
 * DOCUMENT ME!
 *
 * @author prashanthbr
 */
public class ScriptXMLHelper {

    /**
     * Creates a new instance of ScriptXMLHelper
     */
    public ScriptXMLHelper() {
    }

    /**
     * return the DOM Document
     *
     * @param xmlReader Reader
     *
     * @return dom document
     *
     * @throws Exception on parser exception or any other exception
     * @throws SAXException DOCUMENT ME!
     */
    public static Document buildDOMDocument(Reader xmlReader)
            throws Exception {
        Document xmlDoc = null;
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(false);
        docBuilderFactory.setNamespaceAware(true);

        DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

        docBuilder.setErrorHandler(new DefaultHandler() {

            public void fatalError(SAXParseException e)
                    throws SAXException {
                throw new SAXException(e.getMessage());
            }
        });

        docBuilder.setEntityResolver(new EntityResolver() {

            public InputSource resolveEntity(String publicId,
                    String systemId) throws SAXException, IOException {
                StringReader reader = new StringReader(
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N

                InputSource source = new InputSource(reader);
                source.setPublicId(publicId);
                source.setSystemId(systemId);

                return source;
            }
        });

        InputSource is = new InputSource(xmlReader);
        xmlDoc = docBuilder.parse(is);

        return xmlDoc;
    }

    //    public static Document buildDOMDocument(Reader xmlReader, Document aDoc) throws Exception {
    //       
    //        DocumentBuilderFactory docBuilderFactory =
    //            DocumentBuilderFactory.newInstance();
    //        docBuilderFactory.setValidating(false);
    //        DocumentBuilder docBuilder =
    //            docBuilderFactory.newDocumentBuilder();
    //        docBuilder.setErrorHandler( new DefaultHandler() {
    //            public void fatalError(SAXParseException e)
    //            throws SAXException {
    //                throw new SAXException(e.getMessage());
    //            }
    //        });
    //        
    //        docBuilder.setEntityResolver(new EntityResolver() {
    //            public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
    //                StringReader reader = new StringReader("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N
    //                InputSource source = new InputSource(reader);
    //                source.setPublicId(publicId);
    //                source.setSystemId(systemId);
    //                return source;
    //            }
    //        });
    //        
    //        InputSource is = new InputSource(xmlReader);
    //        aDoc = docBuilder.parse(is);
    //        
    //        return aDoc;
    //    }
    /**
     * reads xml text from DOMSource to StringBuffer
     *
     * @param domSource DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static StringBuffer readFromDOMSource(DOMSource domSource) {
        StringWriter writer = new StringWriter();

        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = null;

        try {
            trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");

            StreamResult result = new StreamResult(writer);
            trans.transform(domSource, result);
        } catch (TransformerConfigurationException ex) {
            ex.printStackTrace();
        } catch (TransformerException ex) {
            ex.printStackTrace();
        }

        return writer.getBuffer();
    }

    /**
     * reads the xml text from InputSource into a StringBuffer
     *
     * @param inSource DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static StringBuffer readFromInputSource(InputSource inSource) {
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        InputStream inStream = inSource.getByteStream();
        Reader reader = inSource.getCharacterStream();

        if (reader == null) {
            reader = new InputStreamReader(inStream);
        }

        BufferedReader buff = new BufferedReader(reader);

        try {
            for (String line = null; (line = buff.readLine()) != null;) {
                out.println(line);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }

        return writer.getBuffer();
    }

    /**
     * reads xml from from DOM, SAX or Stream Source into a string buffer
     *
     * @param source DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static StringBuffer readFromSource(Source source) {
        if (source instanceof DOMSource) {
            return readFromDOMSource((DOMSource) source);
        } else {
            InputSource inSource = SAXSource.sourceToInputSource(source);

            if (inSource != null) {
                return readFromInputSource(inSource);
            } else {
                return null;
            }
        }
    }

    /**
     * creates a DOMSource from the xml text read from the reader.
     *
     * @param xmlReader DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static DOMSource createDOMSource(Reader xmlReader) {
        Document doc = null;

        try {
            doc = buildDOMDocument(xmlReader);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return new DOMSource(doc);
    }

    /**
     * converts the ex stracktrace to string.
     *
     * @param ex DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static StringBuffer getExceptionStackTrace(Exception ex) {
        StringWriter strWriter = new StringWriter();

        if (ex != null) {
            PrintWriter out = new PrintWriter(strWriter);
            ex.printStackTrace(out);
        }

        return strWriter.getBuffer();
    }

    /**
     * may be used to set the exception as fault content.
     *
     * @param ex DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getExceptionAsXmlText(Exception ex) {
        String message = ex.getMessage();
        String stackTrace = getExceptionStackTrace(ex).toString();
        String exXmlText = "<exception>" + "<message>" + message +
                "</message>" + "<stack-trace>" + stackTrace + "</stack-trace>" +
                "</exception>";

        return exXmlText;
    }
} //class ScriptXMLHelper ends.
