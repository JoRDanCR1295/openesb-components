/*
 * Helper.java
 */

package com.sun.jbi.sample.component.common;

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
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This is a helper class that have bunch of methods for xml processing.
 * @author chikkala
 */
public class Helper {
    /**
     * return the DOM Document
     * @param xmlReader Reader
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public static Document buildDOMDocument(Reader xmlReader) throws Exception {
        Document xmlDoc = null;
        DocumentBuilderFactory docBuilderFactory =
            DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(false);
        DocumentBuilder docBuilder =
            docBuilderFactory.newDocumentBuilder();
        docBuilder.setErrorHandler( new DefaultHandler() {
            public void fatalError(SAXParseException e)
            throws SAXException {
                throw new SAXException(e.getMessage());
            }
        });
        
        docBuilder.setEntityResolver(new EntityResolver() {
            public InputSource resolveEntity(String publicId, String systemId)
            throws SAXException, IOException {
                StringReader reader =
                    new StringReader("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N
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
    /**
     * reads xml text from DOMSource to StringBuffer
     */
    public static StringBuffer readFromDOMSource(DOMSource domSource) {
        
        StringWriter writer = new StringWriter();
        
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = null;
        try {
            trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
                "yes");
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
     */
    public  static StringBuffer readFromInputSource(InputSource inSource) {
        
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        InputStream inStream = inSource.getByteStream();
        Reader reader = inSource.getCharacterStream();
        if ( reader == null ) {
            reader = new InputStreamReader(inStream);
        }
        BufferedReader buff = new BufferedReader(reader);
        try {
            
            for ( String line = null; (line = buff.readLine()) != null ; ) {
                out.println(line);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
        return writer.getBuffer();
    }
    /**
     * reads xml from from DOM, SAX or Stream Source into a string buffer
     */
    public static StringBuffer readFromSource(Source source) {
        if ( source instanceof DOMSource ) {
            return readFromDOMSource((DOMSource)source);
        } else {
            InputSource inSource = SAXSource.sourceToInputSource(source);
            if ( inSource != null ) {
                return readFromInputSource(inSource);
            } else {
                return null;
            }
        }
    }
    /**
     * creates a DOMSource from the xml text read from the reader.
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
     */
    public static StringBuffer getExceptionStackTrace(Exception ex) {
        StringWriter strWriter = new StringWriter();
        if ( ex != null ) {
            PrintWriter out = new PrintWriter(strWriter);
            ex.printStackTrace(out);
        }
        return strWriter.getBuffer();
    }
    /**
     * may be used to set the exception as fault content.
     */
    public static String getExceptionAsXmlText(Exception ex) {
        String message = ex.getMessage();
        String stackTrace = getExceptionStackTrace(ex).toString();
        String exXmlText =
            "<exception>" +
            "<message>" + message + "</message>" +
            "<stack-trace>" + stackTrace + "</stack-trace>" +
            "</exception>" ;
        return exXmlText;
    }
    
}
