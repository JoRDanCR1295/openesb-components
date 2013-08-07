/*
 * RuntimeHelper.java
 */

package org.openesb.components.camelse.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;
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
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This is a helper class that have bunch of methods for xml processing.
 * @author chikkala
 */
public class RuntimeHelper {
    
    public static Logger getLogger() {
        return RuntimeContext.getInstance().getLogger();
    }
    
    public static void logWarning(Object logObj) {
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.WARNING, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().warning(logObj.toString());
        }
    }
    
    public static void logError(Object logObj) {
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.SEVERE, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().severe(logObj.toString());
        }
    }
    
    public static void logDebug(Object logObj) {
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.FINER, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().finer(logObj.toString());
        }
    }
    
    public static String getComponentName() {
        return RuntimeContext.getInstance().getComponentName();
    }
    public static ComponentContext getComponentContext() {
        return RuntimeContext.getInstance().getComponentContext();
    }
    public static DeliveryChannel getDeliveryChannel() {
        return RuntimeContext.getInstance().getDeliveryChannel();
    }
    
    public static MessageExchangeSupport getMessageExchangeSupport() {
        return RuntimeContext.getInstance().getMessageExchangeSupport();
    }
    
    /**
     * return the DOM Document
     * @param xmlReader Reader
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public static Document buildDOMDocument(Reader xmlReader) throws Exception {
        InputSource xmlSource = new InputSource(xmlReader);
        return buildDOMDocument(xmlSource);
    }
    /**
     * return the DOM Document
     * @param xmlReader Reader
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public static Document buildDOMDocument(InputSource xmlSource) throws Exception {
        Document xmlDoc = null;
        DocumentBuilderFactory docBuilderFactory =
            DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(false);
        docBuilderFactory.setNamespaceAware(true);
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
        
        xmlDoc = docBuilder.parse(xmlSource);
        
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
     * creates a DOMSource from any source. If the source itself is DOMSource,
     * the source is returned as it is as DOMSource.
     */
    public static DOMSource sourceToDOMSource(Source source) {
        if ( source instanceof DOMSource) {
            return (DOMSource) source;
        }
        InputSource xmlSource = SAXSource.sourceToInputSource(source);
        Document doc = null;
        try {
            doc = buildDOMDocument(xmlSource);
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
        String message = replaceXmlEscapeCharsToEntityRefereces(ex.getMessage());
        String stackTrace = replaceXmlEscapeCharsToEntityRefereces(
            getExceptionStackTrace(ex).toString());
        String exXmlText =
            "<java-exception xmlns=\"http://java.sun.com/ns/jbi/samples/java-exception/types/\" >" +
            "<ex-message>" + message + "</ex-message>" +
            "<ex-stack-trace>" + stackTrace + "</ex-stack-trace>" +
            "</java-exception>" ;
        return exXmlText;
    }
    
    /**
     * may be used to set the exception as fault content.
     */
    public static String getExceptionAsText(Exception ex) {
        String message = replaceXmlEscapeCharsToEntityRefereces(ex.getMessage());
        String stackTrace = replaceXmlEscapeCharsToEntityRefereces(
            getExceptionStackTrace(ex).toString());
        StringBuffer buff = new StringBuffer();
        buff.append(message);
        buff.append(System.getProperty("line.separator", "\n"));
        buff.append(stackTrace);
        return buff.toString();
    }
    
    
    /**
     * For attribute values which denote a QName, i.e. include a namespace prefix,
     * resolve the value into a QName.
     * If a namespace can not be resolved, it is set to empty - it does not
     * result in an exception
     * @param attrValue the string value of the attribute
     * @param element the element the attribute belongs to
     */
    public static QName resolveAttrQName(String attrValue, Element element) {
        int aColonLoc = attrValue.indexOf(":");
        String aLocalName = attrValue;
        String aPrefix = null;
        String aNS = null;
        if (aColonLoc > -1) {
            aPrefix = attrValue.substring(0, aColonLoc);
            aLocalName = attrValue.substring(aColonLoc + 1);
            
            // Traverse up the hierarchy until a namespace definition is found
            // or the top of the document is reached.
            Node currNode = element;
            while ((aNS == null || aNS.equals("")) && currNode != null) {
                if (currNode.getNodeType() == Node.ELEMENT_NODE) {
                    aNS = ((Element) currNode).getAttribute("xmlns:" + aPrefix);
                }
                currNode = currNode.getParentNode();
            }
        }
        
        QName qName = new QName(aNS, aLocalName, aPrefix);
        
        return qName;
    }
    
    /**
     * replaces the xml entity references with the xml escape chars
     * @param xmlString Text with the xml escape chars
     * @param Text with the xml entity references
     */
    public static String replaceXmlEscapeCharsToEntityRefereces(String xmlString) {
        if ( xmlString == null ) {
            return xmlString;
        }
        
        // just convert < , > and & only
        StringBuffer sbuff = new StringBuffer(2 * xmlString.length());
        for ( int i = 0; i < xmlString.length(); ++i ) {
            switch ( xmlString.charAt(i) ) {
                case '&': sbuff.append("&amp;");
                break;
                case '<': sbuff.append("&lt;");
                break;
                case '>': sbuff.append("&gt;");
                break;
                default: sbuff.append( xmlString.charAt(i) );
            }
        }
        return sbuff.toString();
    }
    
    /**
     * return Element node from a document node or non document. Use to extract
     * the message root element.
     * @root node from which the Element node will be extracted.
     * @return Element node.
     */
    public static Element getElement(Node root) {
        Element msgEl = null;
        if ( root instanceof Document) {
            msgEl = ((Document)root).getDocumentElement();
        } else if (root instanceof Element) {
            msgEl = (Element)root;
        } else {
            NodeList nodeList = root.getChildNodes();
            for ( int i=0; i < nodeList.getLength(); ++i) {
                Node node = nodeList.item(i);
                if ( node instanceof Element ) {
                    msgEl = (Element) node;
                    break;
                }
            }
        }
        return msgEl;
    }
    
    public static Element getElement(DOMSource domSource) {
        return getElement(domSource.getNode());
    }
    
}
