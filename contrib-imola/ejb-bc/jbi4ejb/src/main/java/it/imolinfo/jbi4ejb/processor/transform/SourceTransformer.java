/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.processor.transform;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.configuration.InterfaceExtractorUtil;
import it.imolinfo.jbi4ejb.jbi.Messages;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Constructor;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.codehaus.xfire.util.stax.W3CDOMStreamReader;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * Taken from servicemix-core <code>SourceTransformer</code> and partially
 * from <code>StAXSoureTransformer</code> class. 
 * // TODO Add<code>StaxSource</code> support
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */

public class SourceTransformer {
    
    /*
     * When converting a DOM tree to a SAXSource,
     * we try to use Xalan internal DOM parser if
     * available.  Else, transform the DOM tree
     * to a String and build a SAXSource on top of
     * it.
     */
    /** The Constant dom2SaxClass. */
    private static final Class dom2SaxClass;
    
    /** The Constant LOG. */
    private static final Logger LOG
    = LoggerFactory.getLogger(SourceTransformer.class); 
    private static final Messages MESSAGES
    = Messages.getMessages(SourceTransformer.class);

    /** The Constant DEFAULT_CHARSET_PROPERTY. */
    private static final String DEFAULT_CHARSET_PROPERTY = "org.apache.servicemix.default.charset";
    
    /** The default charset. */
    private static String defaultCharset = System.getProperty(DEFAULT_CHARSET_PROPERTY, "UTF-8");    

    /** The document builder factory. */
    private DocumentBuilderFactory documentBuilderFactory;
    
    /** The transformer factory. */
    private TransformerFactory transformerFactory;

    /** The input factory. */
    private XMLInputFactory inputFactory;   

    /**
     * Instantiates a new source transformer.
     */
    public SourceTransformer() {
    }

    /**
     * Instantiates a new source transformer.
     * 
     * @param documentBuilderFactory the <code>DocumentBuilderFactory</code>
     */
    public SourceTransformer(DocumentBuilderFactory documentBuilderFactory) {
        this.documentBuilderFactory = documentBuilderFactory;
    }


    /**
     * Converts the given input Source into the required result.
     * 
     * @param source the source to transform
     * @param result the result of the transformation
     * 
     * @throws TransformerException if some transformation error occurs
     */
    public void toResult(Source source, Result result) throws TransformerException {
        if (source == null) {
            return;
        }
        Transformer transformer = createTransfomer();
        if (transformer == null) {
        	String msg=MESSAGES.getString("EJB000701_Could_not_create_transformer");
            LOG.error(msg);
            throw new TransformerException(msg);
        }
        transformer.setOutputProperty(OutputKeys.ENCODING, defaultCharset);
        transformer.transform(source, result);
    }


    /**
     * Converts the given input Source into text.
     * 
     * @param source the source 
     * 
     * @return
     *      The String verions of the source
     * 
     * @throws TransformerException if some transformation error occurs
     */
    public String toString(Source source) throws TransformerException {
        if (source == null) {
            return null;
        } else if (source instanceof StringSource) {
            return ((StringSource) source).getText();
        } else {
            StringWriter buffer = new StringWriter();
            toResult(source, new StreamResult(buffer));
            return buffer.toString();
        }
    }

    /**
     * Converts the given input Node into text.
     * 
     * @param node the Dom node
     * 
     * @return
     *      The <code>Node</code> as a String
     * 
     * @throws TransformerException if some transformation error occurs
     */
    public String toString(Node node) throws TransformerException {
        return toString(new DOMSource(node));
    }

    /**
     * Converts the content of the given message to a String.
     * 
     * @param message The message
     * 
     * @return The message content as String
     *          
     * @throws SAXException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws MessagingException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public String contentToString(NormalizedMessage message) throws MessagingException, TransformerException, ParserConfigurationException, IOException, SAXException {
        return toString(message.getContent());
    }

    /**
     * Converts the source instance to a {@link DOMSource} or returns null if
     * the conversion is not supported (making it easy to derive from this class
     * to add new kinds of conversion).
     * 
     * @param source
     *      The source to transform
     * 
     * @return
     *      The DOMSource from the source.
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public DOMSource toDOMSource(Source source) throws ParserConfigurationException, IOException, SAXException, TransformerException {
        if (source instanceof DOMSource) {
            return (DOMSource) source;
        }
        else if (source instanceof SAXSource) {
            return toDOMSourceFromSAX((SAXSource) source);
        }
        else if (source instanceof StreamSource) {
            return toDOMSourceFromStream((StreamSource) source);
        }
        else {
            return null;
        }
    }

    /**
     * To DOM source.
     * 
     * @param message
     *          The NormalizedMessage
     * 
     * @return the source
     * 
     * @throws MessagingException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     */
    public Source toDOMSource(NormalizedMessage message) throws MessagingException, TransformerException, ParserConfigurationException, IOException, SAXException {
        Node node = toDOMNode(message);
        return new DOMSource(node);
    }

    /**
     * Converts the source instance to a {@link SAXSource} or returns null if
     * the conversion is not supported (making it easy to derive from this class
     * to add new kinds of conversion).
     * 
     * @param source The source to transform
     * 
     * @return the SAXSource from the Source
     * 
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public SAXSource toSAXSource(Source source) throws IOException, SAXException, TransformerException {
        if (source instanceof SAXSource) {
            return (SAXSource) source;
        }
        else if (source instanceof DOMSource) {
            return toSAXSourceFromDOM((DOMSource) source);
        }
        else if (source instanceof StreamSource) {
            return toSAXSourceFromStream((StreamSource) source);
        }
        else {
            return null;
        }
    }

    /**
     * To stream source.
     * 
     * @param source
     *          The Source
     * 
     * @return the stream source
     * 
     * @throws TransformerException
     *             If some problem occurs
     */
    public StreamSource toStreamSource(Source source) throws TransformerException {
        if (source instanceof StreamSource) {
            return (StreamSource) source;
        } else if (source instanceof DOMSource) {
            return toStreamSourceFromDOM((DOMSource) source);
        } else if (source instanceof SAXSource) {
            return toStreamSourceFromSAX((SAXSource) source);
        } else {
            return null;
        }
    }

    /**
     * To stream source from SAX.
     * 
     * @param source
     *          The Source
     * @return the stream source
     * 
     * @throws TransformerException
     *             If some problem occurs
     */
    public StreamSource toStreamSourceFromSAX(SAXSource source) throws TransformerException {
        InputSource inputSource = source.getInputSource();
        if (inputSource != null) {
            if (inputSource.getCharacterStream() != null) {
                return new StreamSource(inputSource.getCharacterStream());
            }
            if (inputSource.getByteStream() != null) {
                return new StreamSource(inputSource.getByteStream());
            }
        }
        String result = toString(source);
        return new StringSource(result);
    }

    /**
     * To stream source from DOM.
     * 
     * @param source
     *          The DOMSource
     * 
     * @return the stream source
     * 
     * @throws TransformerException
     *             If some problem occurs
     */
    public StreamSource toStreamSourceFromDOM(DOMSource source) throws TransformerException {
        String result = toString(source);
        return new StringSource(result);
    }

    /**
     * To SAX source from stream.
     * 
     * @param source
     *          The Source
     * @return the SAX source
     */
    public SAXSource toSAXSourceFromStream(StreamSource source) {
        InputSource inputSource;
        if (source.getReader() != null) {
            inputSource = new InputSource(source.getReader());
        } else {
            inputSource = new InputSource(source.getInputStream());
        }
        inputSource.setSystemId(source.getSystemId());
        inputSource.setPublicId(source.getPublicId());
        return new SAXSource(inputSource);
    }

    /**
     * To reader from source.
     * 
     * @param src
     *          The source
     * 
     * @return the reader
     * 
     * @throws TransformerException
     *             If some problem occurs
     */
    public Reader toReaderFromSource(Source src) throws TransformerException {
        StreamSource stSrc = toStreamSource(src);
        Reader r = stSrc.getReader();
        if (r == null) {
            r = new InputStreamReader(stSrc.getInputStream());
        }
        return r;
    }

    /**
     * To DOM source from stream.
     * 
     * @param source
     *          The stream source
     * 
     * @return the DOM source
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     */
    public DOMSource toDOMSourceFromStream(StreamSource source) throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilder builder = createDocumentBuilder();
        String systemId = source.getSystemId();
        Document document = null;
        Reader reader = source.getReader();
        if (reader != null) {
            document = builder.parse(new InputSource(reader));
        } else {
            InputStream inputStream = source.getInputStream();
            if (inputStream != null) {
                InputSource inputsource = new InputSource(inputStream);
                inputsource.setSystemId(systemId);
                document = builder.parse(inputsource);
            }
            else {
            	String msg=MESSAGES.getString("EJB000702_No_input_stream_or_reader_available");
                LOG.error(msg);
                throw new IOException(msg);   
            }
        }
        return new DOMSource(document, systemId);
    }


    /**
     * Static initializer
     */
    static {
        Class cl = null;
        try {
            cl = Class.forName("org.apache.xalan.xsltc.trax.DOM2SAX");
        } catch (Throwable t) {
            // Static initializer, do nothing...
        	LOG.error("EJB000703_Static_initializer", new Object[]{t.getMessage()});
            
        }
        dom2SaxClass = cl;
    }

    /**
     * To SAX source from DOM.
     * 
     * @param source
     *          The DOMSource
     * 
     * @return the SAX source
     * 
     * @throws TransformerException
     *             If some problem occurs  
     */
    public SAXSource toSAXSourceFromDOM(DOMSource source) throws TransformerException {
        if (dom2SaxClass != null) {
            try {
                Constructor cns = dom2SaxClass.getConstructor(new Class[] { Node.class });
                XMLReader converter = (XMLReader) cns.newInstance(new Object[] { source.getNode() });
                return new SAXSource(converter, new InputSource());
            } catch (Exception e) {
            	String msg=MESSAGES.getString("EJB000704_Exception_in_toSAXSourceFromDOM", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new TransformerException(msg,e);   
            }
        } else {
            String str = toString(source);
            StringReader reader = new StringReader(str);
            return new SAXSource(new InputSource(reader));
        }
    }

    /**
     * To DOM source from SAX.
     * 
     * @param source
     *          The source
     * 
     * @return the DOM source
     * 
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public DOMSource toDOMSourceFromSAX(SAXSource source) throws IOException, SAXException, ParserConfigurationException, TransformerException {
        return new DOMSource(toDOMNodeFromSAX(source));
    }

    /**
     * To DOM node from SAX.
     * 
     * @param source
     *          The Source
     * 
     * @return the node
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public Node toDOMNodeFromSAX(SAXSource source) throws ParserConfigurationException, IOException, SAXException, TransformerException {
        DOMResult result = new DOMResult();
        toResult(source, result);
        return result.getNode();
    }

    /**
     * Converts the given TRaX Source into a W3C DOM node.
     * 
     * @param source
     *          The source
     * 
     * @return
     *      The Node
     * 
     * @throws SAXException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public Node toDOMNode(Source source) throws TransformerException, ParserConfigurationException, IOException, SAXException {
        DOMSource domSrc = toDOMSource(source);
        if (domSrc != null) {
            return domSrc.getNode();
        } else {
            return null;
        }
    }

    /**
     * Avoids multple parsing to DOM by caching the DOM representation in the
     * message as a property so future calls will avoid the reparse - and avoid
     * issues with stream based Source instances.
     * 
     * @param message
     *            the normalized message
     * 
     * @return the W3C DOM node for this message
     * 
     * @throws SAXException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws MessagingException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public Node toDOMNode(NormalizedMessage message) throws MessagingException, TransformerException, ParserConfigurationException, IOException, SAXException {
        Source content = message.getContent();
        Node node = toDOMNode(content);
        return node;
    }

    /**
     * Create a DOM element from the normalized message.
     * 
     * @param message
     * 
     * @return
     *      The DOM element
     * @throws MessagingException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     * @throws SAXException
     */
    public Element toDOMElement(NormalizedMessage message) throws MessagingException, TransformerException, ParserConfigurationException, IOException, SAXException {
        Node node = toDOMNode(message);
        return toDOMElement(node);
    }

    /**
     * Create a DOM element from the given source.
     * 
     * @param source
     *      The Source
     * 
     * @return
     *      The DOM element
     * 
     * @throws TransformerException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     */
    public Element toDOMElement(Source source) throws TransformerException, ParserConfigurationException, IOException, SAXException {
        Node node = toDOMNode(source);
        return toDOMElement(node);
    }

    /**
     * Create a DOM element from the DOM node. Simply cast if the node is an
     * Element, or return the root element if it is a Document.
     * 
     * @param node
     *          The DOM node
     * @return
     *          The DOM element     
     * @throws TransformerException
     *             If some problem occurs
     */
    public Element toDOMElement(Node node) throws TransformerException {
        // If the node is an document, return the root element
        if (node instanceof Document) {
            return ((Document) node).getDocumentElement();
            // If the node is an element, just cast it
        } else if (node instanceof Element) {
            return (Element) node;
            // Other node types are not handled
        } else {
        	String msg=MESSAGES.getString("EJB000705_Unable_to_convert_DOM_node_to_Element");
            LOG.error(msg);
            throw new TransformerException(msg);  
        }
    }

    /**
     * Create a DOM document from the given normalized message.
     * 
     * @param message
     *          The NormalizedMessage
     * @return
     *          The Document
     * 
     * @throws MessagingException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     */
    public Document toDOMDocument(NormalizedMessage message) throws MessagingException, TransformerException, ParserConfigurationException, IOException, SAXException {
        Node node = toDOMNode(message);
        return toDOMDocument(node);
    }

    /**
     * Create a DOM document from the given source.
     * 
     * @param source
     *          The Source
     * 
     * @return
     *          The DOM document
     * 
     * @throws TransformerException
     *             If some problem occurs
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws IOException
     *             If some problem occurs
     * @throws SAXException
     *             If some problem occurs
     */
    public Document toDOMDocument(Source source) throws TransformerException, ParserConfigurationException, IOException, SAXException {
        Node node = toDOMNode(source);
        return toDOMDocument(node);
    }

    /**
     * Create a DOM document from the given Node. If the node is an document,
     * just cast it, if the node is an root element, retrieve its owner element
     * or create a new document and import the node.
     * 
     * @param node
     *      The Node
     * 
     * @return
     *      The DOM document
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public Document toDOMDocument(Node node) throws ParserConfigurationException, TransformerException {
        // If the node is the document, just cast it
        if (node instanceof Document) {
            return (Document) node;
            // If the node is an element
        } else if (node instanceof Element) {
            Element elem = (Element) node;
            // If this is the root element, return its owner document
            if (elem.getOwnerDocument().getDocumentElement() == elem) {
                return elem.getOwnerDocument();
                // else, create a new doc and copy the element inside it
            } else {
                Document doc = createDocument();
                doc.appendChild(doc.importNode(node, true));
                return doc;
            }
            // other element types are not handled
        } else {
        	String msg=MESSAGES.getString("EJB000706_Unable_to_convert_DOM_node_to_Document");
            LOG.error(msg);
            throw new TransformerException(msg);
        }
    }

    // Properties
    //-------------------------------------------------------------------------
    /**
     * Gets the document builder factory.
     * 
     * @return the document builder factory
     */
    public DocumentBuilderFactory getDocumentBuilderFactory() {
        if (documentBuilderFactory == null) {
            documentBuilderFactory = createDocumentBuilderFactory();
        }
        return documentBuilderFactory;
    }

    /**
     * Sets the document builder factory.
     * 
     * @param documentBuilderFactory
     *            the new document builder factory
     */
    public void setDocumentBuilderFactory(DocumentBuilderFactory documentBuilderFactory) {
        this.documentBuilderFactory = documentBuilderFactory;
    }


    // Helper methods
    //-------------------------------------------------------------------------
    /**
     * Creates the document builder factory.
     * 
     * @return the document builder factory
     */
    public DocumentBuilderFactory createDocumentBuilderFactory() {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setIgnoringElementContentWhitespace(true);
        factory.setIgnoringComments(true);
        return factory;
    }


    /**
     * Creates the document builder.
     * 
     * @return the document builder
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     */
    public DocumentBuilder createDocumentBuilder() throws ParserConfigurationException {
        DocumentBuilderFactory factory = getDocumentBuilderFactory();
        return factory.newDocumentBuilder();
    }

    /**
     * Creates the document.
     * 
     * @return the document
     * 
     * @throws ParserConfigurationException
     *             If some problem occurs
     */
    public Document createDocument() throws ParserConfigurationException {
        DocumentBuilder builder = createDocumentBuilder();
        return builder.newDocument();
    }

    /**
     * Gets the transformer factory.
     * 
     * @return the transformer factory
     */
    public TransformerFactory getTransformerFactory() {
        if (transformerFactory == null) {
            transformerFactory = createTransformerFactory();
        }
        return transformerFactory;
    }

    /**
     * Sets the transformer factory.
     * 
     * @param transformerFactory
     *            the new transformer factory
     */
    public void setTransformerFactory(TransformerFactory transformerFactory) {
        this.transformerFactory = transformerFactory;
    }

    /**
     * Creates the transfomer.
     * 
     * @return the transformer
     * 
     * @throws TransformerConfigurationException
     *             If some problem occurs
     */
    public Transformer createTransfomer() throws TransformerConfigurationException {
        TransformerFactory factory = getTransformerFactory();
        return factory.newTransformer();
    }

    /**
     * Creates the transformer factory.
     * 
     * @return the transformer factory
     */
    public TransformerFactory createTransformerFactory() {
        TransformerFactory answer = TransformerFactory.newInstance();
        return answer;
    }

    /**
     * To XML stream reader.
     * 
     * @param source
     *          The Source
     * 
     * @return the XML stream reader
     * 
     * @throws XMLStreamException
     *             If some problem occurs
     * @throws TransformerException
     *             If some problem occurs
     */
    public XMLStreamReader toXMLStreamReader(Source source) throws XMLStreamException, TransformerException {
//      Uncomment to support StaxSource
//      if (source instanceof StaxSource) {
//      return ((StaxSource) source).getXMLStreamReader();
//      }
        // It seems that woodstox 2.9.3 throws some NPE in the servicemix-soap
        // when using DOM, so use our own dom / stax parser
        if (source instanceof DOMSource) {
            Node n = ((DOMSource) source).getNode();
            
            Element el = null;
            if (n instanceof Document) {
                el = ((Document) n).getDocumentElement();
            } else if (n instanceof Element ) {
                el =(Element) n;
            }             
            if (el != null) {
                return new W3CDOMStreamReader(el);
            }
        }
        XMLInputFactory factory = getInputFactory();
        try {
            return factory.createXMLStreamReader(source);
        } catch (XMLStreamException e) {
            return factory.createXMLStreamReader(toReaderFromSource(source));
        }
    }

    // Implementation methods
    //-------------------------------------------------------------------------
    /**
     * Creates the input factory.
     * 
     * @return the XML input factory
     */
    protected XMLInputFactory createInputFactory() {
        XMLInputFactory answer = XMLInputFactory.newInstance();
        return answer;
    }

    // Properties
    //-------------------------------------------------------------------------
    /**
     * Gets the input factory.
     * 
     * @return the input factory
     */
    public XMLInputFactory getInputFactory() {
        if (inputFactory == null) {
            inputFactory = createInputFactory();
        }
        return inputFactory;
    }    
}
