package it.imolinfo.jbi4cics.jbi.processor.wsdl11wrapper;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.servicemix.jbi.jaxp.StringSource;
import org.codehaus.xfire.util.STAXUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/*
 * WrapperUtil.java
 *
 * Created on August 12, 2005, 2:00 PM
 *
 * @author Sun Microsystems
 */
public class WrapperUtil {
	
	/**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(WrapperUtil.class);
    
    /**
     * Constants to build wsdl 1.1 wrapper, e.g. along the lines of <jbi:message
     * version="1.0" type="wsdl:input wsdl:output or wsdl:fault message
     * attribute value QNAME" name="optional name attribute from wsdl:input
     * etc." xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
     * <jbi:part> </jbi:part> <jbi:part> </jbi:part> </jbi:message>
     */
    public static final String WRAPPER_DEFAULT_NAMESPACE_PREFIX = "jbi";

    public static final String WRAPPER_DEFAULT_NAMESPACE = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";

    public static final String WRAPPER_MESSAGE = "jbi:message";

    public static final String WRAPPER_ATTRIBUTE_VERSION = "version";

    public static final String WRAPPER_ATTRIBUTE_VERSION_VALUE = "1.0";

    public static final String WRAPPER_ATTRIBUTE_TYPE = "type";

    public static final String WRAPPER_ATTRIBUTE_NAME = "name";

    public static final String WRAPPER_PART = "jbi:part";

	
	/**
	 * void constructor.
	 */
	  public WrapperUtil(){
	  }

    
    /**
     * Creates and returns a JBI message wrapper element. Does NOT add the
     * created element to the normalDoc - nor does it popluate the wrapper
     * element with a payload
     *
     * @param normalDoc
     *            The target document of the normalization
     * @param type
     *            qualified message name defined in the portmap definition for a
     *            given message
     * @param name
     *            optional name attribute defined in the portmap definition for
     *            a given message
     * @return the jbi message wrapper element
     */
    public static Element createJBIMessageWrapper(Document normalDoc,
            QName type, String name) {

        Element msgWrapper = normalDoc.createElementNS(
                WRAPPER_DEFAULT_NAMESPACE, WRAPPER_MESSAGE);
        msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_VERSION,
                WRAPPER_ATTRIBUTE_VERSION_VALUE);
        String prefix = type.getPrefix();
        if (prefix == null || prefix.length() == 0) {
            prefix = "msgns";
        }
        msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_TYPE, prefix + ":"
                + type.getLocalPart());
        msgWrapper.setAttribute("xmlns:" + prefix, type.getNamespaceURI());
        if (name != null) {
            msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_NAME, name);
        }

        return msgWrapper;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created
     * element to the normalDoc.
     *
     * @param normalDoc
     *            The target document of the normalization
     * @param part
     *            the part payload which must be created by normalDoc
     * @return wrapperElem     
     *            The craeted JBI wrapped part
     */
    public static Element createJBIWrappedPart(Document normalDoc, Node part) {
        Element wrapperElem = normalDoc.createElementNS(
                WRAPPER_DEFAULT_NAMESPACE, WRAPPER_PART);
        if (part != null) {
            wrapperElem.appendChild(part);
        }
        return wrapperElem;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created
     * element to the normalDoc.
     *
     * @param normalDoc
     *            The target document of the normalization
     * @param part
     *            the part payload which must be created by normalDoc
     * @return wrapperElem
     *            the created JBI wrapped part
     */
    public static Element createJBIWrappedPart(Document normalDoc, NodeList part) {
        Element wrapperElem = normalDoc.createElementNS(
                WRAPPER_DEFAULT_NAMESPACE, WRAPPER_PART);
        if (part != null) {
            int noOfNodes = part.getLength();
            for (int nodeCount = 0; nodeCount < noOfNodes; nodeCount++) {
                wrapperElem.appendChild(part.item(nodeCount));
            }
        }
        return wrapperElem;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created
     * element to the normalDoc.
     *
     * @param normalDoc
     *            The target document of the normalization
     * @param part
     *            the part payload which need not be created by normalDoc
     * @return wrapperElem
     *            the created JBI wrapped part
     */
    public static Element importJBIWrappedPart(Document normalDoc, Node part) {
        Element wrapperElem = normalDoc.createElementNS(
                WRAPPER_DEFAULT_NAMESPACE, WRAPPER_PART);
        if (part != null) {
            Node importedPartNode = normalDoc.importNode(part, true);
            wrapperElem.appendChild(importedPartNode);
        }
        return wrapperElem;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created
     * element to the normalDoc.
     *
     * @param normalDoc
     *            The target document of the normalization
     * @param part
     *            the part payload which need not be created by normalDoc
     * @return wrapperElem
     *            the created JBI wrapped part
     */
    public static Element importJBIWrappedPart(Document normalDoc, NodeList part) {
        Element wrapperElem = normalDoc.createElementNS(
                WRAPPER_DEFAULT_NAMESPACE, WRAPPER_PART);
        if (part != null) {
            int noOfNodes = part.getLength();
            for (int nodeCount = 0; nodeCount < noOfNodes; nodeCount++) {
                Node aNode = part.item(nodeCount);
                if (aNode != null) {
                    Node importedPartNode = normalDoc.importNode(aNode, true);
                    wrapperElem.appendChild(importedPartNode);
                }
            }
        }
        return wrapperElem;
    }

    /**
     * @param doc
     *            the message to determine whether it has a normalized WSDL 1.1
     *            wrapper
     * @return true if the message passed in has a WSDL 1.1 wrapper element,
     *         false if it does not
     */
    public static boolean isMessageWrapped(Document doc) {
        boolean wrapperDetected = false;
        if (doc != null) {
            Element jbiMessageWrapper = doc.getDocumentElement();
            if (jbiMessageWrapper != null) {
                String nsURI = jbiMessageWrapper.getNamespaceURI();
                if (nsURI != null && nsURI.equals(WRAPPER_DEFAULT_NAMESPACE)) {
                    wrapperDetected = true;
                }
            }
        }
        return wrapperDetected;
    }

    /**
     * Gets a JBI wrapper. See JBI specs, 5.5.1.1.4
     * TODO :CLEAN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     *
     * @param msgContent
     *            the content message
     * @param qName
     *            the message namespace
     * @param name
     *            the message name
     * @throws WrapperProcessingException 
     *            the wrapper processing exception
     * @return ret
     *            the created JBI wrapped message
     */
    public static Source jbiMessageWrapper(String msgContent, QName qName,
            String name) throws WrapperProcessingException {

        if (qName == null) {
            throw new WrapperProcessingException("messageqName is null");
        }

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder mBuilder;
        try {
            factory.isNamespaceAware();
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        }

        Document normalDoc = mBuilder.newDocument();
        Element normalRoot = null;

        // Create the jbi message wrapper
        Element jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(
                normalDoc, qName, name);
        normalDoc.appendChild(jbiMessageWrapper);

        Document msgDoc;
        try {
            // TODO: There must be a better way...
            msgDoc = mBuilder.parse(new InputSource(new java.io.StringReader(
                    msgContent)));

        } catch (IOException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        } catch (SAXException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        }

        Element jbiMessagePart = WrapperUtil.importJBIWrappedPart(normalDoc,
                msgDoc.getDocumentElement());
        jbiMessageWrapper.appendChild(jbiMessagePart);

        // TODO: To test with more than one part.
//                if (partDef.getElement() != null) {
//                    Element wrapperElem = WrapperUtil.importJBIWrappedPart(
//                            jbiWMDoc, doc.getDocumentElement());
//                    jbiWMDoc.getDocumentElement().appendChild(wrapperElem);
//                } else {
//                    Element wrapperElem = WrapperUtil.importJBIWrappedPart(
//                            jbiWMDoc, doc.getDocumentElement().getChildNodes());
//                    jbiWMDoc.getDocumentElement().appendChild(wrapperElem);
//                }

        try {
            LOG.debug(toXml(normalDoc, "UTF-8", false));
        } catch (Exception ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        }

        return new StringSource(toXml(normalDoc, "UTF-8", false));
    }

    /**
     * Converts the xml documet to a String. Useful for logging.
     *
     * @param node    The node
     * @param encoding    The encoding
     * @param omitXMLDeclaration   The omit XML declaration
     * @throws WrapperProcessingException    The wrapper processing exception
     * @return ret
     *            the created string to Xml
     */
    public static String toXml(Node node, String encoding,
            boolean omitXMLDeclaration) throws WrapperProcessingException {
        String ret = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Transformer trans;
        try {
            trans = TransformerFactory.newInstance().newTransformer();

            trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty(
                    "{http://xml.apache.org/xslt}indent-amount", "4");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
                    omitXMLDeclaration ? "yes" : "no");
            trans.transform(new DOMSource(node), new StreamResult(baos));
            ret = baos.toString(encoding);
        } catch (TransformerException ex) {
            LOG.warn(ex.getMessage(), ex);
        } catch (UnsupportedEncodingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        } catch (TransformerFactoryConfigurationError ex) {
            LOG.error(ex.getMessage(), ex);
            new WrapperProcessingException(ex.getMessage(), ex);
        }
        return ret;
    }

    /**
     * @param docReader    the message to determine whether it has a normalized WSDL 1.1
     *                     wrapper
     * @return    true if the message passed in has a WSDL 1.1 wrapper element,
     *            false if it does not
     * @throws WrapperProcessingException    The wrapper processing exception
     */
    public static boolean isMessageWrapped(XMLStreamReader docReader)
    throws WrapperProcessingException {
        boolean wrapperDetected = false;
        if (docReader != null) {
            try {
                String nsURI = docReader.getNamespaceURI();
                if (nsURI != null && nsURI.equals(WRAPPER_DEFAULT_NAMESPACE)) {
                    wrapperDetected = true;
                }
            } catch (java.lang.IllegalStateException illex) {
                LOG.warn("CIC001201_IO_exception", new Object[] {illex.getMessage()}, illex);
            }
        }
        return wrapperDetected;
    }

    /**
     * UnWraps a normalized WSDL 1.1 message
     * TODO: Verify message parts: there must be a part element for each message part,
     * "All logical parts of the message must appear here, in the same order as defined in the WSDL 1.1 message
     * description referred to by the type attribute" (jbi specs - 5.5.1.1.4)
     *
     * TODO :CLEAN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     *
     * @param docReader
     *            the message to determine whether it has a
     *            wrapper
     * @return true if the message passed in has a WSDL 1.1 wrapper element,
     *         false if it does not
     * @throws WrapperProcessingException The warepper process exception
     */
    public static XMLStreamReader unWrapMessage(XMLStreamReader docReader)
    throws WrapperProcessingException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder mBuilder;

        try {
            factory.isNamespaceAware();
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            ex.printStackTrace();
            throw new WrapperProcessingException(ex);
        }
        Document wrappedDoc = mBuilder.newDocument();
        try {
            STAXUtils.readDocElements(wrappedDoc, docReader, true);
        } catch (XMLStreamException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new WrapperProcessingException(ex.getMessage(), ex);
        }

        Document returnedDoc = mBuilder.newDocument();

        // Get the jbi message wrapper element
        Element jbiMessageWrapper = wrappedDoc.getDocumentElement();

        //  Gets the wrapeed part
        NodeList childNodes = jbiMessageWrapper.getChildNodes();

        // Extract all JBI part wrapped elements
        for (int childCount = 0; childCount < childNodes.getLength(); childCount++) {
            Node currNode = childNodes.item(childCount);
            if (currNode.getNodeType() == Node.ELEMENT_NODE) {
                Element jbiPartWrapper = (Element) currNode;
                NodeList partChildNodes = jbiPartWrapper.getChildNodes();
                for (int i = 0; i < partChildNodes.getLength(); i++) {
                    Node partChildNode = (Node) partChildNodes.item(i);
                    Node importedElement = returnedDoc.importNode(partChildNode, true);
                    returnedDoc.appendChild(importedElement);
                }
            }
        }

        // Logs the unwrapped message
        try {
            LOG.debug(toXml(returnedDoc, "UTF-8", false));
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new WrapperProcessingException(ex);
        }
        XMLInputFactory saxFactory = XMLInputFactory.newInstance();
        XMLStreamReader unwrappedReader;
        try {
            // Temporary...
            String xml = toXml(returnedDoc, "UTF-8", false);
            unwrappedReader = saxFactory.createXMLStreamReader(new StringReader(xml));
        } catch (XMLStreamException ex) {
            ex.printStackTrace();
            throw new WrapperProcessingException(ex);
        }

        return unwrappedReader;
    }

}
