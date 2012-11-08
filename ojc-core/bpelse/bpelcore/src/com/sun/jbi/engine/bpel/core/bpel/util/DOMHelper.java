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
 * @(#)DOMHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.ibm.wsdl.MessageImpl;
import com.ibm.wsdl.PartImpl;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.xpath.dom.BPELSEDOMNodePointer;


/**
 * DOM helper
 *
 * @author Sun Microsystems
 * @version 
 */
public class DOMHelper {
    
    public static final String XMLNS_URI = "http://www.w3.org/2000/xmlns/";
    public static final String XMLNS_PREFIX = "xmlns";
    
    public static final String XMLSI_URI = "http://www.w3.org/2001/XMLSchema-instance";
    public static final String XMLSI_PREFIX = "xsi";
    	
    public static final String BASE_PREFIX = "ns";
    public static final String COLUMN = ":";

    
    private static final Logger LOGGER = Logger.getLogger(DOMHelper.class.getName());

    /**
     * Obtain the byte array from an xml element
     * @param node The element
     * @return String 
     */
    public static byte[] createXml(Element node) {
            String str = createXmlString(node);
            return str.getBytes();
    }

    /**
     * Convert the Element to a string representing XML  
     * @param node The DOM Node
     * @return The string representing XML
     */
    public static String createXmlString(Node node) {
        DOMSource source = new DOMSource(node);
        return createXmlString(source);
    }

    /**
     * Convert the Element to a string representing XML
     * 
     * @param source 
     *            The Source object
     * @return The string representing XML
     */
    public static String createXmlString(Source source) {

        try {
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);

            XmlResourceProviderPool xmlResProviderpool = getXmlResourcePool();
            XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
            Transformer transformer = xmlResourceProvider.getTransformer();

            try {
                transformer.transform(source, result);
            } catch (TransformerException ex) {
                throw ex;
            } finally {
                transformer = null;
                xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
                xmlResourceProvider = null;
            }

            String xmlString = writer.toString();
            return xmlString;

        } catch (Exception e) {
            throw new RuntimeException(
            		I18n.loc("BPCOR-6096: Error occurred while converting the node to a string."), e);
        }
    }    
    
    /**
     * Creates a DOM element from string containing the XML
     * @param xmlString String containing the XML
     * @return DOM element
     * @throws RuntimeException if cannot create DOM
     */
    public static Element createDOM(String xmlString) {
        try {
            InputSource is = new InputSource(new StringReader(xmlString));

            XmlResourceProviderPool pool = getXmlResourcePool();
            XmlResourceProvider rsrc = pool.acquireXmlResourceProvider();
            DocumentBuilder db = rsrc.getDocumentBuilder();
            Document doc = null;
            try { doc = db.parse(is); }
            finally { pool.releaseXmlResourceProvider(rsrc); }
            
            if (doc == null) {
                throw new IllegalArgumentException(I18n.loc("BPCOR-6097: xml parse error: {0}", xmlString));
            }

            return doc.getDocumentElement();
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6098: error while trying to createDOM from: {0} \n {1} '", 
            		xmlString, e.getMessage()), e);
            throw new RuntimeException(I18n.loc("BPCOR-6099: parse xml error"), e);
        }
    }
    
    public static Document readDocument(Reader reader) {
        StreamSource source = new StreamSource(reader);
        DOMResult result = new DOMResult();

        XmlResourceProviderPool xmlResProviderpool = getXmlResourcePool();
        XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
        Transformer transformer = xmlResourceProvider.getTransformer();

        try {
            transformer.transform(source, result);
        } catch (TransformerException ex) {
            throw new RuntimeException(
            		I18n.loc("BPCOR-6096: Error occurred while converting the node to a string."), ex);
        } finally {
            transformer = null;
            xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
            xmlResourceProvider = null;
        }
        Document document = (Document)result.getNode();
        return document;
    }
    /**
     * Creates a DOM document from string containing the XML
     * @param xmlString String containing the XML
     * @return DOM document
     * @throws RuntimeException if cannot create DOM
     */
    public static Document createDOMDoc(String xmlString) {
        try {
            InputSource is = new InputSource(new StringReader(xmlString));

            XmlResourceProviderPool pool = getXmlResourcePool();
            XmlResourceProvider rsrc = pool.acquireXmlResourceProvider();
            DocumentBuilder db = rsrc.getDocumentBuilder();
            Document doc = null;
            try { doc = db.parse(is); }
            finally { pool.releaseXmlResourceProvider(rsrc); }
            
            if (doc == null) {
                throw new IllegalArgumentException(I18n.loc("BPCOR-6097: xml parse error: {0}", xmlString));
            }

            return doc;
        } catch (Exception e) {
            LOGGER.log(
                Level.WARNING,
                I18n.loc("BPCOR-6098: error while trying to createDOM from: {0} \n {1} '", xmlString, e.getMessage()), 
                e);
            throw new RuntimeException(I18n.loc("BPCOR-6099: parse xml error"), e);
        }
    }

    /**
     * Gets the namespace prefix from a colonized name.
     * 
     * @param cName the colonized name
     * @return The prefix of the colonized name.  If there is no prefix
     *          (no colon) in the name, returns empty string.
     */
    public static String getPrefix(String cName) {
        int pos;
        if ((pos = cName.indexOf(':')) >= 0) {
            return cName.substring(0, pos);
        }
        return "";
    }
    
    /**
     * Gets the local name from a colonized name.
     * 
     * @param cName the colonized name
     * @return The local name of the colonized name.  If there is no prefix
     *          (no colon) in the name, returns the name itself.
     */
    public static String getLocalName(String cName) {
        int pos;
        if ((pos = cName.indexOf(':')) >= 0) {
            return cName.substring(pos + 1);
        }
        return cName;
    }
    
    /**
     * Gets an unused namespace prefix for designated element.
     * 
     * @param elem the element
     * @param index starting index to use and also returns the next
     *              index that should be used if the same element is
     *              considered.
     * @return The unused namespace prefix.
     */
    public static String getUnusedPrefix(Element elem, int[] index) {
        StringBuilder sb = new StringBuilder();
        sb.append(BASE_PREFIX);
        String prefix;
        int i = index == null || index.length == 0 ? 0 : index[0];
        while(true) {
            prefix = sb.append(i).toString();
            if (!elem.hasAttributeNS(XMLNS_URI, prefix)
                    && elem.lookupNamespaceURI(prefix) == null) {
                if (index != null && index.length > 0) {
                    index[0] = i + 1;
                }
                return prefix;
            }
            i++;
            sb.setLength(BASE_PREFIX.length());
        }
    }
    
    /**
     * Copies content (child nodes plus attributes) from one node to another.
     * 
     * @param srcNode the source node
     * @param tgtNode the target node
     */
    public static void copyContent(Node srcNode, Node tgtNode) {
        
        Document targetDoc = tgtNode.getOwnerDocument();

        //Inherits namespace declarations from the source node
        NamedNodeMap attrMap;
        Attr attrNode;
        if (tgtNode instanceof Element) {
            Map<String, String> nsMap = new HashMap<String, String>();
            Node tmpNode = srcNode;
            Element tgtElem = (Element) tgtNode;
            String attrName;
            if (BPELSEDOMNodePointer.isNodeQNameType(tgtNode)) {
                String prefixVal = getPrefix(srcNode.getTextContent());
                nodeLoop: while (tmpNode != null) {
                    attrMap = tmpNode.getAttributes();
                    if (attrMap != null) {
                        attrLoop: for (int i = 0, size = attrMap.getLength();
                                i < size; i++) {
                            attrNode = (Attr) attrMap.item(i);
                            attrName = attrNode.getName();
                            String prefix;
                            if ("xmlns".equals(attrName)) {
                                prefix = "";
                            } else if (XMLNS_URI.equals(
                                    attrNode.getNamespaceURI())) {
                                prefix = getLocalName(attrName);
                            } else {
                                continue attrLoop;
                            }
                            if (prefix.equals(prefixVal)) {
                                nsMap.put(prefix, attrNode.getValue());
                                break nodeLoop;
                            }
                        }
                    }
                    tmpNode = tmpNode.getParentNode();
                }
            } else if (BPELSEDOMNodePointer.isNodeSimpleType(tgtNode)) {
                // do nothing.
            } else {
                while (tmpNode != null) {
                    attrMap = tmpNode.getAttributes();
                    if (attrMap != null) {
                        attrLoop: for (int i = 0, size = attrMap.getLength();
                        i < size; i++) {
                            attrNode = (Attr) attrMap.item(i);
                            attrName = attrNode.getName();
                            String prefix;
                            if ("xmlns".equals(attrName)) {
                                prefix = "";
                            } else if (XMLNS_URI.equals(
                                    attrNode.getNamespaceURI())) {
                                prefix = getLocalName(attrName);
                            } else {
                                continue attrLoop;
                            }
                            if (!nsMap.containsKey(prefix)) {
                                nsMap.put(prefix, attrNode.getValue());
                            }
                        }
                    }
                    tmpNode = tmpNode.getParentNode();
                }
            }
            int index[] = new int[1];
            index[0] = 0;
            for (Entry<String, String> entry: nsMap.entrySet()) {
                declareNamespace(entry.getKey(),
                        entry.getValue(), tgtElem, index);
            }
            
            //Copies attributes if any.  Copying attributes here just in case
            //some namespace declaration attributes are falling behind normal
            //attributes.
            attrMap = srcNode.getAttributes();
            if (attrMap != null) {
                final int size = attrMap.getLength();
                for (int i = 0; i < size; i++) {
                    attrNode = (Attr) attrMap.item(i);
//                    attrName = attrNode.getName();
//                    if ("xmlns".equals(attrName)) {
//                        continue;
//                    }
					if (!isXMLNSAttribute(attrNode)) {
						String attrNS = getNameSpace(attrNode);
						tgtElem.setAttributeNS(attrNS, attrNode.getName(),
								attrNode.getValue());
					}
                }
            }
        } else if (tgtNode instanceof Attr) {
            if (BPELSEDOMNodePointer.isAttrNodeQNameType(tgtNode)) {
                // attempt to copy the prefix associated namespace only if it is a QName type attribute.
                // any other attribute (simple type) should be ignored.
                Node tmpNode = srcNode;
                String prefixVal = DOMHelper.getPrefix(srcNode.getTextContent());
                Map<String, String> nsMap = new HashMap<String, String>();
                Element tgtElem = ((Attr) tgtNode).getOwnerElement();
                String attrName;

                nodeLoop: while (tmpNode != null) {
                    attrMap = tmpNode.getAttributes();
                    if (attrMap != null) {
                        attrLoop: for (int i = 0, size = attrMap.getLength();
                        i < size; i++) {
                            attrNode = (Attr) attrMap.item(i);
                            attrName = attrNode.getName();
                            String prefix;
                            if ("xmlns".equals(attrName)) {
                                prefix = "";
                            } else if (DOMHelper.XMLNS_URI.equals(
                                    attrNode.getNamespaceURI())) {
                                prefix = DOMHelper.getLocalName(attrName);
                            } else {
                                continue attrLoop;
                            }
                            if (prefix.equals(prefixVal)) {
                                nsMap.put(prefix, attrNode.getValue());
                                break nodeLoop;
                            }
                        }
                    }
                    tmpNode = tmpNode.getParentNode();
                }
                int index[] = new int[1];
                index[0] = 0;
                for (Entry<String, String> entry: nsMap.entrySet()) {
                    DOMHelper.declareNamespace(entry.getKey(),
                            entry.getValue(), tgtElem, index);
                }            
            }
        }
        //Copies child nodes
        NodeList childNodes = srcNode.getChildNodes();
        final int childCount = childNodes.getLength();
        for (int i = 0; i < childCount; i++) {
            Node child = childNodes.item(i);
            if (child != null) {
                child = targetDoc.importNode(child, true);
                tgtNode.appendChild(child);
            }
        }
    }
    
    public static boolean isXMLNSAttribute(Attr attrNode) {
		String attrNS = attrNode.getNamespaceURI();
		String attrName = attrNode.getName();
		if ((XMLNS_URI.equals(attrNS)) || checkAttrType(attrName,XMLNS_PREFIX)) {
			return true;
		}
		return false;
	}

    public static String getNameSpace(Attr attrNode) {
		String attrNS = attrNode.getNamespaceURI();
		if (attrNS != null) {
			return attrNS;
		}
		String attrName = attrNode.getName();
		if( checkAttrType(attrName,XMLSI_PREFIX) ) {
			return XMLSI_URI;
		}
		return null;
	}
    
    
    /**
     * 
     * @param prefix : attribute name
     * @param compareWith : Compare the attribute name for xmlns and xsi
     * @return boolean
     * 		   true -> the attribute type is either xmlns or begins with xmlns: 
     * 							xsi   or begins with xsi:
     * 		   false -> not an xmlns or xsi type attribute.	
     */
    private static boolean checkAttrType(String prefix, String compareWith ){
    	return (prefix.equals(compareWith) ||
    		prefix.startsWith(compareWith + ":" ));
    }
    
    
    /**
     * Resolves namespace prefix conflict between the specified namespace
     * prefix and the namespace prefix of the specified element, and declares
     * the namespace specified.
     *  
     * @param prefix the prefix to resolve conflict for
     * @param uri the namespace URI
     * @param elem the element that needs to concede on namespace conflict.
     *              Please note that assumption is made on that the element
     *              must not have any content.
     * @param index contains the starting index used to generate the new
     *              prefix.  Also returns the next index to use if the
     *              same element is considered.
     */
    public static boolean declareNamespace(String prefix, String nsURI,
            Element elem, int[] index) {
        String elemNamePrefix =
            elem.getPrefix() == null ? "" : elem.getPrefix();
        prefix = prefix == null ? "" : prefix;
        String uri;
        if (prefix.length() == 0 ) {
            uri = elem.lookupNamespaceURI(null);
            if (uri == null || uri.length() == 0) {
                if (nsURI == null || nsURI.length() == 0) {
                    return true;
                }
                if (elemNamePrefix.length() == 0) {
                    return false;
                }
                elem.setAttributeNS(XMLNS_URI, "xmlns", nsURI);
                return true;
            }
            if (elem.isDefaultNamespace(nsURI)) {
                //the namespace to be declared is already a default namespace,
                //which means nsURI == uri.  There is no need to redeclare it.
                return true;
            }
            elem.removeAttributeNS(XMLNS_URI, "xmlns");
        } else {
            uri = elem.lookupNamespaceURI(prefix);
            if (uri == null || uri.length() == 0) {
                //The prefix hasn't been used to declare any namespace
                if (!elemNamePrefix.equals(prefix)) {
                    elem.setAttributeNS(XMLNS_URI, "xmlns:" + prefix, nsURI);
                    return true;
                }
                uri = elem.getNamespaceURI();
            } else if (nsURI != null && nsURI.equals(uri)) {
                //The prefix has been used to declare a namespace, but
                //the namespace is same as the one that needs to be declared.
                return true;
            }
            elem.removeAttributeNS(XMLNS_URI, "xmlns:" + prefix);
        }
        String newPrefix = getUnusedPrefix(elem, index);
        elem.setAttributeNS(XMLNS_URI, "xmlns:" + newPrefix, uri);
        if (elemNamePrefix.equals(prefix)) {
            elem.setPrefix(newPrefix);
        }
        if (prefix.length() == 0) {
            if (nsURI != null && nsURI.length() > 0) {
                elem.setAttributeNS(XMLNS_URI, "xmlns", nsURI);
            } else {
                if (uri != null && uri.length() > 0) {
                    elem.setAttributeNS(XMLNS_URI, "xmlns", "");
                }
            }
        } else {
            elem.setAttributeNS(XMLNS_URI, "xmlns:" + prefix, nsURI);
        }
        return true;
    }
    
    /**
     * Resolves namespace prefix conflict between the specified namespace
     * prefix and the namespace prefix of the specified element, and declares
     * the namespace specified.
     *  
     * @param prefix the prefix to resolve conflict for
     * @param uri the namespace URI
     * @param elem the element that needs to concede on namespace conflict.
     *              Please note that assumption is made on that the element
     *              must not have any content.
     * @param index contains the starting index used to generate the new
     *              prefix.  Also returns the next index to use if the
     *              same element is considered.
     */
    public static boolean declareNamespace(String prefix, String nsURI,
            Attr elem, int[] index) {
//        String elemNamePrefix =
//            elem.getPrefix() == null ? "" : elem.getPrefix();
//        prefix = prefix == null ? "" : prefix;
//        String uri;
//        if (prefix.length() == 0 ) {
//            uri = elem.lookupNamespaceURI(null);
//            if (uri == null || uri.length() == 0) {
//                if (nsURI == null || nsURI.length() == 0) {
//                    return true;
//                }
//                if (elemNamePrefix.length() == 0) {
//                    return false;
//                }
//                elem.setAttributeNS(XMLNS_URI, "xmlns", nsURI);
//                return true;
//            }
//            if (elem.isDefaultNamespace(nsURI)) {
//                //the namespace to be declared is already a default namespace,
//                //which means nsURI == uri.  There is no need to redeclare it.
//                return true;
//            }
//            elem.removeAttributeNS(XMLNS_URI, "xmlns");
//        } else {
//            uri = elem.lookupNamespaceURI(prefix);
//            if (uri == null || uri.length() == 0) {
//                //The prefix hasn't been used to declare any namespace
//                if (!elemNamePrefix.equals(prefix)) {
//                    elem.setAttributeNS(XMLNS_URI, "xmlns:" + prefix, nsURI);
//                    return true;
//                }
//                uri = elem.getNamespaceURI();
//            } else if (nsURI != null && nsURI.equals(uri)) {
//                //The prefix has been used to declare a namespace, but
//                //the namespace is same as the one that needs to be declared.
//                return true;
//            }
//            elem.removeAttributeNS(XMLNS_URI, "xmlns:" + prefix);
//        }
//        String newPrefix = getUnusedPrefix(elem, index);
//        elem.setAttributeNS(XMLNS_URI, "xmlns:" + newPrefix, uri);
//        if (elemNamePrefix.equals(prefix)) {
//            elem.setPrefix(newPrefix);
//        }
//        if (prefix.length() == 0) {
//            if (nsURI != null && nsURI.length() > 0) {
//                elem.setAttributeNS(XMLNS_URI, "xmlns", nsURI);
//            } else {
//                if (uri != null && uri.length() > 0) {
//                    elem.setAttributeNS(XMLNS_URI, "xmlns", "");
//                }
//            }
//        } else {
//            elem.setAttributeNS(XMLNS_URI, "xmlns:" + prefix, nsURI);
//        }
        return true;
    }
    
    private static XmlResourceProviderPool getXmlResourcePool() {
        return (XmlResourceProviderPool) 
                BPELSERegistry.getInstance().lookup(
                        XmlResourceProviderPool.class.getName());
    }
    
    /**
     * create wsdlmodel message
     *
     * @param messagetype mesage type
     * @param element element
     *
     * @return WSDLMessage wsdlmodel message
     */
    private static Message createMessage(QName messagetype, Element element) {
        Message message = new MessageImpl();
        message.setQName(messagetype);

        Part part = null;
        Node node = element.getFirstChild();

        while (node != null) {
            if (!(node instanceof Element)) {
                node = node.getNextSibling();

                continue;
            }

            part = new PartImpl();
            QName qName = NamespaceUtility.getQName(
            		node.getNamespaceURI(), node.getNodeName(), node.getPrefix());
            part.setElementName(qName);
            part.setName(node.getNodeName());

            message.addPart(part);
            node = node.getNextSibling();
        }

        return message;
    }

    /**
     * construct message
     *
     * @param messagetype message type
     * @param elementString element string
     *
     * @return WSMessage webservice message
     */
    public static WSMessage constructMsg (
        javax.xml.namespace.QName messageType, String elementString
    ) throws Exception {
        WSMessage domMsg = null;
        Element element = createDOMElement(elementString);

        try {
            QName qname = null;

            if ((messageType.getPrefix() != null) && !messageType.getPrefix().equals("")) { // NO I18N
            	qname = NamespaceUtility.getQName(
                        messageType.getNamespaceURI(), messageType.getLocalPart(),
                        messageType.getPrefix());            	
            } else {
                qname = NamespaceUtility.getQName(
                		messageType.getNamespaceURI(), messageType.getLocalPart());
            }

            domMsg = new JBIMessageImpl(createMessage(qname, element));

        } catch (Exception e) {
            throw new Exception(I18n.loc("BPCOR-6100: Could not create a WSMessage for the messageType {0} " + 
            		"and the xml string: {1}", messageType, elementString), e);

        }
        return domMsg;
    }

    /**
     * Creates DOM element from stringified xml string
     *
     * @param stringiXml stringified xml string
     * @return DOM element
     * @throws RuntimeException if cannot create DOM
     */
    private static Element createDOMElement(String xmlString) {
        try {
            InputSource is = new InputSource(new StringReader(xmlString));
            XmlResourceProviderPool pool = getXmlResourcePool();
            XmlResourceProvider rsrc = pool.acquireXmlResourceProvider();
            DocumentBuilder db = rsrc.getDocumentBuilder();
            Document doc = null;
            try { doc = db.parse(is); }
            finally { pool.releaseXmlResourceProvider(rsrc); }

            if (doc == null) {
                throw new IllegalArgumentException(
                		I18n.loc("BPCOR-6101: The document created by parsing the following string was null: {0}", 
                				xmlString));
            }

            return doc.getDocumentElement();
        } catch (Exception e) {
            throw new RuntimeException(I18n.loc("BPCOR-6102: Error occured while trying to create an element " + 
            		"from the following string: {0}", xmlString), e);
        }
    }
    
   public static String constructFault(String message) {
        String retVal = "<fault><reason>" + message + "</reason></fault>";

        return retVal;
    }

    public static QName parseQNameContent(Node documentNode, Node contentNode, String elName) {
        assert contentNode != null && elName != null;
        QName result = null;
        String content = contentNode.getTextContent().trim();
        int index = content.indexOf(COLUMN);                    // locate the namespace prefix
        if (index <= 0) {
            throw new RuntimeException(I18n.loc("BPCOR-7142: {0} is invalid {1} ", elName, content));
        }
        String prefix = content.substring(0, index);
        String localName = content.substring(index + 1);
        String namespaceURI = contentNode.lookupNamespaceURI(prefix);
        namespaceURI = namespaceURI == null
                ? documentNode.lookupNamespaceURI(prefix) : namespaceURI;
        if (namespaceURI == null) {
            throw new RuntimeException(I18n.loc("BPCOR-7143: Namespace prefix is not defined {0}", prefix));
        }

        result = new QName(namespaceURI, localName);
        return result;
    }

    public static String getAttributeValue(Node parentNode, String attrName) {
        String attrValue = null;
        NamedNodeMap attributes = parentNode.getAttributes();
        Node attrNode = attributes.getNamedItem(attrName);
        attrValue = attrNode != null? attrNode.getNodeValue() : null;
        return attrValue;
    }
}
