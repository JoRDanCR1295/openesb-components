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
 * @(#)WrapperUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.messaging;

import java.util.ArrayList;
import java.util.List;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * Utility to construct and parse JBI WSDL 1.1 message wrapper element.
 */
public class WrapperUtil {

    /**
     * Constants to build wsdl 1.1 wrapper, e.g. along the lines of
     *   <jbi:message version="1.0" type="wsdl:input wsdl:output or wsdl:fault message attribute value QNAME" name="optional name attribute from wsdl:input etc." xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
     *       <jbi:part>
     *       </jbi:part>
     *       <jbi:part>
     *       </jbi:part>
     *   </jbi:message>
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
     * Creates and returns a JBI message wrapper element. Does NOT popluate the 
     * wrapper element with a payload.
     * 
     * @param normalDoc The document used as factory.
     * @param type qualified message name defined in the portmap definition for a given message
     * @param name optional name attribute defined in the portmap definition for a given message
     * @return the jbi message wrapper element
     */
    public static Element createJBIMessageWrapper(Document normalDoc, QName type, String name) {
        Element msgWrapper = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, WRAPPER_MESSAGE);
        msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_VERSION, WRAPPER_ATTRIBUTE_VERSION_VALUE);
        String prefix = type.getPrefix();
        if (prefix == null || prefix.length() == 0) {
            prefix = "msgns";
        }
        msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_TYPE, prefix + ":" + type.getLocalPart());
        msgWrapper.setAttribute("xmlns:" + prefix, type.getNamespaceURI());
        if (name != null) {
            msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_NAME, name);
        }
        normalDoc.appendChild(msgWrapper);
        return  msgWrapper;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created 
     * part element to the normalDoc, but does add the nodes in the specified
     * list to the created part element.
     * 
     * @param normalDoc The document used as factory.
     * @param part the part payload which must be created by normalDoc.
     */
    public static Element createJBIWrappedPart(Document normalDoc, NodeList part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
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
     * part element to the normalDoc, but does add the specified node to the
     * created part element.
     * 
     * @param normalDoc The document used as factory.
     * @param part the part payload which must be created by normalDoc.
     */
    public static Element createJBIWrappedPart(Document normalDoc, Node part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
        if (part != null) {
            wrapperElem.appendChild(part);
        }
        return wrapperElem;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created element to the normalDoc.
     * @param normalDoc The document used as factory.
     * @param part the part payload which need not be created by normalDoc.
     */
    public static Element importJBIWrappedPart(Document normalDoc, NodeList part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
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
     * Creates and returns a JBI part wrapper element by importing the specified
     * node into the specified document.
     * @param normalDoc The document used as factory.
     * @param part the part payload which need not be created by normalDoc
     */
    public static Element importJBIWrappedPart(Document normalDoc, Node part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
        if (part != null) {
            Node importedPartNode = normalDoc.importNode(part, true);
            wrapperElem.appendChild(importedPartNode);
         }
        return wrapperElem;
    }

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param wrappedDoc the wrapped document
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
    public static Element getPartElement(Document wrappedDoc) {
        // Get the jbi message wrapper element
        return getPartElement(wrappedDoc.getDocumentElement(), 0);
    }

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param wrappedDoc the wrapped document
     * @param index The zero-based index of the part to get.
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
    public static Element getPartElement(Document wrappedDoc, int index) {
    	return getPartElement(wrappedDoc.getDocumentElement(), index);
    }

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param jbiMessageWrapper the jbi message wrapper element
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
    public static Element getPartElement(Element jbiMessageWrapper) {//throws WrapperProcessingException {
    	return getPartElement(jbiMessageWrapper, 0);
    }

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param jbiMessageWrapper the jbi message wrapper element
     * @param index The zero-based index of the part to get.
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
    public static Element getPartElement(Element jbiMessageWrapper, int index) {
        if (jbiMessageWrapper.getTagName().equalsIgnoreCase(WRAPPER_MESSAGE)) {
            // The JBI wsdl 1.1 wrapper does not contain part names, but all parts
            // have to appear in the exact order defined in the WSDL
        	int partIndex = -1;
            NodeList childNodesI = jbiMessageWrapper.getChildNodes();
            for (int i = 0, I = childNodesI.getLength(); i < I; i++) {
                Node nodeI = childNodesI.item(i);
                if (nodeI.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }
                Element jbiPartWrapper = (Element) nodeI;
                if (!jbiPartWrapper.getTagName().equalsIgnoreCase(WRAPPER_PART)) {
                    continue;
                }
                else {
                	partIndex += 1;
                	if (partIndex != index) {
                		continue;
                	}
                }
                return unwrapPartElement(jbiPartWrapper);
            }
        }
        return null;
    }

    /**
     * Returns a List that contains only the first text node inside each part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param wrappedDoc the wrapped document
     * @return a List containing the first text node in each normalized message part, empty list if no element is present
     */
    public static List getPartTextList(Document wrappedDoc) {
        List<String> list = new ArrayList<String>();
        // Get the jbi message wrapper element
        Element jbiMessageWrapper = wrappedDoc.getDocumentElement();
        if (jbiMessageWrapper.getTagName().equalsIgnoreCase(WRAPPER_MESSAGE)) {
            // The JBI wsdl 1.1 wrapper does not contain part names, but all parts
            // have to appear in the exact order defined in the WSDL
            NodeList childNodesI = jbiMessageWrapper.getChildNodes();
            for (int i = 0, I = childNodesI.getLength(); i < I; i++) {
                Node nodeI = childNodesI.item(i);
                if (nodeI.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }
                Element jbiPartWrapper = (Element) nodeI;
                if (!jbiPartWrapper.getTagName().equalsIgnoreCase(WRAPPER_PART)) {
                    continue;
                }
                NodeList childNodesJ = jbiPartWrapper.getChildNodes();
                for (int j = 0, J = childNodesJ.getLength(); j < J; j++) {
                    Node nodeJ = childNodesJ.item(j);
                    if (nodeJ.getNodeType() == Node.TEXT_NODE) {
                        list.add (nodeJ.getTextContent());
                        break;
                    }
                }
            }
        }
        return list;
    }
    
    public static QName getMessageType(NormalizedMessage nmsg) throws Exception {
        if (nmsg != null) {
            DOMSource src = XmlUtil.toDOMSource(nmsg.getContent());
            Node node = src.getNode();
            Element elem = (node instanceof Element) ? (Element) node
                    : ((node instanceof Document) 
                            ? ((Document) node).getDocumentElement() : null);
            if (elem != null) {
                // elem is expected to be jbi:message
                String attr = elem.getAttribute(WRAPPER_ATTRIBUTE_TYPE);
                int ix = attr.indexOf(":");
                if (ix > 0) {
                    String prefix = attr.substring(0, ix),
                           uri = elem.lookupNamespaceURI(prefix);
                    return new QName(uri, attr.substring(ix + 1), prefix);
                }
            }
        }
        
        return null;
    }

    /**
     * @param doc the message to determine whether it has a normalized WSDL 1.1 wrapper
     * @return true if the message passed in has a WSDL 1.1 wrapper
     * element, false if it does not
     */     
    public static boolean isMessageWrapped(Document doc) {
        if (doc != null) {
            Element jbiMessageWrapper = doc.getDocumentElement();
            if (jbiMessageWrapper != null) {
                String nsURI = jbiMessageWrapper.getNamespaceURI();
                if (nsURI != null && nsURI.equals(WRAPPER_DEFAULT_NAMESPACE)) {
                    return true;
                }
            }
        }
        return false;
    }
    
    /**
     * Evaluates an element to determine if it represents a jbi:part.
     * @param elem The element to evaluate whether it's a normalized WSDL 1.1 wrapper.
     * @return <code>true</code> if the message passed in has a WSDL 1.1 wrapper,
     *         else <code>false</code>.
     */     
    public static boolean isPartWrapped(Element elem) {
        if (elem != null && elem.getTagName().equals(WRAPPER_PART)) {
            String nsURI = elem.getNamespaceURI();
            if (nsURI != null && nsURI.equals(WRAPPER_DEFAULT_NAMESPACE)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Extracts a single element from a wrapped jbi:part element.
     * @param elem The jbi:part element.
     * @return The first child element of the jbi:part or <code>null</code>.
     */
    public static Element unwrapPartElement(Element elem) {
        if (elem != null && elem.getTagName().equals(WRAPPER_PART)) {
            String nsURI = elem.getNamespaceURI();
            if (Util.equals(nsURI, WRAPPER_DEFAULT_NAMESPACE)) {
                NodeList childNodesJ = elem.getChildNodes();
                for (int j = 0, J = childNodesJ.getLength(); j < J; j++) {
                    Node nodeJ = childNodesJ.item(j);
                    if (nodeJ.getNodeType() == Node.ELEMENT_NODE) {
                        return (Element) nodeJ;
                    }
                }

            }
        }
        return null;
    }
}
