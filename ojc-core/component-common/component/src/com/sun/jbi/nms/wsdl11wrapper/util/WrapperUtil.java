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

package com.sun.jbi.nms.wsdl11wrapper.util;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.xml.namespace.QName;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;


/*
 * WrapperUtil.java
 *
 * Created on August 12, 2005, 2:00 PM
 *
 * @author Sun Microsystems
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
    
    private static final String XOP_NAMESPACE = "http://www.w3.org/2004/08/xop/include";
    private static final String XOP_INCLUDE_NODE_NAME = "Include";
    private static final String XOP_CONTENT_ID_SUFFIX = "org.glassfish.openesb.com";

    /**
     * Creates and returns a JBI message wrapper element. Does NOT add the created element to the normalDoc
     * - nor does it popluate the wrapper element with a payload
     * @param normalDoc The target document of the normalization
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
        msgWrapper.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:" + prefix, type.getNamespaceURI());
        if (name != null) {
            msgWrapper.setAttribute(WRAPPER_ATTRIBUTE_NAME, name);
        }

        return  msgWrapper;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created element to the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param partName the name of the part
     * @param part the part payload which must be created by normalDoc
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
     * Creates and returns a JBI part wrapper element. Does NOT add the created element to the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param partName the name of the part
     * @param part the part payload which must be created by normalDoc
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
     * Returns a map of namespace attributes declared on the specified element,
     * where keys in the map will include the &quot;xmlns:&quot; qualifier.
     * 
     * @param elem The element to cull for namespace declarations.
     * @return a map of namespace declarations, which will never be <code>null</code>.
     */
    public static Map extractNamespaceDeclarations(Element elem) {
    	Map<String, String> map = new HashMap<String, String>();
    	if (elem != null) {
    		NamedNodeMap attr = elem.getAttributes();
    		for (int i = 0, len = attr.getLength(); i < len; i++) {
    			Node n = attr.item(i);
    			if (n instanceof Attr) {
    				Attr a = (Attr) n;
    				String name = a.getName();
    				if (name.startsWith("xmlns:")) {
    					map.put(name, a.getValue());
    				}
    			}
    		}
    	}
    	
    	return map;
    }
    
    /**
     * Returns a map of attributes declared on the specified element,
     * where keys in the map will include the &quot;xmlns:&quot; qualifier.
     * Note this method will not extract any namespace definitions.
     * 
     * @param elem The element to cull for attribute declarations.
     * @return a map of attribute declarations, which will never be <code>null</code>.
     */
    public static Map extractAttributeDeclarations(Element elem) {
    	Map<String, Attr> map = new HashMap<String, Attr>();
    	if (elem != null) {
    		NamedNodeMap attr = elem.getAttributes();
    		for (int i = 0, len = attr.getLength(); i < len; i++) {
    			Node n = attr.item(i);
    			if (n instanceof Attr) {
    			    Attr a = (Attr) n;
    			    String name = a.getName();
    			    if (! ( name.startsWith("xmlns:") || name.equalsIgnoreCase("xmlns") )) {
    			        map.put(name, a);
    			    }
    			}
    		}
    	}
    	
    	return map;
    }
    
    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created element to the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param partName the name of the part
     * @param part the part payload which need not be created by normalDoc
     */
    public static Element importJBIWrappedPart(Document normalDoc, NodeList part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
        if (part != null) {
            int noOfNodes = part.getLength();
            for (int nodeCount = 0; nodeCount < noOfNodes; nodeCount++) {
                Node aNode = part.item(nodeCount - (noOfNodes - part.getLength()));
                if (aNode != null) {
                    // Only import the node if it's coming from a different doc
                    if (!aNode.getOwnerDocument().isSameNode(normalDoc))
                    {
                        aNode = normalDoc.importNode(aNode, true);
                    }
                    wrapperElem.appendChild(aNode);
                }
            }
        }
        return wrapperElem;
    }

    /**
     * Creates and returns a JBI part wrapper element. Does NOT add the created element to the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param partName the name of the part
     * @param part the part payload which need not be created by normalDoc
     */
    public static Element importJBIWrappedPart(Document normalDoc, Node part) {
        Element wrapperElem = normalDoc.createElementNS(WRAPPER_DEFAULT_NAMESPACE, 
                                                        WRAPPER_PART);
        if (part != null) {
            // Only import the node if it's coming from a different doc
            if (!part.getOwnerDocument().isSameNode(normalDoc))
            {
                part = normalDoc.importNode(part, true);
            }
            wrapperElem.appendChild(part);
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
    public static Element getPartElement(Document wrappedDoc) throws WrapperProcessingException {
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
                    if (nodeJ.getNodeType() == Node.ELEMENT_NODE) {
                        return (Element)nodeJ;
                    }
                }
            }
        }
        return null;
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
    public static Element getPartElement(Element jbiMessageWrapper) throws WrapperProcessingException {
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
                    if (nodeJ.getNodeType() == Node.ELEMENT_NODE) {
                        return (Element)nodeJ;
                    }
                }
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
     * @throws WrapperProcessingException if the part could not be returned
     */
    public static List getPartTextList(Document wrappedDoc) throws WrapperProcessingException {
        List list = new ArrayList();
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

    /**
     * @param doc the message to determine whether it has a normalized WSDL 1.1 wrapper
     * @return true if the message passed in has a WSDL 1.1 wrapper
     * element, false if it does not
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
     * @param node the unwrapper part node to determine whether or not it is an xop:Include 
     * @return true if the node is an xop:Include
     * element, false if it is not
     */    
    public static String createXopCid() {
        return "cid: " + UUID.randomUUID() + "@" + XOP_CONTENT_ID_SUFFIX;        
    }
    
    /**
     * @param node the unwrapper part node to determine whether or not it is an xop:Include 
     * @return true if the node is an xop:Include
     * element, false if it is not
     */    
    public static boolean isNodeXopInclude(Node node) {
        boolean isXop = false;
        if (node != null &&
            XOP_NAMESPACE.equals(node.getNamespaceURI()) &&
            XOP_INCLUDE_NODE_NAME.equals(node.getLocalName())) {
                
            // make sure the required "href" attribute is there
            NamedNodeMap attrMap = node.getAttributes();
            // NOTE: we are enforcing the validation of the href value to
            // comply with the CID URI scheme here per http://www.w3.org/TR/xop10/#RFC2392
            if (attrMap.getNamedItem("href") != null) {
                return true;
            }
        }
        return isXop;
    }
            
    
    /**
     * @param node the xop:Include node
     * @return the content ID URI defined in the "href" attribute
     * element, false if it is not
     */    
    public static String getXopContentId(Node node) {
        String contentId = null;
        
        // make sure the required "href" attribute is there
        NamedNodeMap attrMap = node.getAttributes();
        Node hrefAttr = attrMap.getNamedItem("href");
        if (hrefAttr != null) {
            String hrefAttrValue = hrefAttr.getNodeValue();
            // NOTE: we are enforcing the validation of the href value to
            // comply with the CID URI scheme here per http://www.w3.org/TR/xop10/#RFC2392
            return hrefAttr.getNodeValue();
        }
        
        return contentId;
    }
}
