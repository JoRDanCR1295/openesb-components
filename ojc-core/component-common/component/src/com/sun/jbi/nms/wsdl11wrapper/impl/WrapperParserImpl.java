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
 * @(#)WrapperParserImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.wsdl.Definition;
import javax.wsdl.Import;
import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.util.Iterator;

/**
 * Assists in processing the WSDL 1.1 wrapper for normalized messages specified by JBI
 *
 * @author Sun Microsystems
 */
public class WrapperParserImpl implements WrapperParser {

    QName messageType;
    String messageName;
    Map wrappedParts = new HashMap();
    List partsOrder;
    boolean wrapperDetected;

    /** Creates a new instance */
    public WrapperParserImpl() {
    }

    /**
     * Parse a normalized message document in JBI WSDL 1.1 wrapper format
     * @param wrappedDoc the wrapped normalized message document
     * @param wsdlDefinition the full wsdl definition, including the definition for the normalized message
     * @throws WrapperProcessingException if there is an issue parsing the normalized message,
     * e.g. if the normalized message could not be found in the WSDL
     */
    public void parse(Document wrappedDoc, Definition wsdlDefinition) throws WrapperProcessingException {

        wrapperDetected = WrapperUtil.isMessageWrapped(wrappedDoc);
        
        Message wsdlMessageDefinition = null;
        if (wrapperDetected) {
            QName wrapperMessageType = extractMessageType(wrappedDoc, wsdlDefinition);
            wsdlMessageDefinition = wsdlDefinition.getMessage(wrapperMessageType);
            if (wsdlMessageDefinition == null) {
                wsdlMessageDefinition = getMessageFromImport(wsdlDefinition, wrapperMessageType, new ArrayList());
                if (wsdlMessageDefinition == null) {
                    throw new WrapperProcessingException("The WSDL definition provided does not contain a definition for the normalized message " + wrapperMessageType.toString());
                }
            }
        }
        
        parse(wrappedDoc, wsdlMessageDefinition);
    }
    
    /**
     * Parse a normalized message document from the imported wsdls recursively.
     * This method is added as the wsdl4j wsdlDefinition.getMessage(..) method only looks for the message in wsdl and first level imports.
     * @param wsdlDefinition the full wsdl definition, including the definition for the normalized message
     * @param wrapperMessageType the message type
     * @param parsedImports Cached list of already parsed imports to avoid cyclic loop.
     */
    private Message getMessageFromImport(Definition wsdlDefinition, QName wrapperMessageType, List parsedImports) {
        Message wsdlMessageDefinition = null;
        Map importmap = null;
        Set importset = null;
        if (wsdlDefinition != null) {
            importmap = wsdlDefinition.getImports();
        }
        if (importmap != null) {
            importset = importmap.keySet();
        }
        if (importset != null && importset.size() > 0) {
            Iterator it = importset.iterator();
            while (wsdlMessageDefinition == null && it.hasNext()) {
                Collection imports = (Collection) importmap.get(it.next());
                for (Object item : imports) {
                    Import importitem = (Import) item;
                    if (parsedImports.contains(item)) // already parsed
                    {
                        continue;
                    }
                    parsedImports.add(item);
                    wsdlMessageDefinition = importitem.getDefinition().getMessage(wrapperMessageType);
                    if (wsdlMessageDefinition != null) {
                        return wsdlMessageDefinition;
                    }
                    wsdlMessageDefinition = getMessageFromImport(importitem.getDefinition(), wrapperMessageType, parsedImports);
                }
            }
        }
//    	while(wsdlMessageDefinition == null && importset != null && importset.iterator().hasNext()) {
//    		Collection imports =(Collection)importmap.get(importset.iterator().next());
//        	for(Object item:imports){
//        		Import importitem = (Import)item;
//        		if(parsedImports.contains(item)) // already parsed
//        			continue;
//        		parsedImports.add(item);
//        		wsdlMessageDefinition = importitem.getDefinition().getMessage(wrapperMessageType);
//        		if(wsdlMessageDefinition != null)
//        			return wsdlMessageDefinition;
//        		wsdlMessageDefinition = getMessageFromImport(importitem.getDefinition(),wrapperMessageType,parsedImports);
//        	}
//    	}
        return wsdlMessageDefinition;
    }

    /**
     * Parse a normalized message document in JBI WSDL 1.1 wrapper format
     * @param wrappedDoc the wrapped normalized message document
     * @param wsdlMessageDefinition the wsdl message definition for the normalized message
     * @throws WrapperProcessingException if there is an issue parsing the normalized message,
     * e.g. if the normalized message does not match the WSDL description
     */
    public void parse(Document wrappedDoc, Message wsdlMessageDefinition) throws WrapperProcessingException {

        // Re-set all internal state
        wrappedParts.clear();
        messageType = null;
        messageName = null;
        wrapperDetected = WrapperUtil.isMessageWrapped(wrappedDoc);        
        
        if (wrapperDetected) {
            // Get the jbi message wrapper element
            Element jbiMessageWrapper = wrappedDoc.getDocumentElement();
            QName unresolvedMessageType = extractMessageType(wrappedDoc, null);
            // Check that wsdl def passed is for this wrapped message
            QName passedInDefQName = wsdlMessageDefinition.getQName();
            if (!passedInDefQName.getLocalPart().equals(unresolvedMessageType.getLocalPart())) {
                throw new WrapperProcessingException("The WSDL message definition " + passedInDefQName.getLocalPart()
                        + " does not match the name of the message wrapper of the normalized message (" + unresolvedMessageType.getLocalPart() + ")");
            } else {
                messageType = passedInDefQName;
            }
            messageName = extractMessageName(wrappedDoc);

            // Get the order of parts
            WSDLInfo info = WSDLInfo.getInstance(wsdlMessageDefinition);
            partsOrder = info.getPartsOrder();

            // The JBI wsdl 1.1 wrapper does not contain part names, but all parts
            // have to appear in the exact order defined in the WSDL
            NodeList childNodes = jbiMessageWrapper.getChildNodes();

            // Extract all JBI part wrapped elements
            for (int childCount = 0; childCount < childNodes.getLength(); childCount++) {
                Node currNode = childNodes.item(childCount);
                if (currNode.getNodeType() == Node.ELEMENT_NODE) {
                    Element jbiPartWrapper = (Element) currNode;
                    int partPos = wrappedParts.size();
                    if (partPos >= partsOrder.size()) {
                        throw new WrapperProcessingException("The normalized message contains more parts than are defined in the WSDL message definition");
                    }
                    String partName = (String) partsOrder.get(partPos);
                    wrappedParts.put(partName, jbiPartWrapper);
                }
            }

            if (partsOrder.size() > wrappedParts.size()) {
                throw new WrapperProcessingException("The normalized message contains less parts than are defined in the WSDL message definition");
            }
        }
    }

    /**
     * Note that the spec mandates that all parts present in the WSDL message definition
     * have to appear in the wrapped normalized message - as such this method has limited usefulness
     *
     * @param partName the name of the part to check
     * @return true if the parsed document contains an entry for the given part - even if the part payload itself is empty.
     * false if the part is not present
     */
    public boolean hasPart(String partName){
        return wrappedParts.containsKey(partName);
    }

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
//TODO: is this really needed/useful?
/*
     public Element getPartElement(String partName) throws WrapperProcessingException {
        Element firstNode = null;
        NodeList allNodes = getPartNodes(partName);
        if (allNodes != null && allNodes.size() > 0) {
            if (aNode.getNodeType() == Node.ELEMENT_NODE || aNode.getNodeType() == Node.TEXT_NODE) {

            firstNode = (Element) allElements.get(0);
        }
        return firstNode;
    }
 */

    /**
     * Returns all Element inside the part wrapper
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return all Elements in the normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
/*
    public List getPartElements(String partName) throws WrapperProcessingException {
        Element wrappedPart = (Element) wrappedParts.get(partName);
        if (wrappedPart == null) {
            throw new WrapperProcessingException("The parsed message does not contain part " + partName);
        }
        List allElements = removeJBIPartWrapper(wrappedPart);
        return allElements;
    }
*/
    /**
     * Returns all Nodes inside the part wrapper
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return all Elements in the normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
    public NodeList getPartNodes(String partName) throws WrapperProcessingException {
        Element wrappedPart = (Element) wrappedParts.get(partName);
        if (wrappedPart == null) {
            throw new WrapperProcessingException("The parsed message does not contain part " + partName);
        }
        NodeList allElements = removeJBIPartWrapper(wrappedPart);
        return allElements;
    }


    /**
     * Returns the part, including the <jbi:part> part wrapper element around
     * the part 'payload'
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return the wrapped normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
    public Element getWrappedPart(String partName) throws WrapperProcessingException {
        Element wrappedPart = (Element) wrappedParts.get(partName);
        if (wrappedPart == null) {
            throw new WrapperProcessingException( "The parsed message does not contain part " + partName );
        }
        return wrappedPart;
    }

    /**
     * @return the number of parts in the normalized message
     */
    public int getNoOfParts() {
        return wrappedParts.size();
    }

    /**
     * @return the names of the parts in the normalized message
     */
    public String[] getPartNames() {
        return (String[]) partsOrder.toArray(new String[0]);
    }

    /**
     * Given the element for a jbi part wrapper, removes the wrapper element
     * and returns the part payload
     * @param wrappedPart the Element representing the JBI part wrapper
     * @return the part payload as a List of Element s
     */
    public static NodeList removeJBIPartWrapper(Element wrappedPart) {
//        List elements = new ArrayList();
        NodeList childNodes = wrappedPart.getChildNodes();

/*
        for (int childCount = 0; childCount < childNodes.getLength(); childCount++) {
            Node aNode = (Node) childNodes.item(childCount);
            if (aNode != null) {
                if (aNode.getNodeType() == Node.ELEMENT_NODE || aNode.getNodeType() == Node.TEXT_NODE) {
                    elements.add(aNode);
                }
            }
        }
*/

        return childNodes;
    }

    /**
     * Get the message type of the message wrapper in the wrapped document.
     * @return the message type as QName
     */
    public QName getMessageType() throws WrapperProcessingException {
        return messageType;
    }

    /**
     * Get the optional message "name" defined in the wrapped document.
     * This is the logical name defined in the operation binding, not the type name.
     * @return the message name
     */
    public String getMessageName() throws WrapperProcessingException {
        return messageName;
    }

    /**
     * @return true if the message passed to the parse() methods has a WSDL 1.1 wrapper
     * element, false if it does not
     */ 
    public boolean isMessageWrapped() {
        return wrapperDetected;
    }
    
    /**
     * Get the message type of the message wrapper in the wrapped document.
     * If the wsdlDefinition passed in does not contain a namespace definition for the
     * type prefix present int the message wrapper the QName namespace will be null
     * @param wrappedDoc the wrapped normalized message
     * @param wsdlDefinition the wsdl definition containing the definition for the normalized message.
     * @return the message type as QName
     */
    public static QName extractMessageType(Document wrappedDoc, Definition wsdlDefinition) throws WrapperProcessingException {
        QName msgTypeQName = null;
        // Get the jbi message wrapper element
        Element jbiMessageWrapper = wrappedDoc.getDocumentElement();
        if (jbiMessageWrapper == null) {
            throw new WrapperProcessingException("Could not get the wrapper message type, no message wrapper element is defined.");
        } else {
            String msgType = jbiMessageWrapper.getAttribute(WrapperUtil.WRAPPER_ATTRIBUTE_TYPE);
            if (msgType == null || msgType.equals("")) {
                throw new WrapperProcessingException("Failed to obtain type attribute from JBI Message Wrapper.");
            }

            int colonPos = msgType.indexOf(':');
            String localName = msgType;
            String prefix = null;
            String namespace = null;
            if (colonPos > -1) {
                prefix = msgType.substring(0, colonPos);
                localName = msgType.substring(colonPos + 1);
                namespace = jbiMessageWrapper.lookupNamespaceURI(prefix);
                if (namespace == null) { // can't find the namespace still, could be that the "xmlns:" attribute is not namespace aware
    	            namespace = jbiMessageWrapper.getAttribute("xmlns:" + prefix);
                }
            }
            
            if (prefix != null) {
            	msgTypeQName = new QName(namespace, localName, prefix);
            } else {
            	msgTypeQName = new QName(namespace, localName);
            }

        }
        return msgTypeQName;
    }

    /**
     * Get the optional message "name" defined in the wrapped document.
     * This is the logical name defined in the operation binding, not the type name.
     * @param wrappedDoc the wrapped normalized message
     * @return the message name
     */
    public static String extractMessageName(Document wrappedDoc) throws WrapperProcessingException {
        String messageName = null;
        // Get the jbi message wrapper element
        Element jbiMessageWrapper = wrappedDoc.getDocumentElement();
        if (jbiMessageWrapper == null) {
            throw new WrapperProcessingException("Could not get the wrapper message name, no message wrapper element is defined.");
        } else {
            messageName = jbiMessageWrapper.getAttribute(WrapperUtil.WRAPPER_ATTRIBUTE_NAME);
        }
        return messageName;
        
    }
    
}
