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
 * @(#)WrapperBuilderImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrappedDocument;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * Assists in building the WSDL 1.1 wrapper for normalized messages specified by JBI
 *
 * Usage sequence:
 * initialize()
 * addParts() or multiple addPart() calls
 * getResult()
 *
 * The same instance should not be used by multiple threads concurrently as it is
 * not guaranteed to be thread safe - and maintains state
 *
 * The re-use of the same instance however is encouraged.
 *
 * @author Sun Microsystems
 */
public class WrapperBuilderImpl implements WrapperBuilder {

    DocumentBuilder mBuilder;
    Document normalDoc;
    Element jbiMessageWrapper;
    WSDLInfo info;
    String operationMessageName;
    boolean isResultPrepared;
    Map mPartNameToPartNodes;
    private Map<String, Map> mNamespaceMap;
    private Map<String, Map> mAttributesMap;
    private Map<String, String> mGlobalNamespaces;
    private Map<String, Attr> mGlobalAttributes;

    /**
     * Creates a new instance
     *
     * The preferred way to create and use this builder implementation is to use
     * the <code>HelperFactory</code> instead.
     */
    public WrapperBuilderImpl() throws WrapperProcessingException {
    }

    /**
     * Re-sets the result document, sets the WSDL message definition of the message to normalize
     */
    public void initialize(Document docToPopulate, Message wsdlMessageDefinition, String operationBindingMessageName) throws WrapperProcessingException {
        isResultPrepared = false;
        mPartNameToPartNodes = null;
        operationMessageName = operationBindingMessageName;
        this.normalDoc = docToPopulate;
        if (docToPopulate == null)  {
            try {
                normalDoc = newDocument();
            } catch (ParserConfigurationException ex) {
                throw new WrapperProcessingException("Failed to create an empty target document for building the wrapped normalized message: " + ex.getMessage(), ex);
            }
        }

        info = WSDLInfo.getInstance(wsdlMessageDefinition);

        // Add a message wrapper
        Document currDoc = normalDoc;
        jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(currDoc, info.getMessageType(), operationBindingMessageName);
        currDoc.appendChild(jbiMessageWrapper);
    }
    
    
    /*
     * the message wrapper is created at this point , does not wait till
     * the first part that is added to the builder.
     */
    
    public void initialize( Message wsdlMessageDefinition, String operationBindingMessageName,Document docToUse) throws WrapperProcessingException {
        isResultPrepared = false;
        mPartNameToPartNodes = null;
        operationMessageName = operationBindingMessageName;
        

        info = WSDLInfo.getInstance(wsdlMessageDefinition);

        
        jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(docToUse, info.getMessageType(), operationBindingMessageName);
        
    }

    /*
     * No need to set the doc wrapper in this version - we grab the doc ref from
     * the first part that is added to the builder.
     */
    public void initialize(Message wsdlMessageDefinition, String operationBindingMessageName) throws WrapperProcessingException {
        isResultPrepared = false;
        mPartNameToPartNodes = null;
        operationMessageName = operationBindingMessageName;

        info = WSDLInfo.getInstance(wsdlMessageDefinition);
    }

    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#declareGlobalNS(java.util.Map) */
    public void declareGlobalNS(Map namespaceMap) {
        if (jbiMessageWrapper != null && namespaceMap != null) {	
            copyNamespaces(jbiMessageWrapper, namespaceMap);
	} else if (namespaceMap != null) {
            mGlobalNamespaces = new HashMap<String,String>();
            mGlobalNamespaces.putAll(namespaceMap);
        }
    }
	
    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#declareGlobalAttributesjava.util.Map) */
    public void declareGlobalAttributes(Map attributeMap) {
        if (jbiMessageWrapper != null && attributeMap != null) {
	    copyAttributes(jbiMessageWrapper, attributeMap);
	} else if (attributeMap != null) {
            mGlobalAttributes = new HashMap<String,Attr>();
            mGlobalAttributes.putAll(attributeMap);
        }
    }
	

	/**
     * Add a part in the right position (wrapped in a JBI part wrapper) to the JBI message wrapper element
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     */
    public void addPart(String partName, NodeList partNodes) throws WrapperProcessingException {
    	addPart(partName, partNodes, null);
    }

    /**
     * Add a part in the right position (wrapped in a JBI part wrapper) to the JBI message wrapper element
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     */
    public void addPart(String partName, Element partNode) throws WrapperProcessingException {
        addPart(partName, new NodeListImpl(partNode), null);
    }

    /**
     * Add parts in the right order (each wrapped in a JBI part wrapper) to the passed in JBI message wrapper element
     * The jbiMessageWrapper must be a node of the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param jbiMessageWrapper The message wrapper element to add the jbi part wrappers and part payload to
     * @param messageDefinintion the WSDL message definition
     * @param partNameToPartNodes a mapping from the part name to the part NodeList (payload).
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     */
    public void addParts(Map partNameToPartNodes) throws WrapperProcessingException {
    	addParts(partNameToPartNodes, null);
    }

    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPart(java.lang.String, org.w3c.dom.Element, java.util.Map) */
    public void addPart(String partName, Element partNode, Map namespaceMap) throws WrapperProcessingException {
        addPart(partName, new NodeListImpl(partNode), namespaceMap);
    }
    
    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPart(java.lang.String, org.w3c.dom.Element, java.util.Map, java.util.Map) */
    public void addPart(String partName, Element partNode, Map namespaceMap, Map attributesMap) throws WrapperProcessingException {
        addPart(partName, new NodeListImpl(partNode), namespaceMap, attributesMap);
    }

    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPart(java.lang.String, org.w3c.dom.NodeList, java.util.Map) */
    public void addPart(String partName, NodeList partNodes, Map namespaceMap) throws WrapperProcessingException {
        // Create a message wrapper if one doesn't already exist
        if (jbiMessageWrapper == null ) {
            if (partNodes.getLength() > 0) {
                Document doc = partNodes.item(0).getOwnerDocument();
                jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(doc,
                                                                        info.getMessageType(),
                                                                        operationMessageName);
            } else {
                throw new WrapperProcessingException("Invalid zero-length data for part: " + partName);
            }
        }
        List partsOrder = info.getPartsOrder();
        int partPos = partsOrder.indexOf(partName);
        if (partPos > -1) {
            if (mPartNameToPartNodes == null) {
                mPartNameToPartNodes = new HashMap();
                mNamespaceMap = new HashMap<String, Map>();
            }
            mPartNameToPartNodes.put(partName, partNodes);
            if (namespaceMap != null) {
            	mNamespaceMap.put(partName, new HashMap(namespaceMap));
            }
        }
        else {
            throw new WrapperProcessingException("Unknown part " + partName + " is not defined in the WSDL message definition for " + info.getMessageType() + ". Cannot add part to normalized message.");
        }

    }
 
    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPart(java.lang.String, org.w3c.dom.NodeList, java.util.Map,java.util.Map) */
    public void addPart(String partName, NodeList partNodes, Map namespaceMap, Map attributesMap) throws WrapperProcessingException {
        // Create a message wrapper if one doesn't already exist
        if (jbiMessageWrapper == null ) {
            if (partNodes.getLength() > 0) {
                Document doc = partNodes.item(0).getOwnerDocument();
                jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(doc,
                                                                        info.getMessageType(),
                                                                        operationMessageName);
            } else {
                throw new WrapperProcessingException("Invalid zero-length data for part: " + partName);
            }
        }
        List partsOrder = info.getPartsOrder();
        int partPos = partsOrder.indexOf(partName);
        if (partPos > -1) {
            if (mPartNameToPartNodes == null) {
                mPartNameToPartNodes = new HashMap();
                mNamespaceMap = new HashMap<String, Map>();
                mAttributesMap = new HashMap<String, Map>();
            }
            mPartNameToPartNodes.put(partName, partNodes);
            if (namespaceMap != null) {
            	mNamespaceMap.put(partName, new HashMap(namespaceMap));
            }
            if (attributesMap != null) {
            	if (mAttributesMap == null) {
                    mAttributesMap = new HashMap<String, Map>();
            	}
                mAttributesMap.put(partName, new HashMap(attributesMap));
            }
        }
        else {
            throw new WrapperProcessingException("Unknown part " + partName + " is not defined in the WSDL message definition for " + info.getMessageType() + ". Cannot add part to normalized message.");
        }

    }

    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPartWithAttachment(java.lang.String) */
    public String addPartWithAttachment(String partName) throws WrapperProcessingException {
        String contentId = WrapperUtil.createXopCid();
        addPartWithAttachment(partName, contentId);

        return contentId;
    }

    /** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addPartWithAttachment(java.lang.String, java.lang.String) */
    public void addPartWithAttachment(String partName, String contentId) throws WrapperProcessingException {
    	Document doc = null;
        // Create a message wrapper if one doesn't already exist
        if (jbiMessageWrapper != null ) {
            doc = jbiMessageWrapper.getOwnerDocument();
        } else {
            doc = normalDoc;
        }

        // create a part Node
        Element aPartNode = doc.createElementNS(XOP_NAMESPACE, "xop:Include");
        if (contentId == null || "".equals(contentId)) {
            aPartNode.setAttribute("href", WrapperUtil.createXopCid());
        } else {
            aPartNode.setAttribute("href", contentId);
        }

        if (mPartNameToPartNodes == null) {
            mPartNameToPartNodes = new HashMap();
            mNamespaceMap = new HashMap<String, Map>();
        }
        mPartNameToPartNodes.put(partName, new NodeListImpl(aPartNode));

    }

	/** @see com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder#addParts(java.util.Map, java.util.Map) */
    public void addParts(Map partNameToPartNodes, Map namespaceMap) throws WrapperProcessingException {
        mPartNameToPartNodes = partNameToPartNodes;
        mNamespaceMap = new HashMap<String, Map>();
        if (namespaceMap != null) {
            for (Iterator iter = partNameToPartNodes.keySet().iterator(); iter.hasNext();) {
                String part = (String) iter.next();
        	mNamespaceMap.put(part, new HashMap(namespaceMap));
            }
        }
    }

    protected void copyNamespaces(Element elem, Map namespaces) {
    	if (elem == null || namespaces == null) return;
    	Set keys = namespaces.keySet();
        for (Iterator iter = keys.iterator(); iter.hasNext();) {
            String prefix = (String) iter.next();
	    elem.setAttributeNS("http://www.w3.org/2000/xmlns/",
		        	(prefix.startsWith("xmlns:")) ? prefix : "xmlns:"+ prefix,
			        (String) namespaces.get(prefix));
        }
    }
    
    
    protected void copyAttributes(Element elem, Map attributes) {
        if (elem == null || attributes == null) return;
    	Set keys = attributes.keySet();
	for (Iterator iter = keys.iterator(); iter.hasNext();) {
	    String name = (String) iter.next();
	    String prefix = (name.indexOf(":") > 0 )? name.substring(0, name.indexOf(":")) : null;
	    Attr attrNode = (Attr) attributes.get(name);
            if (prefix != null) {
                elem.setAttributeNS(attrNode.getNamespaceURI(), name, attrNode.getValue());
            } else {
                elem.setAttribute(name, attrNode.getValue());
            }
	}    
    }

    /**
     * Retrieve the Normalized Message with the parts added (addPart(s)) since the initialize() call
     * Make sure to only retrieve the result after all desired parts have been added.
     *
     * Parts added after the result of a build squence has been retrieved already
     * (meaning without starting a new build squence by calling initialize) will NOT be included
     * in subsequent retrievals of Result
     */
    public Document getResult() throws WrapperProcessingException {
        if (!isResultPrepared) {
            // copy "global" namespaces to JBI message
            if (this.mGlobalNamespaces != null) {
                this.copyNamespaces(jbiMessageWrapper, mGlobalNamespaces);
            }
            
            // copy "global" attributes to JBI message
            if (mGlobalAttributes != null) {
            	copyAttributes(jbiMessageWrapper, mGlobalAttributes);
            }

            Iterator partsIter = info.getOrderedMessageParts().iterator();
            while (partsIter.hasNext()) {
                Part aPart = (Part)partsIter.next();
                String partName = aPart.getName();
                NodeList nodes = null;
                if (mPartNameToPartNodes != null) {
                    Object payload = mPartNameToPartNodes.get(partName);
                    if (payload instanceof NodeList) {
                        nodes = (NodeList) payload;
                    } else {
                        //throw new WrapperProcessingException("Part payload added is not a NodeList: " + (payload == null ? "payload is null" : payload.getClass().getName()));
                        nodes = new NodeListImpl();
                    }
                }

                Element currJBIMessageWrapper = jbiMessageWrapper;
                Element wrapperElem = WrapperUtil.importJBIWrappedPart(currJBIMessageWrapper.getOwnerDocument(), nodes);
                // copy namespaces to the jbi:part element, if any are stored for this part
                if(mNamespaceMap != null){
		    Map nsMap = mNamespaceMap.get(partName);
		    if (nsMap != null) {
		        copyNamespaces(wrapperElem, nsMap);
		    }
		}
		
		// copy attributes to the jbi:part element, if any are stored for this part
                if(mAttributesMap != null){
		    Map attrsMap = mAttributesMap.get(partName);
		    if (attrsMap != null) {
		        copyAttributes(wrapperElem, attrsMap);
		    }
		}
		
                currJBIMessageWrapper.appendChild(wrapperElem);
            }

            if (jbiMessageWrapper == null) {
                // No JBI message wrapper is created yet. This could only
                // happen when the following conditions are true:
                // 1. Wrong use of this wrapper build utility: no initialize() or addPart() calls
                //    before getResult() is invoked. We should validate against this.
                // 2. the alternative initialize() method
                //    void initialize(Message wsdlMessageDefinition, String operationBindingMessageName) is
                //    used to set up the wrapper builder, and no addPart() calls before getResult() is invoked.
                //    At least one part is required in a WSDL message per WSDL 1.1 spec, but Basic Profile 1.0 spec allows
                //    zero parts for soap:bindingS: Use of |wsdl:message| elements with zero parts is permitted in SOAP RPC styles
                //    to permit operations that have no (zero) parameters and/or a return value.
                //    So we need to support that.
                if (info == null ||
                    info.getMessageType() == null) {
                    throw new WrapperProcessingException("initialize() must be called on the WrapperBuilder instance first to set up the WSDL info to normalize the message");
                }

                Document doc = null;
                try {
                    doc = newDocument();
                } catch (ParserConfigurationException e) {
                    throw new WrapperProcessingException("Failed to create an empty target document for building the wrapped normalized message: " + e.getMessage(), e);
                }
                jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(doc,
                                                                        info.getMessageType(),
                                                                        operationMessageName);
                normalDoc = doc;
                // just add the message wrapper node
                normalDoc.appendChild(jbiMessageWrapper);
            } else {
                // Replace the existing document element with the wrapper element
                Document origDoc = jbiMessageWrapper.getOwnerDocument();
                origDoc.removeChild(origDoc.getDocumentElement());
                origDoc.appendChild(jbiMessageWrapper);
                if (normalDoc == null)
                {
                    normalDoc = new WrappedDocument(origDoc, jbiMessageWrapper);
                }
            }

            isResultPrepared = true;
            mPartNameToPartNodes = null;    //reset the parts map
        }
        return normalDoc;
    }

    public Message getStatusMessage() throws WrapperProcessingException {
        Message msg = null;

        try {
            WSDLFactory factory = javax.wsdl.factory.WSDLFactory.newInstance();
            Definition def = factory.newDefinition();
            msg = def.createMessage();
            msg.setQName(new QName(WrapperBuilder.STATUS_TAG));
            Part part = def.createPart();
            part.setName(WrapperBuilder.RESULT_TAG);
            msg.addPart(part);
        } catch (WSDLException ex) {
            throw new WrapperProcessingException(ex);
        }

        return msg;
    }

    public Document getStatusDocument(String statusMessage) throws WrapperProcessingException {
        Document res = null;
        try {
            Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();

            WrapperBuilder builder = HelperFactory.createBuilder();
            builder.initialize(doc, getStatusMessage(), null);
            // add the "wrapper" elements for "normalizing" doc/lit
            Element statusElem = doc.createElement(WrapperBuilder.STATUS_TAG);
            statusElem.appendChild(doc.createTextNode(statusMessage));
            builder.addPart(WrapperBuilder.RESULT_TAG, statusElem);
            res = builder.getResult();
        } catch (ParserConfigurationException ex) {
            throw new WrapperProcessingException(ex);
        }

        return res;
    }

    private Document newDocument() throws ParserConfigurationException {
        if (mBuilder == null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            mBuilder = factory.newDocumentBuilder();
        }
        return mBuilder.newDocument();
    }

}
