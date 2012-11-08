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
 * @(#)JBIMessageImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.impl.WrapperBuilderImpl;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

/**
 * JBI message implementation
 * 
 * @author Sun Microsystems
 * @version 
 */
public class JBIMessageImpl implements WSMessage/* , JBIWSDL11WrapperRdr */,
        java.io.Serializable {
    private static final long serialVersionUID = -3453799599264521178L;
    
    /* */
        private static final String EXT_PART_UNINITIALIZED_ATTR = "uninitialized";
        
        /* */
        private static final String ATTR_VALUE_TRUE = "true";

    private Message mWsdlMessage = null;
    private String mMessageName = null;
    
    private Map<String, Element> parts = new LinkedHashMap<String, Element>();
    
    private XmlResourceProviderPool resourcePool = null;
    
    private Document messageDocument;
    
    private boolean isExternalReference;
    
    private List references = Collections.synchronizedList(new ArrayList());
    
    private Map<String, Object> mNMProperties = new HashMap();
    
    private Map<String, DataHandler> mAttachmentMap = new HashMap<String, DataHandler>();
    
    /**
     * Default constructor
     */    
    public JBIMessageImpl() {
        init();
    }
    
    private void init(){
        resourcePool = (XmlResourceProviderPool)BPELSERegistry.getInstance()
        .lookup(XmlResourceProviderPool.class.getName());        
    }
    
    /**
     * Creates a new Message object.
     * 
     * @param wsdlMessage wsdlmodel WSDLMessage
     * @throws Error DOCUMENT ME!
     */
    public JBIMessageImpl(Message wsdlMessage) {
        init();
        mWsdlMessage = wsdlMessage;
        
        messageDocument = getNewDocument();

        Element wrapperMsgElem = WrapperUtil.createJBIMessageWrapper(messageDocument, getMessageType(), null);
        
        messageDocument.appendChild(wrapperMsgElem);
        
        try {
            Iterator iter = wsdlMessage.getOrderedParts(null).iterator();

            while (iter.hasNext()) {
                Part p = (Part) iter.next();
                
                QName elemName = p.getElementName();
                Element partElement = null;
                if (elemName != null) { // element
                    partElement = createElementNode(elemName);
                    Element partWrapperElement = WrapperUtil.createJBIWrappedPart(messageDocument, partElement);
                    partWrapperElement.setAttribute(EXT_PART_UNINITIALIZED_ATTR, ATTR_VALUE_TRUE);
                    wrapperMsgElem.appendChild(partWrapperElement);
                    parts.put(p.getName(), partElement);
                } else {
                    Element partWrapperElement = WrapperUtil.createJBIWrappedPart(messageDocument, (Node)null);
                    partWrapperElement.setAttribute(EXT_PART_UNINITIALIZED_ATTR, ATTR_VALUE_TRUE);
                    wrapperMsgElem.appendChild(partWrapperElement);
                    parts.put(p.getName(), partWrapperElement);
                }
            }
            
        } catch (Exception e) {
            throw new BPELRuntimeException(e);
        }
    }

    private Element createElementNode(QName cmnElemName) {
        Element partElement;
        String ns = cmnElemName.getNamespaceURI();
        partElement = messageDocument.createElementNS(ns, cmnElemName.getLocalPart());                     
        return partElement;
    }

    /**
     * Creates a new JBIMessageImpl object.
     * 
     * @param element The DOM document containing jbi:message element
     * @param wsdlMessage The WSDL Message model
     * 
     * @throws RuntimeException Thrown if any exception occurs while construting
     * the message.
     */
    public JBIMessageImpl(Document doc, Message wsdlMessage) {
        init();
        constructJBIMessage(doc, wsdlMessage);
    }
    
    //Discards text nodes and returns first element node  
        private Element getElementNode(Node node) {
                while (node != null && (node.getNodeType() != Node.ELEMENT_NODE)) {
                        node = node.getNextSibling();
                }
                
                if (node != null) {
                        return (Element) node;
                }
                
                return null;
        }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.bpel.runtime.WSMessage#getPart(java.lang.String)
     */
    public Element getPart(String part) {
        Element partElement = parts.get(part);
        if (partElement != null) {
            Element partWrapper = getPartWrapper(partElement);
            if (isPartInitialized(partWrapper)) {
                return partElement;
            }
        }
        return null;
    }
   
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#createPart(java.lang.String)
     */
    public Element createPart(String partName) {
        Element partElement = parts.get(partName);
        if (partElement == null) {
                        throw new RuntimeException(I18n.loc("BPCOR-6027: Part {0} not defined on Message {1},", 
                                        partName, mWsdlMessage.getQName()));
                }
        Element partWrapper = getPartWrapper(partElement);
        if (isPartInitialized(partWrapper)) {
                throw new RuntimeException(I18n.loc("BPCOR-6091: Part '{0}' already created on Message '{1}'.", 
                                        partName, mWsdlMessage.getQName()));
        }
        partWrapper.removeAttribute(EXT_PART_UNINITIALIZED_ATTR);
        return partElement;
        }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#allPartsInitialized()
     */
        public boolean allPartsInitialized() {
                Iterator iter = mWsdlMessage.getOrderedParts(null).iterator();
                boolean allPartsInitialized = true;
                
        while (iter.hasNext()) {
            Part p = (Part) iter.next();
            Element partElement = parts.get(p.getName());
            if (!isPartInitialized(getPartWrapper(partElement))) {
                // We found a part that was uninitialized exit and return false.
                allPartsInitialized = false;
                break;
            }
        }
        
                return allPartsInitialized;
        }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getNonInitializedParts()
     */
    public List<Part> getNonInitializedParts() {
        List<Part> nonInitializedParts = new ArrayList<Part>();
                Iterator iter = mWsdlMessage.getOrderedParts(null).iterator();
                boolean allPartsInitialized = true;

        while (iter.hasNext()) {
            Part p = (Part) iter.next();
            Element partElement = parts.get(p.getName());
            if (!isPartInitialized(getPartWrapper(partElement))) {
                nonInitializedParts.add(p);
            }
        }

                return nonInitializedParts.isEmpty() ? null : nonInitializedParts;
    }

        /*
     * 
     */
    private boolean isPartInitialized(Element partWrapper) {
        // Check to see if it is uninitialized.
        String attr = partWrapper.getAttribute(EXT_PART_UNINITIALIZED_ATTR);

        // if the attribute value is an empty string it is initialized
        if (Utility.isEmpty(attr)) {
                return true;
        }

        return false;
    }
    
    /*
     * 
     */
    private Element getPartWrapper(Element partElement) {
        Element partWrapper = null;

        // Get the part wrapper for the part element
        if ((WrapperUtil.WRAPPER_DEFAULT_NAMESPACE.equals(partElement.getNamespaceURI())) 
                        && (WrapperUtil.WRAPPER_PART.equals(partElement.getNodeName()))) {
                // The part element is the part wrapper.
                partWrapper = partElement;
        } else {
                // The parent is the part wrapper.
                partWrapper = (Element) partElement.getParentNode();
        }
        
        return partWrapper;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getMessageType()
     */
    public QName getMessageType() {
        return mWsdlMessage.getQName();         
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getWSDLMessage()
     */
    public Message getWSDLMessage() {
        return mWsdlMessage;
    }
    
    public Document getDocument() {
        return messageDocument;
    }
    
    public Element getElement() {
        return getDocument().getDocumentElement();
    }
    
    private static boolean isStatus(Message msg) {
        if (msg.getQName().equals(WrapperBuilder.STATUS_TAG)) {
            return true;
        }
        return false;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
                Element element = getDocument().getDocumentElement();
                if (element != null) {
                        if (BPELDocument.BPEL_NAMESPACE.equals(getMessageType()
                                        .getNamespaceURI())) {
                                return element.getFirstChild().getTextContent();
                        } else {
                                return DOMHelper.createXmlString(element);
                        }
                } else {
                        return null;
                }
        }

        /**
         * @return Returns the mMessageName.
         */
        public String getMessageName() {
                return mMessageName;
        }
    

    
    
    private void constructJBIMessage(Document doc, Message wsdlMessage) {
        try {
            mWsdlMessage = wsdlMessage;
            messageDocument = doc;

            WrapperParser parser = HelperFactory.createParser();
            if (isStatus(wsdlMessage)) {
                WrapperBuilder wBuilder = new WrapperBuilderImpl();
                parser.parse(messageDocument, wBuilder.getStatusMessage());
            } else {
                parser.parse(messageDocument, wsdlMessage);
            }
            mMessageName = parser.getMessageName();

            List<Part> partList = wsdlMessage.getOrderedParts(null);
            
            int partCount = parser.getNoOfParts();
            String[] partNameArray = parser.getPartNames();

            for (int index = 0; index < partCount; index++) {
                String partName = partNameArray[index];
                Element jbiPartElement = parser.getWrappedPart(partName);
                Part partDef = partList.get(index);
                QName elementName = partDef.getElementName();

                if (elementName != null) {
                    Node firstChild = jbiPartElement.getFirstChild();
                    Element child = null;
                    if (firstChild != null) {
                        child = getElementNode(firstChild);
                    }
                    if (child == null) {
                        child = createElementNode(elementName);
                        jbiPartElement.appendChild(child);
                    }
                    parts.put(partName, child);
                } else {
                    parts.put(partName, jbiPartElement);
                }
            }
        } catch (Exception ex) {
            throw new RuntimeException(I18n.loc("BPCOR-6113: Unable to parse WSDL 1.1 wrapper"), ex); //$NON-NLS-1$
        }
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpms.bpel.runtime.WSMessage#copy()
     */
    public WSMessage copy() {
        
        JBIMessageImpl copy =  new JBIMessageImpl();
        copy.mWsdlMessage = this.mWsdlMessage;
        copy.mMessageName = this.mMessageName;
        Document document = getNewDocument();
        Element wrapperMsgElem = WrapperUtil.createJBIMessageWrapper(document, getMessageType(), null);
        document.appendChild(wrapperMsgElem);
        copy.messageDocument = document;
        copy.mNMProperties=(Map)((HashMap)this.mNMProperties).clone();
        
        Iterator iterator = this.parts.keySet().iterator();
        
        for( ;iterator.hasNext(); ) { 
            String partName = (String) iterator.next();
            Part partDef = copy.mWsdlMessage .getPart(partName);
            Element partElement = (Element) this.parts.get(partName);
            Node partElementCopy = document.importNode(partElement, true);
            Element partWrapperElement = null;
            Element partEl = null;
            //check if we need to create a wrapper
            if (partDef.getElementName() != null) { 
                 partWrapperElement = WrapperUtil.createJBIWrappedPart(document, partElementCopy);
                 partEl = (Element) partElementCopy;
            }else {
                partWrapperElement = (Element) partElementCopy;
                partEl = partWrapperElement;
            }
            wrapperMsgElem.appendChild(partWrapperElement);            
            copy.parts.put(partName, partEl);
        }
        return copy;
    }
    
    private String getMessageNameAttribute() {
        return this.messageDocument.getDocumentElement().getAttribute(WrapperUtil.WRAPPER_ATTRIBUTE_NAME);    
    }
    
    private void copyNamespaces(Node node, Map<String, String> nsMap) {
        if (node == null || !(node instanceof Element)) {
            return;
        }
        Set<String> keys = nsMap.keySet();
        Element elem = (Element) node;
        for (Iterator<String> iter = keys.iterator(); iter.hasNext();) {
            String prefix = iter.next();
            elem.setAttributeNS("http://www.w3.org/2000/xmlns/", prefix, 
                            nsMap.get(prefix));
        }
    }
    
    private Document getNewDocument() {
        XmlResourceProvider xmlResourceProvider = resourcePool.acquireXmlResourceProvider();
        Document dom = xmlResourceProvider.getDocumentBuilder().newDocument();
        resourcePool.releaseXmlResourceProvider(xmlResourceProvider);
        xmlResourceProvider = null;
        return dom;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#setAsExternalReference()
     */
    public void setAsExternalReference() {
        this.isExternalReference = true;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#addInternalReference(com.sun.bpel.model.meta.RVariable)
     */
    public void addInternalReference(RVariable variable) {
        this.references.add(variable);
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#removeInternalReference(com.sun.bpel.model.meta.RVariable)
     */
    public void removeInternalReference(RVariable variable) {
        this.references.remove(variable);
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#isOnlyReference(com.sun.bpel.model.meta.RVariable)
     */
    public boolean isOnlyReference(RVariable variable) {
        if (isExternalReference) {
            return false;
        } else if (references.size() > 1) {
            return false;
        } else if (references.size() == 1) {
            return references.contains(variable);
        }
        
        // the following condition should never happen, should we throw exception here?
        return true;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getNMProperty(java.lang.String, java.lang.String)
     */
    public Object getNMProperty(String propertyKey) {
        return mNMProperties.get(propertyKey);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getNMProperties()
     */
    public Map<String, Object> getNMProperties() {
        return mNMProperties;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#setNMProperty(java.lang.String, java.lang.String, java.lang.Object)
     */
    public void setNMProperty(String propertyKey, Object val) {
        /** 
         * DEV NOTES:- make sure that the values are never references to some 
         * BPELSE reference-able objects. For example, they shouldn't be something like
         * JXPath's NodePointer objects. If such an object is sent to out to a different 
         * component, functionality will fail.
         * TODO ??Should we throw an exception if it is not of the types expected??
         * Expected types are Strings and DOM Node.
         */

        mNMProperties.put(propertyKey, val);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getAttachment(java.lang.String)
     */
    public DataHandler getAttachment(String name) {

        return mAttachmentMap.get(name);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#setAttachment(java.lang.String, javax.activation.DataHandler)
     */
    public void setAttachment(String name, DataHandler dHdlr) {
        if (mAttachmentMap.get(name) != null) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-4016: Overriding an existing attachment referred by {0} on message {1}", name, getMessageName()));
        }
        mAttachmentMap.put(name, dHdlr);        
    }
    Logger LOGGER = Logger.getLogger(JBIMessageImpl.class.getName());

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage#getAttachments()
     */
    public Set<String> getAttachments() {
        return mAttachmentMap.keySet();
    }
    
    
}
