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
 * @(#)XMLDocumentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.impl;

import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLDocumentElement;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.model.selection.DefaultXMLNodeSelectionModelFactory;
import com.sun.wsdl.model.common.model.selection.XMLNodeSelectionModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Implements a XML document.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class XMLDocumentImpl extends XMLElementImpl implements XMLDocument {
    
    /** Serial version ID for this class. */
    static final long serialVersionUID = 2848392908287667475L;
    
    /** Holds the root element. */
    protected XMLDocumentElement rootElement;
    
    /** Holds processing instructions. */
    protected ArrayList processingInstructions = new ArrayList();
    
    /** Holds a map of all imported documents. */
    protected Map importedDocuments = new HashMap();
    
    /** Holds the base URI where this document is persisted. */
    protected String baseURI;
    
    /** Holds the data changed flag. */
    protected boolean dataChanged;
    
    /** holds falg for event firing/handling, needed for GUI
     * Not required when this document and all contained elements
     * as read only model.
     * 
     * */
    
    protected boolean mEnableEvents = true;

    protected XMLNodeSelectionModel mSelectionModel;
    
    /** Creates a new instance of XMLDocumentImpl */
    public XMLDocumentImpl() {
        dataChanged = false;
    }
    
    /** @see XMLDocument#initializeEInsightManager
     */
    
    /** @see XMLDocument#setDataChanged
     */
    public void setDataChanged(boolean changed) {
        dataChanged = changed;
    }
    
    /** @see XMLDocument#isDataChanged
     */
    public boolean isDataChanged() {
        return dataChanged;
    }
    
    /** @see XMLDocument#getDocumentElement
     */
    public XMLDocumentElement getDocumentElement() {
        XMLDocumentElement retDocElem = rootElement;
        if (null == retDocElem) {
            XMLDocument doc = getRootOwnerDocument();
            if (doc != null) {
                retDocElem = ((XMLDocumentImpl) doc).rootElement;
            }
        }
        return retDocElem;
    }
    
    /** @see XMLDocument#setDocumentElement
     */
    public void setDocumentElement(XMLDocumentElement r) {
        super.replaceChild(rootElement, r);
        rootElement = r;
    }
    
    /** Getter for root owner document.
     * @return  Root owner document.
     */
    public XMLDocument getRootOwnerDocument() {
        return (getOwnerDocument() == null
            ? this : getOwnerDocument().getRootOwnerDocument());
    }
    
    /** Indexed getter for processing instructions in the document.
     * @param   i   Index of processing instruction.
     * @return  Processing instruction.
     */
    public XMLProcessingInstruction getProcessingInstruction(int i) {
        return (XMLProcessingInstruction) processingInstructions.get(i);
    }
    
    /** Indexed setter for processing instruction in the document.
     * @param   i   Index of processing instruction.
     * @param   p   Processing instruction.
     */
    public void setProcessingInstruction(int i, XMLProcessingInstruction p) {
        if (processingInstructions.size() == i) {
            addProcessingInstruction(p);
        } else {
            processingInstructions.set(i, p);
        }
    }
    
    /** Adds a processing instruction to the document.
     * @param   p   Processing instruction.
     */
    public void addProcessingInstruction(XMLProcessingInstruction p) {
        super.addChild(p);
        processingInstructions.add(p);
    }
    
    /** Removes a processing instruction from the document.
     * @param   i   Index to the processing instruction.
     */
    public void removeProcessingInstruction(int i) {
        super.removeChild(getProcessingInstruction(i));
        processingInstructions.remove(i);
    }
    
    /** Removes a processing instructions from the document.
     * @param   p   Processing instruction.
     * @return  <code>true</code> if sucessfully removed.
     */
    public boolean removeProcessingInstruction(XMLProcessingInstruction p) {
        super.removeChild(p);
        return processingInstructions.remove(p);
    }
    
    /** Returns number of processing instructions in document.
     * @return  Number of processing instructions.
     */
    public int getProcessingInstructionSize() {
        return processingInstructions.size();
    }
    
    /** Creates a processing instruction for the document.
     * @return  new processing instruction.
     */
    public XMLProcessingInstruction createProcessingInstruction() {
        return new XMLProcessingInstructionImpl();
    }
    
    /** Creates a XML comment.
     * @return  new comment.
     */
    public XMLComment createXmlComment() {
        return new XMLCommentImpl();
    }
    
    /** Gets the default namespace URI declartion.
     * @return  Default namespace URI; <code>null</code> if none.
     */
    public String getDefaultNamespace() {
        return (getDocumentElement() != null
            ? getDocumentElement().getDefaultNamespace()
            : null);
    }
    
    /** Gets a namespace URI declartion.
     * @param   prefix  Prefix for namespace.
     * @return  A namespace URI; <code>null</code> if none.
     */
    public String getNamespace(String prefix) {
        return (getDocumentElement() != null
            ? getDocumentElement().getNamespace(prefix)
            : null);
    }
    
    /**
     * Gets the prefix associated with the given namespace URI.
     * @param namespaceURI the namespace URI
     * @return the namespace prefix or null if the namespace URI is not
     * currently associated with a prefix
     */
    public String getNamespacePrefix(String namespaceURI) {
        return (getDocumentElement() != null
            ? getDocumentElement().getNamespacePrefix(namespaceURI)
            : null);
    }
    
    /**
     * first looks for existing prefix for given namespace.
     * If exist then return this first prefix otherwise create
     * a new unique prefix set it to the map of prefix to namespace and return it.
     * @return new prefix.
     */
    public String createAndSetNamespacePrefix(String namespaceURI) {
    	String prefix = getNamespacePrefix(namespaceURI);
    	if(prefix != null && !prefix.equals("")) {
    		return prefix;
    	}
    	
    	int prefixCounter = 0;
    	
    	String prefixStr = "ns";
    	Map prefixToNs = getNamespaces();
    	while(true) {
    		prefix = prefixStr + prefixCounter;
    		if(!prefixToNs.containsKey(prefix)) {
    			setNamespace(prefix, namespaceURI);
    			break;
    		}
    	}
    	
    	return prefix;
    }
    
    /** Gets the XML namespace size.
     * @return  Number of namespaces defined.
     */
    public int getNamespaceSize() {
        return (getDocumentElement() != null
            ? getDocumentElement().getNamespaceSize()
            : 0);
    }
    
    /** Gets the XML namespace prefixes.
     * @return  A array of namespace prefixes; <tt>xmlns</tt> is the default.
     *          Can also be <code>null</code> if none.
     */
    public String[] getNamespacePrefixes() {
        return (getDocumentElement() != null
            ? getDocumentElement().getNamespacePrefixes()
            : null);
    }
    
    /** Gets all the XML namespace prefixes as a map.
     * @return  <code>Map</code> object representing all the namespaces.
     */
    public Map getNamespaces() {
        return (getDocumentElement() != null
            ? getDocumentElement().getNamespaces()
            : super.getNamespaces());
    }
    
    /**
     * @see XMLDocumentElement#getTargetNamespace
     */
    public String getTargetNamespace() {
        return (getDocumentElement() != null
            ? getDocumentElement().getTargetNamespace()
            : null);
    }
    
    /** @see XMLDocumentElement#setDefaultNamespace(java.lang.String)
     */
    public void setDefaultNamespace(String uri) {
        if (getDocumentElement() != null) {
            getDocumentElement().setDefaultNamespace(uri);
        }
    }
    
    /** @see XMLElement#setNamespace(java.lang.String, java.lang.String)
     */
    public void setNamespace(String prefix, String uri) {
        if (getDocumentElement() != null) {
            getDocumentElement().setNamespace(prefix, uri);
        }
    }
    
    /** @see XMLElement#setNamespaces
     */
    public void setNamespaces(Map m) {
        if (getDocumentElement() != null) {
            getDocumentElement().setNamespaces(m);
        }
    }
    
    /**
     * @see XMLElement#setTargetNamespace
     */
    public void setTargetNamespace(String tns) {
        if (getDocumentElement() != null) {
            getDocumentElement().setTargetNamespace(tns);
        }
    }
    
    /** Getter for imported document.
     * @param   ns  Namespace of document.
     * @return  Imported document.
     */
    public XMLDocument getImportedDocument(String ns) {
        return (XMLDocument) importedDocuments.get(ns);
    }
    
    /** Setter for imported document.
     * @param   ns      Namespace of document.
     * @param   doc     Imported document.
     */
    public void setImportedDocument(String ns, XMLDocument doc) {
        importedDocuments.put(ns, doc);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLDocument#getBaseURI
     */
    public String getBaseURI() {
        return baseURI;
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLDocument#setBaseURI
     */
    public void setBaseURI(String uri) {
        baseURI = uri.replace('\\', '/');
    }

    /** Resets this document so it can be reused.
     */
    public void reset() {
        setDocumentElement(null);
        processingInstructions.clear();
        importedDocuments.clear();
        dataChanged = false;
        if (nodeChildren != null) {
            nodeChildren.clear();
        }
    }
    
    
    /**
     * check if event firing is enabled.
     * needed for GUI.
     * Not required when this document and all contained elements
     * are used as read only model.
     * Default is true.
     * @return boolean
     */
    public boolean isEnableEvents() {
    	return mEnableEvents;
    }
    
    /**
     * set if event firing is enabled.
     * needed for GUI
     * Not required when this document and all contained elements
     * are used as read only model.
     * @param enable boolean
     */
    public void setEnableEvents(boolean enable) {
    	this.mEnableEvents = enable;
    }
    
    
    /**
     * Get the XMLNodeSelectionModel. Each document should maintain singleton
     * XMLNodeSelectionModel
     */
    public XMLNodeSelectionModel getSelectionModel() {
        if(this.mSelectionModel == null) {
            this.mSelectionModel = DefaultXMLNodeSelectionModelFactory.getInstance().creatXMLNodeSelectionModel(this);
        }
        
        return this.mSelectionModel;
    }
}
