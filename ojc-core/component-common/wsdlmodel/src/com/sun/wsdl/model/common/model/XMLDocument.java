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
 * @(#)XMLDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;


import com.sun.wsdl.model.common.model.selection.XMLNodeSelectionModel;
import java.util.Map;
import javax.xml.namespace.QName;
/**
 * Describes a XML document.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XMLDocument
    extends XMLElement {
    
    /** Set the data changed flag.
     * @param   changed     <code>true</code> if data is changed and needs saving.
     */
    void setDataChanged(boolean changed);
    
    /** Tests if the data is changed.
     * @return  <code>true</code> if data is changed and needs saving.
     */
    boolean isDataChanged();
    
    /** Getter for the root element in the document.
     * @return  Root element in the document.
     */
    XMLDocumentElement getDocumentElement();
    
    /** Setter for the root element in the document.
     * @param   r   Root element in the document.
     */
    void setDocumentElement(XMLDocumentElement r);
    
    /** Getter for root owner document.
     * @return  Root owner document.
     */
    XMLDocument getRootOwnerDocument();
    
    /** Indexed getter for processing instructions in the document.
     * @param   i   Index of processing instruction.
     * @return  Processing instruction.
     */
    XMLProcessingInstruction getProcessingInstruction(int i);
    
    /** Indexed setter for processing instruction in the document.
     * @param   i   Index of processing instruction.
     * @param   p   Processing instruction.
     */
    void setProcessingInstruction(int i, XMLProcessingInstruction p);
    
    /** Adds a processing instruction to the document.
     * @param   p   Processing instruction.
     */
    void addProcessingInstruction(XMLProcessingInstruction p);
    
    /** Removes a processing instruction from the document.
     * @param   i   Index to the processing instruction.
     */
    void removeProcessingInstruction(int i);
    
    /** Removes a processing instructions from the document.
     * @param   p   Processing instruction.
     * @return  <code>true</code> if sucessfully removed.
     */
    boolean removeProcessingInstruction(XMLProcessingInstruction p);
    
    /** Returns number of processing instructions in document.
     * @return  Number of processing instructions.
     */
    int getProcessingInstructionSize();
    
    /** Creates a processing instruction for the document.
     * @return  new processing instruction.
     */
    XMLProcessingInstruction createProcessingInstruction();
    
    /** Creates a XML comment.
     * @return  new comment.
     */
    XMLComment createXmlComment();
    
    /** Creates a XML node for this document.
     * @param   t   Type of node to create.
     * @return  New XML node.
     */
    XMLNode createXmlNode(String t);
    
    /** @see XMLDocumentElement#getTargetNamespace
     */
    String getTargetNamespace();
    
    /** @see XMLDocumentElement#setTargetNamespace
     */
    void setTargetNamespace(String tns);
    
    /** Gets the default namespace URI declartion.
     * @return  Default namespace URI; <code>null</code> if none.
     */
    String getDefaultNamespace();
    
    /** Gets a namespace URI declartion.
     * @param   prefix  Prefix for namespace.
     * @return  A namespace URI; <code>null</code> if none.
     */
    String getNamespace(String prefix);
    
    /**
     * Gets the prefix associated with the given namespace URI.
     * @param namespaceURI the namespace URI
     * @return the namespace prefix or null if the namespace URI is not
     * currently associated with a prefix
     */
    String getNamespacePrefix(String namespaceURI);
    
    /**
     * first looks for existing prefix for given namespace.
     * If exist then return this first prefix otherwise create
     * a new unique prefix set it to the map of prefix to namespace and return it.
     * @return new prefix.
     */
    String createAndSetNamespacePrefix(String namespaceURI);
    
    /** Gets the XML namespace size.
     * @return  Number of namespaces defined.
     */
    int getNamespaceSize();
    
    /** Gets the XML namespace prefixes.
     * @return  A array of namespace prefixes; <tt>xmlns</tt> is the default.
     *          Can also be <code>null</code> if none.
     */
    String[] getNamespacePrefixes();
    
    /** Gets all the XML namespace prefixes as a map.
     * @return  <code>Map</code> object representing all the namespaces.
     */
    Map getNamespaces();
    
    /** Getter for imported document.
     * @param   ns  Namespace of document.
     * @return  Imported document.
     */
    XMLDocument getImportedDocument(String ns);
    
    /** Setter for imported document.
     * @param   ns      Namespace of document.
     * @param   doc     Imported document.
     */
    void setImportedDocument(String ns, XMLDocument doc);
    
    /** Getter for the base URI where this document is persisted.
     * @return  Base URI.
     */
    String getBaseURI();
    
    /** Setter for the base URI where this document is persisted.
     * @param   uri     Base URI.
     */
    void setBaseURI(String uri);
    
    /**
     * get an named Object that has a name attribute whose value is same
     * as local name of the given qName and the object is defined
     * in the namespace given by the namespace in the qName.
     * 
     * localName must be present in the qName.
     * If namespace is missing in qName then default namespace of the
     * document is used.
     * 
     * A null is returned if no object is found.
     * 
     * This method may be implemented differently by each concrete
     * document class.
     *  
     * @param qName QName of the object we are searching.
     * @return Object object matching the qName. caller should
     * check for the type of object he is looking for.
     */
    Object getElementByQName(QName qName);
    
    /**
     * check if event firing is enabled.
     * needed for GUI.
     * Not required when this document and all contained elements
     * are used as read only model.
     * Default is true.
     * @return boolean
     */
    boolean isEnableEvents();
    
    /**
     * set if event firing is enabled.
     * needed for GUI
     * Not required when this document and all contained elements
     * are used as read only model.
     * @param enable boolean
     */
    void setEnableEvents(boolean enable);
    
    /**
     * Reset this document. removes children, processing instruction etc
     *
     */
    void reset();
    
    /**
     * Get the XMLNodeSelectionModel. Each document should maintain singleton
     * XMLNodeSelectionModel
     */
    XMLNodeSelectionModel getSelectionModel();
}
