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
 * @(#)WrappedDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper.util;

import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Comment;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;
import org.w3c.dom.UserDataHandler;


/*
 * This class is simply a decorator around an existing Document implementation
 * that allows us to modify the behavior of certain methods.  For example, the
 * SOAPDocumentImpl class in SAAJ spits out exceptions when getDocumentElement()
 * is called on a document that has been wrapped or unwrapped because the root
 * element is not SOAP:Envelope.
 *
 * @author Sun Microsystems
 */
public class WrappedDocument implements org.w3c.dom.Document {
    
    /** Delegate reference to Document implementation. */
    Document doc_;
    /** Element reference to return from getDocumentElement() */
    Element root_;

    /** Create a new instance of WrappedDocument.
     *  @param doc Delegate reference to Document implementation
     *  @param root Element reference to return from getDocumentElement()
     */
    public WrappedDocument(Document doc, Element root)
    {
        doc_ = doc;
        root_ = root;
    }
    
    /** Always returns the reference passed in on the ctor instead of delegating
     *  the call.
     */
    public Element getDocumentElement() {
        return root_;
    } 

    public Node importNode(Node importedNode, boolean deep) throws DOMException {
        return doc_.importNode(importedNode, deep);
    }

    public void setXmlStandalone(boolean xmlStandalone) throws DOMException {
        doc_.setXmlStandalone(xmlStandalone);
    }

    public void setStrictErrorChecking(boolean strictErrorChecking) {
        doc_.setStrictErrorChecking(strictErrorChecking);
    }

    public Node cloneNode(boolean deep) {
        return doc_.cloneNode(deep);
    }

    public Object setUserData(String key, Object data, UserDataHandler handler) {
        return doc_. setUserData(key, data, handler);
    }

    public Node renameNode(Node n, String namespaceURI, String qualifiedName) throws DOMException {
        return doc_.renameNode(n, namespaceURI, qualifiedName);
    }

    public void setXmlVersion(String xmlVersion) throws DOMException {
        doc_.setXmlVersion(xmlVersion);
    }

    public void setTextContent(String textContent) throws DOMException {
        doc_.setTextContent(textContent) ;
    }

    public void setPrefix(String prefix) throws DOMException {
        doc_.setPrefix(prefix);
    }

    public void setNodeValue(String nodeValue) throws DOMException {
        doc_.setNodeValue(nodeValue) ;
    }

    public void setDocumentURI(String documentURI) {
        doc_.setDocumentURI(documentURI);
    }

    public String lookupPrefix(String namespaceURI) {
        return doc_.lookupPrefix(namespaceURI);
    }

    public String lookupNamespaceURI(String prefix) {
        return doc_.lookupNamespaceURI(prefix);
    }

    public NodeList getElementsByTagName(String tagname) {
        return doc_.getElementsByTagName(tagname);
    }

    public Element getElementById(String elementId) {
        return doc_.getElementById(elementId);
    }

    public Text createTextNode(String data) {
        return doc_.createTextNode(data);
    }

    public Attr createAttribute(String name) throws DOMException {
        return doc_.createAttribute(name);
    }

    public CDATASection createCDATASection(String data) throws DOMException {
        return doc_.createCDATASection(data);
    }

    public Comment createComment(String data) {
        return doc_.createComment(data);
    }

    public Element createElement(String tagName) throws DOMException {
        return doc_.createElement(tagName);
    }

    public EntityReference createEntityReference(String name) throws DOMException {
        return doc_.createEntityReference(name);
    }

    public Object getUserData(String key) {
        return doc_.getUserData(key);
    }


    public Node adoptNode(Node source) throws DOMException {
        return doc_.adoptNode(source);
    }
    
    public void normalizeDocument() {
        doc_.normalizeDocument();
    }


    public NodeList getElementsByTagNameNS(String namespaceURI, String localName) {
        return doc_.getElementsByTagNameNS(namespaceURI, localName);
    }

    public DOMConfiguration getDomConfig() {
        return doc_.getDomConfig();
    }

    public String getDocumentURI() {
        return doc_.getDocumentURI();
    }

    public DocumentType getDoctype() {
        return doc_.getDoctype() ;
    }


    public Attr createAttributeNS(String namespaceURI, String qualifiedName) throws DOMException {
        return doc_.createAttributeNS(namespaceURI, qualifiedName) ;
    }

    public DocumentFragment createDocumentFragment() {
        return  doc_.createDocumentFragment();
    }

    public Element createElementNS(String namespaceURI, String qualifiedName) throws DOMException {
        return doc_.createElementNS(namespaceURI, qualifiedName);
    }

    public ProcessingInstruction createProcessingInstruction(String target, String data) throws DOMException {
        return doc_.createProcessingInstruction(target, data) ;
    }

    public DOMImplementation getImplementation() {
        return  doc_.getImplementation();
    }

    public String getInputEncoding() {
        return  doc_.getInputEncoding();
    }

    public boolean getStrictErrorChecking() {
        return doc_.getStrictErrorChecking();
    }

    public String getXmlEncoding() {
        return doc_.getXmlEncoding();
    }

    public boolean getXmlStandalone() {
        return doc_.getXmlStandalone();
    }

    public String getXmlVersion() {
        return doc_.getXmlVersion();
    }

    public Object getFeature(String feature, String version) {
        return doc_.getFeature(feature, version);
    }

    public boolean isSameNode(Node other) {
        return doc_.isSameNode(other);
    }

    public boolean isEqualNode(Node arg) {
        return doc_.isEqualNode(arg);
    }

    public boolean isDefaultNamespace(String namespaceURI) {
        return doc_.isDefaultNamespace(namespaceURI);
    }

    public short compareDocumentPosition(Node other) throws DOMException {
        return doc_.compareDocumentPosition(other);
    }

    public Node appendChild(Node newChild) throws DOMException {
        return doc_.appendChild(newChild);
    }

    public Node removeChild(Node oldChild) throws DOMException {
        return doc_.removeChild(oldChild);
    }

    public String getNodeValue() throws DOMException {
        return doc_.getNodeValue() ;
    }

    public short getNodeType() {
        return doc_.getNodeType();
    }

    public String getNodeName() {
        return doc_.getNodeName() ;
    }

    public Node getNextSibling() {
        return doc_.getNextSibling() ;
    }

    public String getNamespaceURI() {
        return doc_.getNamespaceURI();
    }

    public String getLocalName() {
        return doc_.getLocalName();
    }

    public Node getLastChild() {
        return doc_.getLastChild();
    }

    public Node getFirstChild() {
        return doc_.getFirstChild();
    }

    public NodeList getChildNodes() {
        return doc_.getChildNodes();
    }

    public String getBaseURI() {
        return doc_.getBaseURI();
    }

    public NamedNodeMap getAttributes() {
        return doc_.getAttributes();
    }

    public Document getOwnerDocument() {
        return doc_.getOwnerDocument() ;
    }

    public Node getParentNode() {
        return doc_.getParentNode();
    }

    public String getPrefix() {
        return doc_.getPrefix();
    }

    public Node getPreviousSibling() {
        return doc_.getPreviousSibling();
    }

    public String getTextContent() throws DOMException {
        return doc_.getTextContent() ;
    }

    public boolean hasAttributes() {
        return doc_.hasAttributes();
    }

    public boolean hasChildNodes() {
        return doc_.hasChildNodes();
    }

    public Node insertBefore(Node newChild, Node refChild) throws DOMException {
        return doc_.insertBefore(newChild, refChild);
    }

    public boolean isSupported(String feature, String version) {
        return doc_.isSupported(feature, version);
    }

    public void normalize() {
        doc_.normalize();
    }

    public Node replaceChild(Node newChild, Node oldChild) throws DOMException {
        return doc_.replaceChild(newChild, oldChild) ;
    }
}
