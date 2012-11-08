/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.common;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This object provides utility methods to manipulate DOM tree
 *
 * @author Sun Microsystems, Inc.
 */
public class DOMUtil {
    /** static object to access the methods of this object */
    public static final DOMUtil UTIL = new DOMUtil();
    /** Creates a new instance of DOMUtil */
    public DOMUtil() {
    }
    
    /** gets the element
     * @return Element with tagname
     * @param aParentDocument Document for parent node
     * @param aTagName String for tagname
     */
    public Element getElement(Document aParentDocument, String aTagName) {
        NodeList nodeList = aParentDocument.getElementsByTagName(aTagName);
        return ( nodeList != null ) ? (Element) nodeList.item(0) : null;
    }
    
    /** get the child element
     * @param elem parent element
     * @param tagName element to look for
     * @return child element of type tagName
     */
    public Element getElement(Element elem, String tagName) {
        NodeList nl = elem.getElementsByTagName(tagName);
        Element childElem = (Element) nl.item(0);
        
        return childElem;
    }
    
    /** gets the element value
     * @param doc document
     * @param elemName element
     * @return value of the element
     */
    public String getElementValue(Document doc, String elemName) {
        String elemVal = null;
        
        Element elem = getElement(doc, elemName);
        elemVal = getTextData(elem);
        
        return elemVal;
    }
    
    /** use this util method to create/retrieve a Text Node in a element
     * @param aElement Element node
     * @return Text node for text data
     */
    private Text getText(Element aElement ) {
        Node node = null;
        aElement.normalize();
        node = aElement.getFirstChild();
        if ( node == null || !(node instanceof Text) ) {
            node = aElement.getOwnerDocument().createTextNode("");
            aElement.appendChild(node);
        }
        return (Text) node;
    }
    
    /** use this util method to set a Text Data in a element
     * @param aElement Element for text node
     * @param aData String contains text
     */
    public void setTextData(Element aElement,String aData) {
        getText(aElement).setData(aData);
    }
    
    /** use this util method to retrieve a Text Data in a element
     * @param aElement Element for text node
     * @return String contains text
     */
    public String getTextData(Element aElement) {
        return getText(aElement).getData();
    }
    
    /** save document to stream
     * @return xml text
     * @param aDocument Document
     * @param aWriter is what the aDocument is serialized to.
     * @throws Exception on error
     */
    public void DOM2Text( Document aDocument, Writer aWriter )
    throws Exception {
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer;
        try {
            transformer = transformerFactory.newTransformer();
        } catch (TransformerConfigurationException e) {
            transformer = null;
            throw e;
        }
        
        
        transformer.setOutputProperty(OutputKeys.METHOD,"xml");
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,"no");
        transformer.setOutputProperty(OutputKeys.STANDALONE,"yes");
        // transformer.setOutputProperty( OutputKeys.CDATA_SECTION_ELEMENTS, "" );
        transformer.setOutputProperty(OutputKeys.INDENT,"yes");
        try {
            transformer.transform(new DOMSource(aDocument),new StreamResult(aWriter));
        } catch (TransformerException e) {
            throw e;
        }
        
    }
    
    /** gets list of elements
     * @return NodeList with tagname
     * @param aParentElement Element for parent
     * @param aTagName String for tagname
     */
    public NodeList getElements(Element aParentElement, String aTagName) {
        return aParentElement.getElementsByTagNameNS(
                aParentElement.getNamespaceURI(), aTagName);
    }
    
    /** gets set of elements
     * @return NodeList with tagname
     * @param aParentDocument Document for parent node
     * @param aTagName String for tagname
     */
    public NodeList getElements(Document aParentDocument, String aTagName) {
        return aParentDocument.getElementsByTagNameNS("*",aTagName);
    }
    
    /** get the children of the same type element tag name
     * @param aElement Element for parent node
     *
     * @param aElementTagName String for tagname
     * @return NodeList for list of children with the tagname
     */
    public NodeList getChildElements(Element aElement, String aElementTagName) {
        NodeList nodeList = aElement.getChildNodes();
        NodeListImpl list = new NodeListImpl();
        int count = nodeList.getLength();
        for ( int i = 0; i < count; ++i ) {
            Node node = nodeList.item(i);
            if ( node instanceof Element ) {
                String tagName = getElementName((Element)node);
                if ( tagName.equals(aElementTagName) ) {
                    list.add(node);
                }
            }
        }
        return list;
    }
    
    /** get the child with the specified tag name
     * @param aElement Element for parent node
     *
     * @param aElementTagName String for tagname
     * @return NodeList for list of children with the tagname
     */
    public Element getChildElement(Element aElement, String aElementTagName) {
        NodeList list = getChildElements(aElement, aElementTagName);
        if ( list != null && list.getLength() > 0 ) {
            return (Element) list.item(0);
        } else {
            return null;
        }
    }
    /**
     * get Element Tag Name with striped prefix.
     * @param aElement Element object
     * @return String with stripped prefix
     */
    public String getElementName(Element aElement) {
        String tagName = aElement.getTagName();
        return getName(tagName);
    }
    
    /**
     * strips the prefix of the name if present
     * @param aName String value of Name with prefix
     * @return String for name after striping prefix
     */
    public String getName(String aName) {
        int lastIdx = aName.lastIndexOf(':');
        if ( lastIdx >= 0 ) {
            return aName.substring(lastIdx + 1);
        }
        return aName;
    }
    
    /**
     * return the DOM Document
     * @param xmlText String
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public Document buildDOMDocument(String xmlText, boolean validating ) throws Exception {
        StringReader reader = new StringReader(xmlText);
        return DOMUtil.UTIL.buildDOMDocument(reader, validating);
    }
    
    /**
     * return the DOM Document
     * @param xmlReader Reader
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public Document buildDOMDocument(Reader xmlReader, boolean validating ) throws Exception {
        Document xmlDoc = null;
        DocumentBuilderFactory docBuilderFactory =
                DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(validating);
        DocumentBuilder docBuilder =
                docBuilderFactory.newDocumentBuilder();
        docBuilder.setErrorHandler( new DefaultHandler() {
            public void fatalError(SAXParseException e)
            throws SAXException {
                throw new SAXException(e.getMessage());
            }
        });
        
        docBuilder.setEntityResolver(new EntityResolver() {
            public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
                StringReader reader = new StringReader("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N
                InputSource source = new InputSource(reader);
                source.setPublicId(publicId);
                source.setSystemId(systemId);
                return source;
            }
        });
        
        InputSource is = new InputSource(xmlReader);
        xmlDoc = docBuilder.parse(is);
        
        return xmlDoc;
    }
    
    /**
     * replaces the xml entity refereces with the xml escape chars
     * @param xmlString Text with the xml xml escape chars
     * @param Text with the xml entity refereces
     */
    public static String replaceXmlEscapeCharsToEntityRefereces(String xmlString) {
        if ( xmlString == null ) {
            return xmlString;
        }
        
        // just convert < , > and & only
        StringBuffer sbuff = new StringBuffer(2 * xmlString.length());
        for ( int i = 0; i < xmlString.length(); ++i ) {
            switch ( xmlString.charAt(i) ) {
                case '&': sbuff.append("&amp;");
                break;
                case '<': sbuff.append("&lt;");
                break;
                case '>': sbuff.append("&gt;");
                break;
                default: sbuff.append( xmlString.charAt(i) );
            }
        }
        return sbuff.toString();
    }
    
    /**
     * replaces the xml entity refereces with the xml escape chars
     * @param xmlString Text with the xml entity references
     * @param Text with the xml escape chars
     */
    public static String replaceXmlEntityReferecesToEscapeChars(String xmlString) {
        if ( xmlString == null ) {
            return xmlString;
        }
        // just look for these entity refereces only
        String[] entities = {"&lt;", "&gt;", "&amp;"};
        char[] escapeChar = { '<', '>', '&'};
        
        StringBuffer sbuff = new StringBuffer(xmlString.length());
        // sbuff.
        
        for ( int i = 0; i < xmlString.length(); ++i ) {
            char ch = xmlString.charAt(i);
            
            if ( ch != '&' ) {
                // not found &. append the ch and continue
//                System.out.println("Non & Char " + ch);
                sbuff.append(ch);
                continue;
            }
            
            String what = null;
            boolean found = false;
            int eIdx = 0;
            // found & char at i
            for (; eIdx < entities.length; ++eIdx ) {
                what = entities[eIdx];
                found = xmlString.startsWith(what, i);
                if ( found ) {
//                    System.out.println("Found Entity " + what);
                    break;
                }
            }
            
            if ( !found ) {
                // not found the entity
//                System.out.println("Entity Not found");
                continue;
            }
            
            // found the entity. so replace it with escape ch
//            System.out.println("Appending escape char " + escapeChar[eIdx]);
            sbuff.append(escapeChar[eIdx]);
//            System.out.println("Buff : " + sbuff.toString());
            // now increment i to entity.length-1 as the i will be incremented +1 in the for loop
            i += what.length()-1 ;
        }
        
        return sbuff.toString();
    }
    
    /**
     * NodeListImpl
     *
     */
    public static class NodeListImpl extends ArrayList implements NodeList {
        /** Default Constructor
         */
        public NodeListImpl() {
            super(); }
        /** nodelist length
         * @return int for number of nodes in nodelist
         */
        public int getLength() {
            return this.size();
        }
        /** return a node
         * @param aIndex int for the index of the node
         * @return Node at the index
         */
        public Node item(int aIndex) {
            return (Node) this.get(aIndex);
        }
    }
    
    
    
}
