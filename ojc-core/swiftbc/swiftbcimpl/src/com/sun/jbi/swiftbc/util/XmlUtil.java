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
 * @(#)XmlUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * This is the utility class to handle XML data.
 *
 * @author      Sun developers
 */
public class XmlUtil {
    
    /**
     * Creates a Document object
     *
     * @param namespaceAware  namespaceAware flag
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocument(boolean namespaceAware)
        throws Exception {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(namespaceAware);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();

        return document;
    }
    
    /**
     * Creates a Document object with InputSource.
     *
     * @param namespaceAware  namespaceAware flag
     * @param source          InputSource object
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocument(boolean namespaceAware,
                                           InputSource source)
                                           throws SAXException, IOException, Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(source);

        return document;
    }

    /**
     * Creates a Document object with String based XML data
     *
     * @param namespaceAware  namespaceAware flag
     * @param xml             XML data
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocumentFromXML(boolean namespaceAware,
                                                 String xml)
                                                 throws SAXException, IOException, Exception {
        return createDocument(namespaceAware, new InputSource(new StringReader(xml)));
    }

    /**
     * Gets the text attribute of a DOM node
     *
     * @param node  Description of the Parameter
     * @return      The text value
     */
    public static String getText(Node node) {
        StringBuffer sb = new StringBuffer();
        NodeList children = node.getChildNodes();
        for (int i = 0, length = children.getLength(); i < length; i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                sb.append(child.getNodeValue());
            }
        }
        return sb.toString();
    }

    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = tFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        
        return baos.toByteArray();
    }
    
    /**
     * Serializes DOM node to String.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration) 
                                           throws Exception {
        String xmlData = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        Transformer trans = TransformerFactory.newInstance().newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration,
                                          String method) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = tFactory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        
        return baos.toByteArray();
    }
    
    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration,
                                           String method) 
                                           throws Exception {
        String xmlData = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        Transformer trans = TransformerFactory.newInstance().newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    public static DOMResult transformToDOMResult(Source source) throws Exception {
    	DOMResult result = new DOMResult();
    	
        Transformer trans = TransformerFactory.newInstance().newTransformer();
        trans.transform(source, result);
        
        return result;
    }
}
