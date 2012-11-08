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

package com.sun.jbi.jmsbc;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamResult;

/**
 * This is the utility class to handle XML data.
 *
 * @author      Sun developers
 */
public class XmlUtil {
    
    
    /**
     * Creates a Document object with InputSource.
     *
     * @param builder         DocumentBuilder instance used to build the document
     * @param namespaceAware  namespaceAware flag
     * @param source          InputSource object
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocumentFromInputSource(DocumentBuilder builder,
                                          InputSource source)
                                          throws SAXException, IOException, Exception {
        Document document = builder.parse(source);
        return document;
    }

    /**
     * Creates a Document object with String based XML data
     *
     * @param builder         DocumentBuilder instance used to build the document
     * @param namespaceAware  namespaceAware flag
     * @param xml             XML data
     * @return                Document object
     * @exception             exception upon error
     */
    public static Document createDocumentFromXML(DocumentBuilder builder,
                                                 String xml)
                                                 throws SAXException, IOException, Exception {
        return createDocumentFromInputSource (builder, 
                                              new InputSource(new StringReader(xml)));
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
     * @param trans                     Transformer instance used to convert DOM node
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Transformer trans,
                                          Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        resetOuputProperties(trans);// reset anything previously set
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
     * @param trans                     Transformer instance used to convert DOM node
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Transformer trans,
                                           Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration,
                                           String method) 
                                           throws Exception {
        String xmlData = null;
        
        
        if(node.getNodeType() == Node.TEXT_NODE){
        	return node.getTextContent();
        }
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        resetOuputProperties(trans);// reset anything previously set
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    /**
     * Serializes DOM node to String.
     *
     * @param trans                     Transformer instance used to convert DOM node
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Transformer trans,
                                           Node node, 
                                           String encoding, 
                                           boolean omitXMLDeclaration) 
                                           throws Exception {
        String xmlData = transformToString(trans,node,encoding,omitXMLDeclaration,"xml");        
        return xmlData;
    }

    /**
     * Serializes DOM node to String.
     *
     * @param trans                     Transformer instance used to convert DOM node
     * @param src  			Source input
     * @param encoding 			encoding style
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        String representation of the DOM node
     * @exception            		exception upon error
     */
    public static String transformToString(Transformer trans,
                                           Source src, 
                                           String encoding, 
                                           boolean omitXMLDeclaration) 
                                           throws Exception {
        String xmlData = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        resetOuputProperties(trans);// reset anything previously set
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "no");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(src, new StreamResult(baos));
        xmlData = baos.toString(encoding);
        
        return xmlData;
    }
    
    /**
     * Serializes DOM node to an array of bytes.
     *
     * @param trans                     Transformer instance used to convert DOM node
     * @param node  			DOM node
     * @param encoding 			encoding style
     * @param method	                method to write the result
     * @param omitXMLDeclaration	omitXMLDeclaration flag
     * @return      		        byte array representation of the DOM node
     * @exception            		exception upon error
     */
    public static byte[] transformToBytes(Transformer trans,
                                          Node node, 
                                          String encoding,
                                          boolean omitXMLDeclaration,
                                          String method) 
                                          throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
 
        resetOuputProperties(trans);// reset anything previously set
        trans.setOutputProperty(OutputKeys.ENCODING, encoding);
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, method);
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
        trans.transform(new DOMSource(node), new StreamResult(baos));
        
        return baos.toByteArray();
    }
    
    /**
     * Process an input XML source tree into a transformed DOM
     *
     * @param trans                     Transformer instance used to convert DOM node
     * @param source  			XML input source
     * @param docForResult              Document to place the DOM tree
     * @return      		        transformation result
     * @exception            		exception upon error
     */    
    public static DOMResult transformToDOMResult(Transformer trans, Source source, Document docForResult) 
    throws Exception {
    	DOMResult result = new DOMResult(docForResult);
    	
    	resetOuputProperties(trans);// reset anything previously set
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.transform(source, result);
        
        return result;
    }    

    /**
     * Process an input XML source tree into a transformed DOM
     *
     * @param trans                     Transformer instance used to convert DOM node
     * @param source  			XML input source
     * @return      		        transformation result
     * @exception            		exception upon error
     */    
    public static DOMResult transformToDOMResult(Transformer trans, Source source) 
    throws Exception {
    	DOMResult result = new DOMResult();
    	
    	resetOuputProperties(trans);// reset anything previously set
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.transform(source, result);
        
        return result;
    }        
    
    /**
     * reset transformer output properties.
     * Sun JDK Implementation com.sun.org.apache.xalan.internal.xsltc.trax.TransformerImpl reverts the output properties 
     * to properties set in the Templates with xsl:output on calling setOutputProperties(null),
     * but latest apache implementaion sets it to null and causes nullpointer exception on next call to set Outputproperty.
     * Added this check to support ibm implementation on AIX.
     */
    private static void resetOuputProperties(Transformer trans) {
    	trans.setOutputProperties(null);
    	try{
    		if(trans.getOutputProperties() == null)
    			trans.setOutputProperties(new Properties());
    	} catch (NullPointerException exp) {
    		trans.setOutputProperties(new Properties());
    	}
    }
}
