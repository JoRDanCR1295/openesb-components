/*
 * @(#)DOMUtils.java        $Revision: 1.4 $ $Date: 2008/10/25 22:04:10 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.util;

import java.io.StringWriter;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DocumentFragment;

import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;

/**
 * Various DOM utility methods.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/10/25 22:04:10 $
 * 
 * @since 0.1
 */
public final class DOMUtils {
    
    private DOMUtils() {}
    
    public static String documentFragmentToString(DocumentFragment fragment) {
        return documentFragmentToString(fragment, false);
    }
    
    public static String documentFragmentToString(DocumentFragment fragment, boolean indent) {
        return domSourceToString(new DOMSource(fragment), indent);
    }

    public static String documentToString(org.w3c.dom.Document document) {
        return documentToString(document, false);
    }
    
    public static String documentToString(org.w3c.dom.Document document, boolean indent) {
        return domSourceToString(new DOMSource(document), indent);
    }
    
    public static String domSourceToString(DOMSource domSource, boolean indent) {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.setOutputProperty(OutputKeys.INDENT, indent ? "yes" : "no");
            
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            transformer.transform(domSource, result);
            
            return writer.toString();
            
        } catch (TransformerConfigurationException e) {
            throw new RuntimeException(e);
            
        } catch (TransformerException e) {
            throw new RuntimeException(e);
        }
    }
    
    public static void main(String[] args) throws ParserConfigurationException {
        Element root = new Element("root", "http://www.example.org/xml/abc");
        root.setNamespacePrefix("abc");
        Element child = new Element("child", "http://www.example.org/xml/abc");
        root.appendChild(child);
        child.setNamespacePrefix("abc");
        Attribute childAttribute = new Attribute("abc:id", "http://www.example.org/xml/abc", "123");
        child.addAttribute(childAttribute);

        Document document = new Document(root);
        
        System.out.println(document.toXML());
        
        String documentString = documentToString(XOMUtils.elementToDocument(root), true);
        System.out.println(documentString);
        
        String fragmentString = documentFragmentToString(XOMUtils.elementToDocumentFragment(root), true);
        System.out.println(fragmentString);
    }
}
