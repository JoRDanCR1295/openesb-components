/*
 * @(#)XOMUtils.java        $Revision: 1.2 $ $Date: 2008/07/03 05:46:03 $
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

package org.openesb.components.rules4jbi.shared.util;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DocumentFragment;

import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;
import nu.xom.Node;
import nu.xom.Nodes;
import nu.xom.ParsingException;
import nu.xom.Serializer;
import nu.xom.ValidityException;
import nu.xom.converters.DOMConverter;

/**
 * Utilities that simplify working/testing with XOM.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/03 05:46:03 $
 * 
 * @since 0.1
 */
public final class XOMUtils {

    private XOMUtils() {}

    public static DocumentFragment elementToDocumentFragment(nu.xom.Element element) {
        org.w3c.dom.Document domDocument = elementToDocument(element);

        DocumentFragment fragment = domDocument.createDocumentFragment();
        fragment.appendChild(domDocument.getDocumentElement());

        return fragment;
    }

    public static org.w3c.dom.Document elementToDocument(nu.xom.Element element) {
        Element localCopy = (Element) element.copy();
        
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            DOMImplementation domImplementation = builder.getDOMImplementation();

            Document document = new Document(localCopy);
            org.w3c.dom.Document domDocument = DOMConverter.convert(document, domImplementation);

            return domDocument;

        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        }
    }
    
    public static void prettyPrint(Element element) {
        prettyPrint(new Document(element));
    }

    public static void prettyPrint(Element element, int maxLength) {
        prettyPrint(new Document(element), maxLength);
    }
    
    public static void prettyPrint(Document document, int maxLength) {
        try {
            Serializer serializer = new Serializer(System.out, "ISO-8859-1");
            serializer.setIndent(4);
            serializer.setMaxLength(maxLength);
            serializer.write(document);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void prettyPrint(Document document) {
        prettyPrint(document, 105);
    }
    
    public static Document toDocument(Source source) {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();
//            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
//            transformer.setOutputProperty(OutputKeys.INDENT, indent ? "yes" : "no");

            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            
            transformer.transform(source, result);
            
            Builder builder = new Builder();
            Document document = builder.build(writer.toString(), null);

            return document;
            
        } catch (ValidityException e) {
            throw new RuntimeException(e);
            
        } catch (ParsingException e) {
            throw new RuntimeException(e);
            
        } catch (IOException e) {
            throw new RuntimeException(e);
            
        } catch (TransformerConfigurationException e) {
            throw new RuntimeException(e);

        } catch (TransformerException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Converts the specified XML string into <code>nu.xom.Element</code> object.
     * The XML string may contain the xml declaration, which will be ignored;
     * i.e. only the root element will be extracted.
     * 
     * @param element XML string to be converted.
     * @return <code>nu.xom.Element</code> representing the passed XML string.
     */
    public static Element toElement(String element) {
        try {
            Builder builder = new Builder();
            
//            Document document = builder.build(new StringReader(element));
            Document document = builder.build(element, null);
            
            Element root = document.getRootElement();
            
            return (Element) root.copy();
            
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    //TODO: add junit test
    public static List<Node> asList(Nodes nodes) {
        List<Node> result = new ArrayList<Node>(nodes.size());

        for (int i = 0; i < nodes.size(); i++) {
            result.add(nodes.get(i));
        }

        return result;
    }
    
    //TODO: add junit test
    public static List<Element> asList(Elements elements) {
        List<Element> result = new ArrayList<Element>(elements.size());

        for (int i = 0; i < elements.size(); i++) {
            result.add(elements.get(i));
        }

        return result;
    }
    
    public static void main(String[] args) {
        String customer = "<customer xmlns='http://www.example.org/xml/ns/whatever'>"
            + "<name>Johnny Bravo</name><creditLimit>500</creditLimit></customer>";
        
        Element element = toElement(customer);
        
        System.out.println("Got element: " + element.toXML());
        
        org.w3c.dom.Document domDocument = elementToDocument(element);
        
        DOMSource domSource = new DOMSource(domDocument);
        
        System.out.println("Got DOMSource: " + domSource);

        Document document = toDocument(domSource);
        
        System.out.println("Result: " + document.toXML());
    }
}
