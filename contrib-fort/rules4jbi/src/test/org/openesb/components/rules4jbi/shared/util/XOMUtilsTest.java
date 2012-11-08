/*
 * @(#)XOMUtilsTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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

import nu.xom.Attribute;
import nu.xom.Element;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class XOMUtilsTest {

    @Test
    public void toElement() {
        Element expected = new Element("hello");
        Element result = XOMUtils.toElement("<hello/>");
        assertEquals(expected, result);
        
        expected = new Element("hello", "http://namespaceUri");
        result = XOMUtils.toElement("<hello xmlns='http://namespaceUri'/>");
        assertEquals(expected, result);

        expected = new Element("xs:hello", "http://namespaceUri");
        result = XOMUtils.toElement("<xs:hello xmlns:xs='http://namespaceUri'/>");
        assertEquals(expected, result);
        
        expected = new Element("hello", "http://namespaceUri");
        expected.setNamespacePrefix("xs");
        expected.addAttribute(new Attribute("xs:id", "http://namespaceUri", "3"));
        
        result = XOMUtils.toElement("<xs:hello xmlns:xs='http://namespaceUri' xs:id='3' />");
        assertEquals(expected, result);

        expected.appendChild("text 123");
        result = XOMUtils.toElement("<xs:hello xs:id='3' xmlns:xs='http://namespaceUri'>text 123</xs:hello>");
        assertEquals(expected, result);
        
        String schema = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                      + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                      + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                      + "<xs:element name='invoice' type='tns:invoiceType'/>"
                      + "<xs:complexType name='invoiceType'/>"
                      + "</xs:schema>";

        expected = new Element("xs:schema", "http://www.w3.org/2001/XMLSchema");
        expected.addNamespaceDeclaration("tns", "http://www.example.org/xml/ns/test");
        expected.addAttribute(new Attribute("targetNamespace", "http://www.example.org/xml/ns/test"));
        expected.addAttribute(new Attribute("version", "1.0"));
        expected.addAttribute(new Attribute("elementFormDefault", "qualified"));
        
        Element child = new Element("xs:element", "http://www.w3.org/2001/XMLSchema");
        child.addAttribute(new Attribute("name", "invoice"));
        child.addAttribute(new Attribute("type", "tns:invoiceType"));
        expected.appendChild(child);

        child = new Element("xs:complexType", "http://www.w3.org/2001/XMLSchema");
        child.addAttribute(new Attribute("name", "invoiceType"));
        expected.appendChild(child);
        
        result = XOMUtils.toElement(schema);
        assertEquals(expected, result);
    }
}