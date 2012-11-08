/*
 * @(#)MainSchemaCreatorTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.schema;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import nu.xom.Element;
import org.junit.Test;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class MainSchemaCreatorTest {

    @Test
    public void createSchemaElement() {
        String expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "</xs:schema>";
        
        Element result = MainSchemaCreator.createSchemaElement("http://www.example.org/xml/ns/test");
        assertEquals(XOMUtils.toElement(expected), result);
        
        expected = "<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema' "
                + "targetNamespace='http://example.org/whatnot' "
                + "xmlns:tns='http://example.org/whatnot' "
                + "elementFormDefault='qualified' " 
                + "           version='1.0'/>";
        
        result = MainSchemaCreator.createSchemaElement("http://example.org/whatnot");
        assertEquals(XOMUtils.toElement(expected), result);
    }

    @Test
    public void createImportElement() {
        String expected = "<xs:import xmlns:xs='http://www.w3.org/2001/XMLSchema' "
                + "namespace='http://www.example.org/xml/ns/test' />";
        
        Element result = MainSchemaCreator.createImportElement("http://www.example.org/xml/ns/test");
        assertEquals(XOMUtils.toElement(expected), result);
        
        expected = "<xs:import namespace='http://example.org/whatnot' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema'/>";
        
        result = MainSchemaCreator.createImportElement("http://example.org/whatnot");
        assertEquals(XOMUtils.toElement(expected), result);
    }
    
    @Test
    public void addImports() {
        String input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        String expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://example.org/ns1'/>"
                + "<xs:import namespace='http://example.org/ns2'/>"
                + "<xs:import namespace='http://example.org/ns3'/>"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        List<String> namespaces = new ArrayList<String>();
        
        namespaces.add("http://example.org/ns1");
        namespaces.add("http://example.org/ns2");
        namespaces.add("http://example.org/ns3");
        
        Element result = MainSchemaCreator.addImports(XOMUtils.toElement(input), namespaces);
        assertEquals(XOMUtils.toElement(expected), result);
    }
    
    @Test
    public void addNamespaceDeclarations() {
        String input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        String expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0' "
                + "xmlns:ns1='http://example.org/ns1' "
                + "xmlns:ns2='http://example.org/ns2' "
                + "xmlns:ns3='http://example.org/ns3'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        Map<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("ns1", "http://example.org/ns1");
        namespaces.put("ns2", "http://example.org/ns2");
        namespaces.put("ns3", "http://example.org/ns3");
        
        Element result = MainSchemaCreator.addNamespaceDeclarations(XOMUtils.toElement(input), namespaces);
        assertEquals(XOMUtils.toElement(expected), result);
    }
    
    @Test
    public void createElementReference() {
        String expected = "<xs:element xmlns:xs='http://www.w3.org/2001/XMLSchema' ref='a:foo' maxOccurs='12'/>";
        Element result = MainSchemaCreator.createElementReference("a:foo", 12);
        assertEquals(XOMUtils.toElement(expected), result);

        expected = "<xs:element xmlns:xs='http://www.w3.org/2001/XMLSchema' ref='ns1:customer'/>";
        result = MainSchemaCreator.createElementReference("ns1:customer", 1);
        assertEquals(XOMUtils.toElement(expected), result);
        
        expected = "<xs:element xmlns:xs='http://www.w3.org/2001/XMLSchema'"
                + " ref='ns1:customer' maxOccurs='unbounded'/>";
        
        result = MainSchemaCreator.createElementReference("ns1:customer", Integer.MAX_VALUE);
        assertEquals(XOMUtils.toElement(expected), result);
    }
}
