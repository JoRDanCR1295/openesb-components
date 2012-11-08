/*
 * @(#)SecondarySchemaCreatorTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
import nu.xom.Element;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class SecondarySchemaCreatorTest {

    @Test
    public void removeSchemaLocationFromImports() {
        String input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "</xs:schema>";
        
        Element result = SecondarySchemaCreator.removeSchemaLocationFromImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(input), result);
        
        input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "</xs:schema>";
        
        String expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' />"
                + "</xs:schema>";
        
        result = SecondarySchemaCreator.removeSchemaLocationFromImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
        
        input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='http://whereever.com/abc.xsd' />"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' />"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        result = SecondarySchemaCreator.removeSchemaLocationFromImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
        
        input = "<xsd:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xsd='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test'>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "<xsd:element name='client' type='ns1:customerType' />"
                + "<xsd:complexType name='invoice'><xsd:sequence><xsd:element name='amount' type='xsd:int' />"
                + "<xsd:element minOccurs='0' name='description' type='xsd:string' />"
                + "<xsd:element minOccurs='0' name='status' type='xsd:string' /></xsd:sequence></xsd:complexType>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever2' "
                + "schemaLocation='../../righthere' />"
                + "<xsd:element name='invoice' type='tns:invoice' />"
                + "</xsd:schema>";
        
        expected = "<xsd:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xsd='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test'>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever' />"
                + "<xsd:element name='client' type='ns1:customerType' />"
                + "<xsd:complexType name='invoice'><xsd:sequence><xsd:element name='amount' type='xsd:int' />"
                + "<xsd:element minOccurs='0' name='description' type='xsd:string' />"
                + "<xsd:element minOccurs='0' name='status' type='xsd:string' /></xsd:sequence></xsd:complexType>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever2' "
                + " />"
                + "<xsd:element name='invoice' type='tns:invoice' />"
                + "</xsd:schema>";
        
        result = SecondarySchemaCreator.removeSchemaLocationFromImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
    }
    
    @Test
    public void removeImports() {
        String input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "</xs:schema>";
        
        Element result = SecondarySchemaCreator.removeImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(input), result);
        
        input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "</xs:schema>";
        
        String expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "</xs:schema>";
        
        result = SecondarySchemaCreator.removeImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
        
        input = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        expected = "<xs:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xs='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test' version='1.0'>"
                + "<xs:element name='client' type='ns1:customerType' />"
                + "</xs:schema>";
        
        result = SecondarySchemaCreator.removeImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
        
        input = "<xsd:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xsd='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test'>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever' "
                + "schemaLocation='../rightthere' />"
                + "<xsd:element name='client' type='ns1:customerType' />"
                + "<xsd:complexType name='invoice'><xsd:sequence><xsd:element name='amount' type='xsd:int' />"
                + "<xsd:element minOccurs='0' name='description' type='xsd:string' />"
                + "<xsd:element minOccurs='0' name='status' type='xsd:string' /></xsd:sequence></xsd:complexType>"
                + "<xsd:import namespace='http://www.example.org/xml/ns/whatever2' "
                + "schemaLocation='../../righthere' />"
                + "<xsd:element name='invoice' type='tns:invoice' />"
                + "</xsd:schema>";
        
        expected = "<xsd:schema xmlns:tns='http://www.example.org/xml/ns/test' "
                + "xmlns:xsd='http://www.w3.org/2001/XMLSchema' elementFormDefault='qualified' "
                + "targetNamespace='http://www.example.org/xml/ns/test'>"
                + "<xsd:element name='client' type='ns1:customerType' />"
                + "<xsd:complexType name='invoice'><xsd:sequence><xsd:element name='amount' type='xsd:int' />"
                + "<xsd:element minOccurs='0' name='description' type='xsd:string' />"
                + "<xsd:element minOccurs='0' name='status' type='xsd:string' /></xsd:sequence></xsd:complexType>"
                + "<xsd:element name='invoice' type='tns:invoice' />"
                + "</xsd:schema>";
        
        result = SecondarySchemaCreator.removeImports(XOMUtils.toElement(input));
        assertEquals(XOMUtils.toElement(expected), result);
    }
}
