/*
 * @(#)MainSchemaCreator.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nu.xom.Attribute;
import nu.xom.Element;

import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class MainSchemaCreator {

    private final BusinessObjectManager businessObjectManager;
    
    private final NamespacePrefixGenerator generator;
    
    private Element schema;
    
    public MainSchemaCreator(BusinessObjectManager businessObjectManager) {
        this.businessObjectManager = businessObjectManager;
        
        generator = new NamespacePrefixGenerator(businessObjectManager.getElementNames());
    }
    
    public Element createSchema() {
        schema = createSchemaElement(WSDLConstants.TYPES_NAMESPACE_URI);
        
        schema = addNamespaceDeclarations(schema, generator.getPrefixes());
        
        schema = addImports(schema, generator.getNamespaces());
        
        Iterator<BusinessObject> inputObjects = businessObjectManager.inputIterator();
        schema.appendChild(
                createDataElement(WSDLConstants.INPUT_ELEMENT_NAME, createElementReferences(inputObjects)));
        
        Iterator<BusinessObject> outputObjects = businessObjectManager.outputIterator();
        schema.appendChild(
                createDataElement(WSDLConstants.OUTPUT_ELEMENT_NAME, createElementReferences(outputObjects)));
        
        return schema;
    }
    
    List<Element> createElementReferences(Iterator<BusinessObject> iterator) {
        List<Element> elementReferences = new ArrayList<Element>();
        
        while (iterator.hasNext()) {
            BusinessObject businessObject = iterator.next();
            
            String prefixedLocalName = generator.getPrefixedLocalName(businessObject.getElementName());
            
            Element elementReference = createElementReference(prefixedLocalName, businessObject.getCardinality());
            
            elementReferences.add(elementReference);
        }
        
        return elementReferences;
    }
    
    static Element createDataElement(String elementName, List<Element> elementReferences) {
        Element result = new Element("xs:element", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);
        
        Attribute name = new Attribute("name", elementName);
        result.addAttribute(name);
        
        Element complexType = new Element("xs:complexType",  WSDLConstants.XML_SCHEMA_NAMESPACE_URI);
        
        Element sequence = new Element("xs:sequence", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        for (Element element : elementReferences) {
            sequence.appendChild(element);
        }
        
        complexType.appendChild(sequence);
        
        result.appendChild(complexType);
        
        return result;
    }
    
    /**
     * Creates this xml element: <xs:element ref="a:transport" maxOccurs="10" />.
     * 
     * @param prefixedName
     * @param cardinality
     * @return
     */
    static Element createElementReference(String prefixedName, int cardinality) {

        //TODO: change to assert statements
        if (cardinality <= 0) {
            throw new IllegalArgumentException("Cardinality must be greater than zero");
        }

        if (prefixedName == null) {
            throw new IllegalArgumentException("Prefixed name cannot be null");
        }

        if (!prefixedName.contains(":")) {
            throw new IllegalArgumentException("Prefixed name must contain colon");
        }

        Element element = new Element("xs:element", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        Attribute ref = new Attribute("ref", prefixedName);
        element.addAttribute(ref);

        /* The default value for both minOccurs and maxOccurs is 1 */
        if (cardinality != 1) {
            Attribute maxOccurs = new Attribute("maxOccurs",
                    cardinality == Integer.MAX_VALUE ? "unbounded" : Integer.toString(cardinality));
            
            element.addAttribute(maxOccurs);
        }

        return element;
    }

    static Element addImports(Element schema, List<String> namespaces) {
        assert schema != null;
        assert schema.getLocalName().equals("schema");
        assert schema.getNamespaceURI().equals(WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        Element result = (Element) schema.copy();

        for (int i = 0; i < namespaces.size(); i++) {
            String namespace = namespaces.get(i);
            result.insertChild(createImportElement(namespace), i);
        }
        
        return result;
    }
    
    static Element createImportElement(String namespace) {
        Element importElement = new Element("xs:import", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        Attribute namespaceAttribute = new Attribute("namespace", namespace);
        importElement.addAttribute(namespaceAttribute);

        return importElement;
    }
    
    static Element addNamespaceDeclarations(Element schema, Map<String, String> namespaces) {
        assert schema != null;
        assert schema.getLocalName().equals("schema");
        assert schema.getNamespaceURI().equals(WSDLConstants.XML_SCHEMA_NAMESPACE_URI);
        
        Element result = (Element) schema.copy();
        
        for (String prefix : namespaces.keySet()) {
            result.addNamespaceDeclaration(prefix, namespaces.get(prefix));
        }
        
        return result;
    }
    
    static Element createSchemaElement(String targetNamespace) {
        Element schema = new Element("xs:schema", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        Attribute version = new Attribute("version", "1.0");
        schema.addAttribute(version);

        Attribute elementFormDefault = new Attribute("elementFormDefault", "qualified");
        schema.addAttribute(elementFormDefault);

        Attribute targetNS = new Attribute("targetNamespace", targetNamespace);
        schema.addAttribute(targetNS);

        //TODO: do we really need tns?
        schema.addNamespaceDeclaration("tns", targetNamespace);

        return schema;
    }
    
//    public static void main(String[] args) {
//        BusinessObjectManager manager = new BusinessObjectManager();
//
//        manager.addInputObject(Customer.class, 1);
//        manager.addInputObject(Invoice.class, 2);
//        manager.addOutputObject(Invoice.class, 5);
//        
//        MainSchemaCreator generator = new MainSchemaCreator(manager);
//        
//        XOMUtils.prettyPrint(generator.createSchema());
//    }
}
