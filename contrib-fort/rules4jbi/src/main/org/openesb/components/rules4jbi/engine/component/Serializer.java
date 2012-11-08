/*
 * @(#)Serializer.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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

package org.openesb.components.rules4jbi.engine.component;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMImplementation;

import nu.xom.Element;
import nu.xom.converters.DOMConverter;

/**
 * This class is used to serialize Objects to XML and vice versa.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class Serializer {
    
    private Class<?>[] types;

    public Serializer(Class<?>[] types) {
        this.types = types;
    }
    
    private boolean isTypeRegistered(Class<?> clazz) {
        for (int i = 0; i < types.length; i++) {
            if (types[i] == clazz) {
                return true;
            }
        }

        return false;
    }
    
    public Element serialize(Object obj) {
        if (!isTypeRegistered(obj.getClass())) {
            throw new IllegalArgumentException(
                    "Class " + obj.getClass().getName() + " is not registered with this serializer");
        }
        
        try {
            JAXBContext context = JAXBContext.newInstance(obj.getClass());
            Marshaller marshaller = context.createMarshaller();
//            marshaller.setProperty("jaxb.formatted.output", Boolean.TRUE);
            
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            org.w3c.dom.Document domDocument = db.newDocument();

            marshaller.marshal(obj, domDocument);

            nu.xom.Document xomDocument = DOMConverter.convert(domDocument);
            Element rootElement = xomDocument.getRootElement();

            return (Element) rootElement.copy();
            
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
            
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
    
    public Object deserialize(Element element) {
        /* defensive copy to remove the element from a parent, if it has any */
        nu.xom.Document xomDocument = new nu.xom.Document((Element) element.copy());
        
        try {
            JAXBContext context = JAXBContext.newInstance(types);
            Unmarshaller unmarshaller = context.createUnmarshaller();

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            DOMImplementation domImplementation = db.getDOMImplementation();

            org.w3c.dom.Document domDocument = DOMConverter.convert(xomDocument, domImplementation);

            Object result = unmarshaller.unmarshal(domDocument);

            return result;
            
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
            
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
    
//    public static void main(String[] args) {
//        Serializer serializer = new Serializer(new Class<?>[] {Customer.class, Invoice.class});
//        
//        Customer customer = new Customer();
//        customer.setName("Johnny Bravo");
//        customer.setCreditLimit(500);
//        System.out.println(customer);
//        
//        Element customerElement = serializer.serialize(customer);
//        XOMUtils.prettyPrint(customerElement);
//        
//        Object obj = serializer.deserialize(customerElement);
//        System.out.println(obj);
//    }
}
