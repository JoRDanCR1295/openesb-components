/*
 * @(#)BusinessObject.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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


import java.lang.reflect.Constructor;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.JAXBIntrospector;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.namespace.QName;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerImpl;

/**
 * This class represents a business object that is used as an input and/or output to a rule session.
 * A class must fulfill the following conditions, in order to be usable as a business object:
 * <p>
 * <ul> 
 *   <li>it has a no-arg constructor
 *   <li>it is annotated with the @XmlRootElement annotation
 *   <li>it belongs to a (xml) namespace
 * </ul>
 * <p>
 * Using JAXB2, you can associate a class and its components with a namespace through the @XmlRootElement,
 * @XmlType, @XmlElement and @XmlAttribute annotations, or use a <code>package-info.java</code> file to define
 * a namespace for the whole package.
 * Note that two objects of this class are considered equal iff they have the same QName, i.e. they represent
 * the same JAXB2 serializable class. The cardinality is not taken into consideration.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @see javax.xml.bind.annotation.XmlRootElement
 * @since 0.1
 */
public final class BusinessObject {

    private Logger logger = null;
   
    private final Class<?> objectType;
    
    private final int cardinality;
    
    private final QName elementName;
    
    public BusinessObject(Class<?> objectType, int cardinality) {
        logger = new LoggerImpl(java.util.logging.Logger.getLogger(BusinessObject.class.getName()));

        if (objectType == null) {
            throw new NullPointerException("Input object cannot be null");
        }

        if (cardinality <= 0) {
            throw new IllegalArgumentException("Cartinality must be greater than zero: " + cardinality);
        }

        if (!containsXmlRootElementAnnotation(objectType)) {
            throw new IllegalArgumentException(
                    "Input object must contain @XmlRootElement annotation: " + objectType.getName());
        }

        if (!containsNoArgConstructor(objectType)) {
            throw new IllegalArgumentException(
                    "Input object '" + objectType.getName() + "' must have a no-arg constructor");
        }

        if (!belongsToNamespace(objectType)) {
            throw new IllegalArgumentException(
                    "Input object '" + objectType.getName() + "' must belong to a namespace");
        }
        
        logger.fine("Creating business object '%s' with cardinality %d",
                objectType.getSimpleName(), cardinality);
        
        this.objectType = objectType;

        this.cardinality = cardinality;

        elementName = getElementName(objectType);
        
        if (elementName == null) {
            throw new IllegalArgumentException("Could not determine the element name of the business object: "
                    + objectType.getName());
        }
    }

    public static boolean containsXmlRootElementAnnotation(Class<?> clazz) {
        return clazz.getAnnotation(XmlRootElement.class) != null;
    }

    public static boolean containsNoArgConstructor(Class<?> clazz) {
        try {
            Constructor<?> constructor = clazz.getConstructor();

            return true;

        } catch (NoSuchMethodException e) {

            return false;

        } catch (SecurityException e) {
            throw new RuntimeException(e);
        }
    }

    public static boolean belongsToNamespace(Class<?> clazz) {
        return !getElementName(clazz).getNamespaceURI().equals("");
    }
    
    static Object newInstance(Class<?> clazz) {
        try {
            return clazz.newInstance();

        } catch (InstantiationException e) {
            throw new RuntimeException(e);

        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
    
    public static QName getElementName(Class<?> clazz) {
        try {
            JAXBContext context = JAXBContext.newInstance(clazz);
            JAXBIntrospector introspector = context.createJAXBIntrospector();
            return introspector.getElementName(newInstance(clazz));

        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof BusinessObject)) {
            return false;
        }
        
        final BusinessObject other = (BusinessObject) obj;
        
        return elementName.equals(other.elementName);
    }

    @Override
    public int hashCode() {
        return elementName.hashCode();
    }

    public QName getElementName() {
        return elementName;
    }

    public Class<?> getObjectType() {
        return objectType;
    }

    public int getCardinality() {
        return cardinality;
    }
}
