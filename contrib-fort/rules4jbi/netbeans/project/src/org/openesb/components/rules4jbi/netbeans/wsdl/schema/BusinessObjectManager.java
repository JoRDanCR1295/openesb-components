/*
 * @(#)BusinessObjectManager.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.namespace.QName;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class BusinessObjectManager {
    
    private final Set<BusinessObject> inputObjects;
    
    private final Set<BusinessObject> outputObjects;

    public BusinessObjectManager() {
        inputObjects = new LinkedHashSet<BusinessObject>();

        outputObjects = new LinkedHashSet<BusinessObject>();
    }
    
    public void addInputObject(Class<?> objectType, int cardinality) {
        addBusinessObject(objectType, cardinality, inputObjects);
    }
    
    public void addOutputObject(Class<?> objectType, int cardinality) {
        addBusinessObject(objectType, cardinality, outputObjects);
    }

    private void addBusinessObject(Class<?> objectType, int cardinality, Set<BusinessObject> businessObjects) {
        BusinessObject businessObject = new BusinessObject(objectType, cardinality);

        if (businessObjects.contains(businessObject)) {
            throw new IllegalArgumentException("Object of this type is already registered");
        }
        
        businessObjects.add(businessObject);
    }
    
    public Set<QName> getElementNames() {
        Set<BusinessObject> allObjects = new LinkedHashSet<BusinessObject>();
        allObjects = union(inputObjects, outputObjects);
        
        Set<QName> result = new LinkedHashSet<QName>();
        
        for (BusinessObject businessObject : allObjects) {
            result.add(businessObject.getElementName());
        }
        
        return result;
    }
    
    public Set<Class<?>> getObjectTypes() {
        Set<BusinessObject> allObjects = new LinkedHashSet<BusinessObject>();
        allObjects = union(inputObjects, outputObjects);
        
        Set<Class<?>> result = new LinkedHashSet<Class<?>>();
        
        for (BusinessObject businessObject : allObjects) {
            result.add(businessObject.getObjectType());
        }
        
        return result;
    }
    
    public Iterator<BusinessObject> inputIterator() {
        return inputObjects.iterator();
    }
    
    public Iterator<BusinessObject> outputIterator() {
        return outputObjects.iterator();
    }
    
    static <T> Set<T> union(Set<T> x, Set<T> y) {
        Set<T> result = new LinkedHashSet<T>();
        
        result.addAll(x);
        result.addAll(y);
        
        return result;
    }
}
