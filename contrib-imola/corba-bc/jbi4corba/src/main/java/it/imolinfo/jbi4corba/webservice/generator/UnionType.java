 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

/**
 * 
 * class UnionType - define union type class 
 * @author laurlg
 */
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class UnionType implements SearchedType {

    private String typeName;
    private Class discriminatorType;
    private Map<String, Class> classFields;

    /**
     * Constructor
     */
    public UnionType() {
        classFields = new HashMap<String, Class>();
    }

    /**
     * Constructor
     * @param name - name of the union type
     * @param discr - discriminator type
     * @param fields - map with fields name and types
     */
    public UnionType(String name, Class discr, Map<String, Class> fields) {
        typeName = name;
        discriminatorType = discr;
        classFields = new HashMap<String, Class>();
        classFields.putAll(fields);
    }

    /**
     * 
     * @return typeName
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * 
     * @param typeName
     */
    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    /**
     * 
     * @return discriminatorType
     */
    public Class getDiscriminatorType() {
        return discriminatorType;
    }

    /**
     * 
     * @param discriminatorType
     */
    public void setDiscriminatorType(Class discriminatorType) {
        this.discriminatorType = discriminatorType;
    }

    /**
     * 
     * @return field names
     */
    public Set<String> getTypeFieldNameList() {
        return classFields.keySet();
    }

    /**
     * 
     * @param fieldName
     * @return type of field
     */
    public Class getFieldType(String fieldName) {
        return classFields.get(fieldName);
    }

    /**
     * 
     * @param fieldName
     * @param fieldType
     */
    public void addTypeField(String fieldName, Class fieldType) {
        classFields.put(fieldName, fieldType);
    }
    
    /**
     * 
     * @return number of fields
     */
    public int getNumberOfFields()
    {
    	return classFields.size();
    }
    
}