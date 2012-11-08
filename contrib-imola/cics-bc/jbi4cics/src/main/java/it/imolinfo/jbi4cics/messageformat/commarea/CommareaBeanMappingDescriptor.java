/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.messageformat.commarea;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.collections.map.ListOrderedMap;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * Questa classe fondamentalmente wrappa una mappa fra i nomi dei field nella
 * commarea e i nomi delle properies di un java bean.
 *
 * @author raffaele
 */
public class CommareaBeanMappingDescriptor implements MappingDescriptor {

    private Map<String, FieldDescriptor> fieldMap = new ListOrderedMap();

    /**
     * The bean class.
     */
    private Class beanClass;

    /**
     * Creates a new instance of this class.
     */
    public CommareaBeanMappingDescriptor() {
    }

    /**
     * Returns the bean class.
     *
     * @return  the bean class.
     */
    public Class getBeanClass() {
        return beanClass;
    }

    /**
     * Sets the bean class.
     *
     * @param  beanClass  the bean class to set.
     */
    public void setBeanClass(Class beanClass) {
        this.beanClass = beanClass;
    }

    public void addFieldMapping(String propertyName, String fieldName,
            FieldDescriptor fieldDescriptor) throws FormatException {
        if (!(fieldDescriptor instanceof CobolTypeDescriptor)) {
            throw new FormatException(
                    "CIC001700_Expected_cobol_type_descriptor",
                    new Object[] { fieldDescriptor.getClass() });
        }
        fieldMap.put(propertyName, fieldDescriptor);
    }

    /**
     * Not implementaed.
     *
     * @throws  FormatException  ever.
     */
    public void addFieldMapping(String propertyName, Integer fieldIndex,
            FieldDescriptor fieldDescriptor) throws FormatException {
        throw new FormatException("CIC001701_Error_adding_field_mapping");
    }

    public int getBufferedLength() throws FormatException {
        int size = 0;

        for (FieldDescriptor desc : fieldMap.values()) {
            CobolTypeDescriptor cobolDesc = (CobolTypeDescriptor) desc;

            size += cobolDesc.getBufferedLength();
        }
        return size;
    }

    public Map<String, FieldDescriptor> getFieldMap() {
        return Collections.unmodifiableMap(fieldMap);
    }

    public void setCodePage(String codePage) {
        for (FieldDescriptor desc : fieldMap.values()) {
            CobolTypeDescriptor cobolDesc = (CobolTypeDescriptor) desc;

            cobolDesc.setCodePage(codePage);
        }
    }

    @Override
    public String toString() {                                  // Overridden
        return ReflectionToStringBuilder.toString(this);
    }

    @Override
    public int hashCode() {                                     // Overridden
        return fieldMap.hashCode();
    }

    @Override
    public boolean equals(Object obj) {                         // Overridden
        if (obj instanceof CommareaBeanMappingDescriptor) {
            CommareaBeanMappingDescriptor that
                    = (CommareaBeanMappingDescriptor) obj;

            return fieldMap.equals(that.fieldMap);
        }
        return false;
    }
}
