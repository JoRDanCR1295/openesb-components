/*
 * ExtAttribute.java
 */
package org.netbeans.modules.wsdlextensions.sample.binding.model;

import org.netbeans.modules.xml.xam.dom.Attribute;

/**
 * @author chikkala
 */
public enum ExtAttribute implements Attribute, ExtConstants {

    //TODO: define any additional extension elements attributes here

    ACTION(ATTR_ACTION), 
    LOCATION(ATTR_LOCATION);

    private String name;
    private Class type;
    private Class subtype;

    ExtAttribute(String name) {
        this(name, String.class);
    }

    ExtAttribute(String name, Class type) {
        this(name, type, null);
    }

    ExtAttribute(String name, Class type, Class subtype) {
        this.name = name;
        this.type = type;
        this.subtype = subtype;
    }

    @Override
    public String toString() {
        return name;
    }

    public Class getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public Class getMemberType() {
        return subtype;
    }
}
