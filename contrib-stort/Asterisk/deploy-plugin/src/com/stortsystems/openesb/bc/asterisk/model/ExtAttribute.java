/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ExtAttribute.java,v 1.1 2008/01/20 16:40:08 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import org.netbeans.modules.xml.xam.dom.Attribute;

public enum ExtAttribute implements Attribute, ExtConstants {
    
    //TODO: define any additional extension elements attributes here
    
    EVENTTYPES(ATTR_EVENTTYPES),
    ACTION(ATTR_ACTION),
    ADDRESS(ATTR_ADDRESS),
    USERNAME(ATTR_USERNAME),
    PASSWORD(ATTR_PASSWORD),
    MBEAN(ATTR_PORT);
    
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
