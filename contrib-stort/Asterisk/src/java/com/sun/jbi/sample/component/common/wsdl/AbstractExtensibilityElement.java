/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AbstractExtensibilityElement.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common.wsdl;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

public abstract class AbstractExtensibilityElement implements ExtensibilityElement, java.io.Serializable {
    public static final long serialVersionUID = 1;
    private QName mElementType;
    private Boolean mRequired;
    
    /** Creates a new instance of AbstractExtensibilityElement */
    protected AbstractExtensibilityElement() {
    }
    
    public void setElementType(QName elementType) {
        this.mElementType = elementType;
    }
    
    public QName getElementType() {
        return this.mElementType;
    }
    
    public void setRequired(Boolean required) {
        this.mRequired = required;
    }
    
    public Boolean getRequired() {
        return this.mRequired;
    }
    
}
