/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ExtensibilityElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.impl;

import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLExtensibleElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the extensibility element in WSDL.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ExtensibilityElementImpl extends WSDLExtensibleElementImpl implements ExtensibilityElement {
    
    /** Holds the element type. */
    protected QName elementType;
    
    /** Holds the required property. */
    protected Boolean required;
    
    /** Constructs an ExtensibilityElement instance. */
    public ExtensibilityElementImpl() {
        super();
    }
    
    /** Constructs an ExtensibilityElement instance.
     * @param   d   Owner document.
     */
    public ExtensibilityElementImpl(XMLDocument d) {
        super(d);
    }
    
    /**
     * @see ExtensibilityElement#setElementType
     */
    public void setElementType(QName elementType) {
        this.elementType = elementType;
        setQualifiedName(elementType);
    }
    
    /**
     * @see ExtensibilityElement#getElementType
     */
    public QName getElementType() {
        return elementType;
    }
    
    /**
     * @see ExtensibilityElement#setRequired
     */
    public void setRequired(Boolean required) {
        this.required = required;
    }
    
    /**
     * @see ExtensibilityElement#getRequired
     */
    public Boolean getRequired() {
        return required;
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);

        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }

        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
    
}
