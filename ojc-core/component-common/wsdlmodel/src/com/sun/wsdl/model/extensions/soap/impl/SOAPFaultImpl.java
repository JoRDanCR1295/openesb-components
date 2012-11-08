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
 * @(#)SOAPFaultImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap.impl;

import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP fault extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPFaultImpl extends ExtensibilityElementImpl implements SOAPFault {
    
    /** Creates a new instance of SOAPFaultImpl */
    public SOAPFaultImpl() {
        super();
        initSOAPFault();
    }
    
    /** Constructor for new SOAP fault instance.
     * @param   d   Owner document.
     */
    public SOAPFaultImpl(XMLDocument d) {
        super(d);
        initSOAPFault();
    }
    
    /** Initializes this class.
     */
    private void initSOAPFault() {
        setQualifiedName(SOAPFault.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPFault.ATTR.NAME, String.class, true, null),
            new XMLAttributeImpl(SOAPFault.ATTR.USE, String.class, true, new String[] {"literal", "encoded"}),
            new XMLAttributeImpl(SOAPFault.ATTR.ENCODING_STYLE, String.class, false, null),
            new XMLAttributeImpl(SOAPFault.ATTR.NAMESPACE, String.class, false, null)
        };
    }
    
    /**
     * @see SOAPFault#getName
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * @see SOAPFault#setName(String)
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /**
     * @see SOAPFault#setName(String, String)
     */
    public void setName(String qName, String name) {
        setAttribute(NAME, qName, name);
    }
    
    /**
     * @see SOAPFault#getUse
     */
    public String getUse() {
        return xmlAttrs[USE].getValue();
    }
    
    /**
     * @see SOAPFault#setUse(String)
     */
    public void setUse(String use) {
        setAttribute(USE, use);
    }
    
    /**
     * @see SOAPFault#setUse(String, String)
     */
    public void setUse(String qName, String use) {
        setAttribute(USE, qName, use);
    }
    
    /**
     * @see SOAPFault#getEncodingStyle
     */
    public String getEncodingStyle() {
        return xmlAttrs[ENCODING_STYLE].getValue();
    }
    
    /**
     * @see SOAPFault#setEncodingStyle(String)
     */
    public void setEncodingStyle(String style) {
        setAttribute(ENCODING_STYLE, style);
    }
    
    /**
     * @see SOAPFault#setEncodingStyle(String, String)
     */
    public void setEncodingStyle(String qName, String style) {
        setAttribute(ENCODING_STYLE, qName, style);
    }
    
    /**
     * @see SOAPFault#getNamespaceURI
     */
    public String getNamespaceURI() {
        return xmlAttrs[NAMESPACE].getValue();
    }
    
    /**
     * @see SOAPFault#setNamespaceURI(String)
     */
    public void setNamespaceURI(String namespace) {
        setAttribute(NAMESPACE, namespace);
    }
    
    /**
     * @see SOAPFault#setNamespaceURI(String, String)
     */
    public void setNamespaceURI(String qName, String namespace) {
        setAttribute(NAMESPACE, qName, namespace);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
