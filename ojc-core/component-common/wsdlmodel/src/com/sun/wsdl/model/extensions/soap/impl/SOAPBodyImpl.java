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
 * @(#)SOAPBodyImpl.java 
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
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP body extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPBodyImpl extends ExtensibilityElementImpl implements SOAPBody {
    
    /** Creates a new instance of SOAPBodyImpl */
    public SOAPBodyImpl() {
        super();
        initSOAPBody();
    }
    
    /** Constructor for new SOAP body instance.
     * @param   d   Owner document.
     */
    public SOAPBodyImpl(XMLDocument d) {
        super(d);
        initSOAPBody();
    }
    
    /** Initializes this class.
     */
    private void initSOAPBody() {
        setQualifiedName(SOAPBody.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPBody.ATTR.PARTS, String.class,  false, null),
            new XMLAttributeImpl(SOAPBody.ATTR.USE, String.class, true, new String[] {"literal", "encoded"}),
            new XMLAttributeImpl(SOAPBody.ATTR.ENCODING_STYLE, String.class, false, null),
            new XMLAttributeImpl(SOAPBody.ATTR.NAMESPACE, String.class, false, null)
        };
    }
    
    /**
     * @see SOAPBody#getParts
     */
    public String getParts() {
        return xmlAttrs[PARTS].getValue();
    }
    
    /**
     * @see SOAPBody#setParts(String)
     */
    public void setParts(String parts) {
        setAttribute(PARTS, parts);
    }
    
    /**
     * @see SOAPBody#setParts(String, String)
     */
    public void setParts(String qName, String parts) {
        setAttribute(PARTS, qName, parts);
    }
    
    /**
     * @see SOAPBody#getUse
     */
    public String getUse() {
        return xmlAttrs[USE].getValue();
    }
    
    /**
     * @see SOAPBody#setUse(String)
     */
    public void setUse(String use) {
        setAttribute(USE, use);
    }
    
    /**
     * @see SOAPBody#setUse(String, String)
     */
    public void setUse(String qName, String use) {
        setAttribute(USE, qName, use);
    }
    
    /**
     * @see SOAPBody#getEncodingStyle
     */
    public String getEncodingStyle() {
        return xmlAttrs[ENCODING_STYLE].getValue();
    }
    
    /**
     * @see SOAPBody#setEncodingStyle(String)
     */
    public void setEncodingStyle(String style) {
        setAttribute(ENCODING_STYLE, style);
    }
    
    /**
     * @see SOAPBody#setEncodingStyle(String, String)
     */
    public void setEncodingStyle(String qName, String style) {
        setAttribute(ENCODING_STYLE, qName, style);
    }
    
    /**
     * @see SOAPBody#getNamespaceURI
     */
    public String getNamespaceURI() {
        return xmlAttrs[NAMESPACE].getValue();
    }
    
    /**
     * @see SOAPBody#setNamespaceURI(String)
     */
    public void setNamespaceURI(String namespace) {
        setAttribute(NAMESPACE, namespace);
    }
    
    /**
     * @see SOAPBody#setNamespaceURI(String, String)
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
