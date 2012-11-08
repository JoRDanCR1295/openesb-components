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
 * @(#)SOAPHeaderFaultImpl.java 
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
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP header fault extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPHeaderFaultImpl extends ExtensibilityElementImpl implements SOAPHeaderFault {
    
    /** Creates a new instance of SOAPHeaderFaultImpl */
    public SOAPHeaderFaultImpl() {
        super();
        initSOAPHeaderFault();
    }
    
    /** Constructor for new SOAP header fault instance.
     * @param   d   Owner document.
     */
    public SOAPHeaderFaultImpl(XMLDocument d) {
        super(d);
        initSOAPHeaderFault();
    }
    
    /** Initializes this class.
     */
    private void initSOAPHeaderFault() {
        setQualifiedName(SOAPHeaderFault.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPHeaderFault.ATTR.MESSAGE, String.class, true, null),
            new XMLAttributeImpl(SOAPHeaderFault.ATTR.PART, String.class,  true, null),
            new XMLAttributeImpl(SOAPHeaderFault.ATTR.USE, String.class, true, new String[] {"literal", "encoded"}),
            new XMLAttributeImpl(SOAPHeaderFault.ATTR.ENCODING_STYLE, String.class, false, null),
            new XMLAttributeImpl(SOAPHeaderFault.ATTR.NAMESPACE, String.class, false, null)
        };
    }
    
    /**
     * @see SOAPHeaderFault#getMessage
     */
    public String getMessage() {
        return xmlAttrs[MESSAGE].getValue();
    }
    
    /**
     * @see SOAPHeaderFault#setMessage(String)
     */
    public void setMessage(String message) {
        setAttribute(MESSAGE, message);
    }
    
    /**
     * @see SOAPHeaderFault#setMessage(String, String)
     */
    public void setMessage(String qName, String message) {
        setAttribute(MESSAGE, qName, message);
    }
    
    /**
     * @see SOAPHeaderFault#getPart
     */
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }
    
    /**
     * @see SOAPHeaderFault#setPart(String)
     */
    public void setPart(String parts) {
        setAttribute(PART, parts);
    }
    
    /**
     * @see SOAPHeaderFault#setPart(String, String)
     */
    public void setPart(String qName, String part) {
        setAttribute(PART, qName, part);
    }
    
    /**
     * @see SOAPHeaderFault#getUse
     */
    public String getUse() {
        return xmlAttrs[USE].getValue();
    }
    
    /**
     * @see SOAPHeaderFault#setUse(String)
     */
    public void setUse(String use) {
        setAttribute(USE, use);
    }
    
    /**
     * @see SOAPHeaderFault#setUse(String, String)
     */
    public void setUse(String qName, String use) {
        setAttribute(USE, qName, use);
    }
    
    /**
     * @see SOAPHeaderFault#getEncodingStyle
     */
    public String getEncodingStyle() {
        return xmlAttrs[ENCODING_STYLE].getValue();
    }
    
    /**
     * @see SOAPHeaderFault#setEncodingStyle(String)
     */
    public void setEncodingStyle(String style) {
        setAttribute(ENCODING_STYLE, style);
    }
    
    /**
     * @see SOAPHeaderFault#setEncodingStyle(String, String)
     */
    public void setEncodingStyle(String qName, String style) {
        setAttribute(ENCODING_STYLE, qName, style);
    }
    
    /**
     * @see SOAPHeaderFault#getNamespaceURI
     */
    public String getNamespaceURI() {
        return xmlAttrs[NAMESPACE].getValue();
    }
    
    /**
     * @see SOAPHeaderFault#setNamespaceURI(String)
     */
    public void setNamespaceURI(String namespace) {
        setAttribute(NAMESPACE, namespace);
    }
    
    /**
     * @see SOAPHeaderFault#setNamespaceURI(String, String)
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
