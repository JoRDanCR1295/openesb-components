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
 * @(#)SOAPBindingImpl.java 
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
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP binding extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPBindingImpl extends ExtensibilityElementImpl
    implements SOAPBinding {
    
    /** Creates a new instance of SOAPBindingImpl */
    public SOAPBindingImpl() {
        super();
        initSOAPBinding();
    }
    
    /** Constructor for new binding instance.
     * @param   d   Owner document.
     */
    public SOAPBindingImpl(XMLDocument d) {
        super(d);
        initSOAPBinding();
    }
    
    /** Initializes this class.
     */
    private void initSOAPBinding() {
        setQualifiedName(SOAPBinding.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPBinding.ATTR.STYLE, String.class,  true, new String[] {"document", "rpc"}),
            new XMLAttributeImpl(SOAPBinding.ATTR.TRANSPORT, String.class, true, null)
        };
    }
    
    /**
     * @see SOAPBinding#getStyle
     */
    public String getStyle() {
        return xmlAttrs[STYLE].getValue();
    }
    
    /**
     * @see SOAPBinding#setStyle(String)
     */
    public void setStyle(String style) {
        setAttribute(STYLE, style);
    }
    
    /**
     * @see SOAPBinding#setStyle(String, String)
     */
    public void setStyle(String qName, String style) {
        setAttribute(STYLE, qName, style);
    }
    
    /**
     * @see SOAPBinding#getTransport
     */
    public String getTransport() {
        return xmlAttrs[TRANSPORT].getValue();
    }
    
    /**
     * @see SOAPBinding#setTransport(String)
     */
    public void setTransport(String transport) {
        setAttribute(TRANSPORT, transport);
    }
    
    /**
     * @see SOAPBinding#setTransport(String, String)
     */
    public void setTransport(String qName, String transport) {
        setAttribute(TRANSPORT, qName, transport);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
