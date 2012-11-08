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
 * @(#)SOAPOperationImpl.java 
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
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP operation extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPOperationImpl extends ExtensibilityElementImpl implements SOAPOperation {
    
    /** Creates a new instance of SOAPBindingImpl */
    public SOAPOperationImpl() {
        super();
        initSOAPOperation();
    }
    
    /** Constructor for new binding instance.
     * @param   d   Owner document.
     */
    public SOAPOperationImpl(XMLDocument d) {
        super(d);
        initSOAPOperation();
    }
    
    /** Initializes this class.
     */
    private void initSOAPOperation() {
        setQualifiedName(SOAPOperation.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPOperation.ATTR.SOAP_ACTION, String.class, false, null),
            new XMLAttributeImpl(SOAPOperation.ATTR.STYLE, String.class, false, new String[] {"document", "rpc"})
        };
    }
    
    /**
     * @see SOAPOperation#getSoapAction
     */
    public String getSoapAction() {
        return xmlAttrs[SOAP_ACTION].getValue();
    }
    
    /**
     * @see SOAPOperation#setSoapAction(String)
     */
    public void setSoapAction(String uri) {
        setAttribute(SOAP_ACTION, uri);
    }
    
    /**
     * @see SOAPOperation#setSoapAction(String, String)
     */
    public void setSoapAction(String qName, String uri) {
        setAttribute(SOAP_ACTION, qName, uri);
    }
    
    /**
     * @see SOAPOperation#getStyle
     */
    public String getStyle() {
        return xmlAttrs[STYLE].getValue();
    }
    
    /**
     * @see SOAPOperation#setStyle(String)
     */
    public void setStyle(String style) {
        setAttribute(STYLE, style);
    }
    
    /**
     * @see SOAPOperation#setStyle(String, String)
     */
    public void setStyle(String qName, String style) {
        setAttribute(STYLE, qName, style);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
