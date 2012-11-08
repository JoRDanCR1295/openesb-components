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
 * @(#)SOAPAddressImpl.java 
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
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the SOAP body extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPAddressImpl extends ExtensibilityElementImpl implements SOAPAddress {
    
    /** Creates a new instance of SOAPAddressImpl */
    public SOAPAddressImpl() {
        super();
        initSOAPAddress();
    }
    
    /** Constructor for new SOAP address instance.
     * @param   d   Owner document.
     */
    public SOAPAddressImpl(XMLDocument d) {
        super(d);
        initSOAPAddress();
    }
    
    /** Initializes this class.
     */
    private void initSOAPAddress() {
        setQualifiedName(SOAPAddress.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPAddress.ATTR.LOCATION, String.class,  true, null)
        };
    }
    
    /**
     * @see SOAPAddress#getLocation
     */
    public String getLocation() {
        return xmlAttrs[LOCATION].getValue();
    }
    
    /**
     * @see SOAPAddress#setLocation(String)
     */
    public void setLocation(String location) {
        setAttribute(LOCATION, location);
    }
    
    /**
     * @see SOAPAddress#setLocation(String, String)
     */
    public void setLocation(String qName, String location) {
        setAttribute(LOCATION, qName, location);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
