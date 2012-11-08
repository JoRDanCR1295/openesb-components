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
 * @(#)WSDLElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLElement;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLElementImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.visitor.SAXWriteVisitor;
import com.sun.wsdl.model.visitor.SAXWriteVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;

import java.io.StringWriter;

/**
 *
 * @author Sun Microsystems
 * @version 
 */
public class WSDLElementImpl extends XMLElementImpl implements WSDLElement {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 6616409224719567079L;
    
    /** Holds visitor service for serializing */
    private static VisitorService visitorService = new SAXWriteVisitorService();
    
    /** Creates a new instance of WSDLElementImpl */
    public WSDLElementImpl() {
        super();
        initWSDLElement();
    }
    
    /** Creates a new instance of WSDLElementImpl.
     * @param   d   Owner document.
     */
    public WSDLElementImpl(XMLDocument d) {
        super(d);
        initWSDLElement();
    }
    
    /** Initializes the WSDL Element.
     */
    private void initWSDLElement() {
        owningNamespace = WSDLDocument.WSDL_NAMESPACE;
        owningNamespacePrefix = WSDLDocument.WSDL_PREFIX;
    }
    
    /** @see XMLElement#setDefaultNamespace(java.lang.String)
     */
    public void setDefaultNamespace(String uri) {
        super.setDefaultNamespace(uri);
        if (!qNameExplicitlySet && !owningNamespace.equals(uri)) {
            setQualifiedName(owningNamespace, owningNamespacePrefix);
        }
    }
    
    /** @see XMLNode#setLocalName
     */
    public void setLocalName(String localName) {
        super.setLocalName(localName);
        if (!qNameExplicitlySet) {
            String defaultNS = getDefaultNamespace();
            if ((defaultNS != null) && !owningNamespace.equals(defaultNS)) {
                setQualifiedName(owningNamespace, owningNamespacePrefix);
            }
        }
    }
    
    /** Morphs a generic visitor into the WSDL visitor.
     * @param   w   A generic working visitor.
     * @return  A WSDL visitor.
     */
    protected WSDLVisitor morphVisitor(Visitor w) {
        if (w instanceof WSDLVisitor) {
            return (WSDLVisitor) w;
        }
        return (WSDLVisitor) w.getVisitorService().fetch(WSDLVisitor.class,
            w.getVisitorSupport());
    }
    
    /** Shows the XML presentation of this element (and its children).
     * @return  XML presentation of this element (and children).
     */
    public String toString() {
        String retStr = super.toString();
        StringWriter sw = new StringWriter();
        Object[] params = new Object[] {sw, Boolean.TRUE, Boolean.TRUE};
        WSDLVisitor visitor = (WSDLVisitor) visitorService.fetch(WSDLVisitor.class, null);
        visitor.prepare(params);
        try {
            ((SAXWriteVisitor) visitor).getWriterSupport().getXmlWriter().startDocument();
            accept(visitor);
            ((SAXWriteVisitor) visitor).getWriterSupport().getXmlWriter().endDocument();
            retStr = retStr + "={" + sw.toString() + "}";
        } catch (Throwable trw) {
            // just return standard
        }
        return retStr;
    }
}
