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
 * @(#)BaseElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.io.StringWriter;

import org.xml.sax.SAXException;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.visitor.SAXWriteVisitor;
import com.sun.bpel.model.visitor.SAXWriteVisitorService;
import com.sun.bpel.model.wsdlmodel.impl.XMLElementImpl;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.bpel.xml.common.visitor.VisitorService;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class BaseElementImpl extends XMLElementImpl {
	
	/** Visitor service to use for serializing */
    private static VisitorService visitorService = new SAXWriteVisitorService();

    /**   */
    private String mToString = "";
    
    /** Creates a new instance of BaseElementImpl */
    public BaseElementImpl() {
        super();
        initBaseElement();
    }
    
    /** Creates a new instance of BaseElementImpl.
     * @param   d   Owner document.
     */
    public BaseElementImpl(XMLDocument d) {
        super(d);
        initBaseElement();
    }
    
    /** Initializes this element.
     */
    private void initBaseElement() {
        owningNamespace = BPELDocument.BPEL_NAMESPACE;
        owningNamespacePrefix = BPELDocument.BPEL_PREFIX;
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
    
    

    /** Morphs a generic visitor into the BPEL visitor.
     * @param   w   A generic worker visitor.
     * @return  A BPEL visitor.
     */
    protected BPELVisitor morphVisitor(Visitor w) {
        if (w instanceof BPELVisitor) {
            return (BPELVisitor) w;
        }
        return (BPELVisitor) w.getVisitorService().fetch(BPELVisitor.class,
            w.getVisitorSupport());
    }
    
    public void setString() throws SAXException {
        String retStr = super.toString();
        String serializedBPELDocument = getSerializedBPELDocument(); 

        retStr = retStr  + "={" + serializedBPELDocument + "}";

        mToString = retStr;
    }
    
	/** Shows the XML presentation of this element (and its children).
     * @return  XML presentation of this element (and children).
     */
    public String toString() {
        return mToString;
    }
    
    /**
     * Method added for monitoring API to provide serialized BPEL document
     * 
     * @return
     * @throws SAXException
     */
    public String getSerializedBPELDocument() throws SAXException {
        StringWriter sw = new StringWriter();
        Object[] params = new Object[] {sw, Boolean.TRUE, Boolean.TRUE};
        BPELVisitor visitor = (BPELVisitor) visitorService.fetch(BPELVisitor.class, null);
        visitor.prepare(params);
        
        ((SAXWriteVisitor) visitor).getWriterSupport().getXmlWriter().startDocument();
        accept(visitor);
        ((SAXWriteVisitor) visitor).getWriterSupport().getXmlWriter().endDocument();
        return sw.toString();
    }
}
