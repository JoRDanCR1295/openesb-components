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
 * @(#)XSDVariableImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.xml.NamespaceUtility;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.XSDVariable;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;

/**
 * Implementation for XSD variable.
 * 
 * @author mbhasin
 */
public class XSDVariableImpl implements XSDVariable {

    private Document mDocument = null;
    private Element mRoot = null;
    private QName mVariable = null;

    public XSDVariableImpl(QName variable) {
        
        this.mVariable = variable;
        
        XmlResourceProviderPool xmlResProviderpool 
                = (XmlResourceProviderPool)BPELSERegistry.getInstance()
                    .lookup(XmlResourceProviderPool.class.getName());                            
        XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
        Document doc = xmlResourceProvider.getDocumentBuilder().newDocument();
        xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);            
        xmlResourceProvider = null;        
        
        String ns = variable.getNamespaceURI();
        String localName = variable.getLocalPart();
        
        mRoot = doc.createElementNS(ns, localName);
        doc.appendChild(mRoot);
    }
    
    public XSDVariableImpl(Element rootElem) {
        mRoot = rootElem;
        String namespaceURI = rootElem.getNamespaceURI();
        String localName = rootElem.getLocalName();
        String prefix = rootElem.getPrefix();
        if (prefix != null) {
        	this.mVariable = NamespaceUtility.getQName(
        			namespaceURI, localName, prefix);
        } else { // namespaceURI and localName can never be null
        	this.mVariable = NamespaceUtility.getQName(
        			namespaceURI, localName);
        }
        
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.impl.XSDVariable#getRootElement()
     */
    public Element getRootElement() {
        return this.mRoot;
    }
    
    /**
     * @see java.lang.Object#toString()
     */    
    public String toString() {
        if(mRoot != null) {
            return DOMHelper.createXmlString(mRoot);
        } else {
            return null;
        }
    }
}
