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
 * @(#)NamedWSDLExtensibleElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.NamedWSDLElement;
import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLDocument;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class NamedWSDLExtensibleElementImpl extends WSDLExtensibleElementImpl implements NamedWSDLElement {
	
	/** Creates a new instance of NamedWSDLExtensibleElementImpl */
    public NamedWSDLExtensibleElementImpl() {
        super();
    }
    
    /** Creates a new instance of WSDLElementImpl.
     * @param   d   Owner document.
     */
    public NamedWSDLExtensibleElementImpl(XMLDocument d) {
        super(d);
    }
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
	/***
     * get the QName of this element. Qname will have namespace where this element is defined and name of this element.
     * @return QName
     */
    public QName getQName() {
    	String name = getName();
    	/*String namespace = null;
    	if(this.getOwnerDocument() != null) {
    		namespace = this.getOwnerDocument().getTargetNamespace();
    		//if targetNamespace is null then it use default namespace
    		if(namespace == null) {
    			namespace = this.getDefaultNamespace();
    		}
    	}*/
    	
    	QName qName = NamespaceUtility.resolveAndGetQName(name, this);
    	
    	return qName;
    }
    
    /***
     * Utility method.
     * get the QName of this element. This method calls getQName() then 
     * if there is a namespace available in QName then it trys to replace it with the first prefix for this namespace in the owner document of this element.
     * If no prefix is found then QName is same as calling getQName().
     * @return QName
     */
    public QName getPrefixedQName() {
    	QName qName = getQName();
    	
    	if(qName != null) {
    		String ns = qName.getNamespaceURI();
    		//String localName = qName.getLocalName();
    		String localName = qName.getLocalPart();
    		
    		if(ns != null && !ns.trim().equals("")) {
    			WSDLDocument document = (WSDLDocument) this.getOwnerDocument();
    			WSDLDefinitions definition = document.getDocumentDefinitions();
    			String prefix = definition.getNamespacePrefix(ns);
    			if(prefix != null && !prefix.trim().equals("")) {
    				qName = NamespaceUtility.getQName(ns, localName, prefix);
    			}	
    		}
    	}
    	
    	return qName;
    }

}
