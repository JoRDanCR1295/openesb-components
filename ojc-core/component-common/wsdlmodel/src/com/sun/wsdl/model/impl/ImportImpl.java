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
 * @(#)ImportImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLParseContext;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.FileURIResolverFactory;
import com.sun.wsdl.model.visitor.LazyImportVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;


/**
 * Describes the &lt;import&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ImportImpl
    extends WSDLElementImpl
    implements Import {
    
	private transient Object mImportedObject;
	
    /** Creates a new instance of ImportImpl */
    public ImportImpl() {
        super();
        initImport();
    }
    
    /** Constructor for new import instance.
     * @param   d   Owner document.
     */
    public ImportImpl(XMLDocument d) {
        super(d);
        initImport();
    }
    
    /**
     * Initializes this class.
     */
    private void initImport() {
        setLocalName(Import.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAMESPACE, String.class, false, null),
            new XMLAttributeImpl(ATTR.LOCATION, String.class, false, null)
        };
    }
    
    /** @see com.sun.wsdl.model.Import#getNamespaceAttr()
     */
    public String getNamespaceAttr() {
        return xmlAttrs[NAMESPACE].getValue();
    }
    
    /** @see com.sun.wsdl.model.Import#setNamespaceAttr(java.lang.String)
     */
    public void setNamespaceAttr(String namespace) {
        setAttribute(NAMESPACE, namespace);
    }
    
    /** @see com.sun.wsdl.model.Import#getLocation()
     */
    public String getLocation() {
        return xmlAttrs[LOCATION].getValue();
    }
    
    /** @see com.sun.wsdl.model.Import#setLocation(java.lang.String)
     */
    public void setLocation(String location) {
        setAttribute(LOCATION, location);
    }
    
    /** @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.model.visitor.Visitor)
     */
    public boolean accept(Visitor w) {
    	WSDLVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
    
    /**
     * set the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @param document top level document
     */
    public void setImportedObject(Object obj) {
    	this.mImportedObject = obj;
    }

    /**
     * get the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @return WSDLDefinitions or XMLSchema
     */
    public Object getImportedObject() {
    	if(this.mImportedObject != null) {
    		return this.mImportedObject;
    	}
    	
    	try {
    		LazyImportVisitorService visitorService = new LazyImportVisitorService();
    		WSDLParseContext wsdlParseContext = null;
    		if(this.getOwnerDocument() != null) {
    			wsdlParseContext = ((WSDLDocument) this.getOwnerDocument()).getWSDLParseContext();
    		}
    		
    		if(wsdlParseContext == null) {
    			wsdlParseContext = new WSDLParseContext.DefaultParseContext();
    		}
    		
    		visitorService.setWSDLParseContext(wsdlParseContext);
    		WSDLVisitor visitor = (WSDLVisitor) visitorService.fetch(WSDLVisitor.class, null);
    		
    		BaseURIResolver resolver = wsdlParseContext.getBaseURIResolver();
    		    		
        	Object[] params = new Object[] {
                resolver,   // Assumed to be file/http based
            };
        	
    	    visitor.prepare(params);
    	    visitor.visit(this);
    	} catch (Exception e) {
    		throw new XMLParseVisitorException("Cannot load imported document", e);
    	}
    	
    	return this.mImportedObject;
	}


    



}
