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

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Import.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.visitor.LazyImportVisitorService;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.bpel.xml.common.visitor.XMLParseVisitorException;


/**
 * @author Sun Microsystems
 * 
 * 
 */
public class ImportImpl extends BPELElementImpl implements Import {
	
	private Object mImportedObject;
	
	public ImportImpl() {
		super();
		initImport();
	}
	
	public ImportImpl(XMLDocument d) {
		super(d);
		initImport();
	}
	
	private void initImport() {
		setLocalName(Import.TAG);
		xmlAttrs = new XMLAttribute[] {
	            new XMLAttributeImpl(ATTR.NAMESPACE, String.class, false, null),
	            new XMLAttributeImpl(ATTR.LOCATION, String.class, false,
	                                 null),
	            new XMLAttributeImpl(ATTR.IMPORT_TYPE, String.class, true, null),
	            
	        };
	}
	
	public void setNamespace(String namespace) {
		setAttribute(NAMESPACE, namespace);
	}
	 
	public String getNamespace() {
		return xmlAttrs[NAMESPACE].getValue();
	}
	
	public void setLocation(String newLocation) {
		setAttribute(LOCATION, newLocation);
	}
	
	public String getLocation() {
		return xmlAttrs[LOCATION].getValue();
	}
	
	public void setImportType(String newImportType) {
		setAttribute(IMPORT_TYPE, newImportType);
	}
	
	public String getImportType() {
		return xmlAttrs[IMPORT_TYPE].getValue();
	}
	
	/**
     * set the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @param document top level document
     */
    public void setImportedObject(Object document) {
    	this.mImportedObject = document;
    }

    /**
     * get the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @return WSDLDocument or XMLSchema
     */
    public Object getImportedObject() {
    	if(this.mImportedObject != null) {
    		return this.mImportedObject;
    	}
    	
    	BPELParseContext bpelParseContext = null;
    	if(this.getOwnerDocument() != null) {
			bpelParseContext = ((BPELDocument) this.getOwnerDocument()).getBPELParseContext();
		}
    	
    	if(bpelParseContext == null) {
    		throw new IllegalArgumentException("BPELParseContext should not be null");
    	}
    	
    	try {
    		LazyImportVisitorService visitorService = new LazyImportVisitorService();
    		
    		
    		    		
    		visitorService.setBPELParseContext(bpelParseContext);
    		BPELVisitor visitor = (BPELVisitor) visitorService.fetch(BPELVisitor.class, null);
    		visitor.visit(this);
    	} catch (Exception e) {
    		throw new XMLParseVisitorException("Cannot load imported document "+ this, e);
    	}
    	
    	return this.mImportedObject;

    }
    
	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
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
}
