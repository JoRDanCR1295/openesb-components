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
 * @(#)BPELDocumentParseFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.io.Reader;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentFactory;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.visitor.SAXParseVisitorService;
import com.sun.bpel.xml.common.visitor.VisitorService;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class BPELDocumentParseFactoryImpl extends BPELDocumentParseFactory {
    
    /** Creates a new instance of BPELDocumentParseFactoryImpl. */
    public BPELDocumentParseFactoryImpl() {
    }
    
    public BPELDocument load(Reader reader, BPELParseContext context) {
    	if(reader == null) {
    		throw new IllegalArgumentException("Reader is null, must specify Reader.");
    	}
    	
    	if(context == null) {
    		throw new IllegalArgumentException("BPELParseContext is null, must specify BPELParseContext.");
    	}
    	
        BPELDocument document = context.getBPELDocumentFactory().newBPELDocument();
        load(reader, context, document);
        //No lazy imports.  Resolve all imports
        document.getDocumentProcess().getAllImportedWSDLDefinitions();
        document.getDocumentProcess().getAllImportedXMLSchemas();
        return document;
	}
    
    public void load(Reader reader, BPELParseContext context, BPELDocument document) {
    	if(reader == null) {
    		throw new IllegalArgumentException("Reader is null, must specify Reader.");
    	}
    	
    	if(context == null) {
    		throw new IllegalArgumentException("BPELParseContext is null, must specify BPELParseContext.");
    	}
    	
    	if(document == null) {
    		throw new IllegalArgumentException("BPELDocument is null, must specify BPELDocument.");
    	}
    	
    	try {
    	SAXParseVisitorService visitorService = new SAXParseVisitorService();
    	visitorService.setBPELParseContext(context);
        
    	document.reset();
        document.setBPELParseContext(context);
        document.setEnableEvents(context.isEnableEvents());
        
        Object[] params = new Object[] {reader};
        traverse(document, visitorService, params);
    	} catch(RuntimeException ex) {
    		//if parse error ocuured set document process to null rethrow exception
    		document.setDocumentProcess(null);
    		throw ex;
    	}
        
	}

	/** @see com.sun.bpel.model.BPELDocument#traverse(com.sun.bpel.model.common.visitor.VisitorService,
     *  java.lang.Object[])
     */
    private void traverse(BPELDocument document, VisitorService s, Object[] v) {
        BPELVisitor visitor = (BPELVisitor) s.fetch(BPELVisitor.class, null);
        visitor.prepare(v);
        document.accept(visitor);
    }
    
    
}
