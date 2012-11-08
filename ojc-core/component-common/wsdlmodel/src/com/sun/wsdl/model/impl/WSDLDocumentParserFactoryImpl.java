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
 * @(#)WSDLDocumentParserFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import java.io.Reader;
import java.util.Iterator;
import java.util.List;

import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLDocumentParserFactory;
import com.sun.wsdl.model.WSDLElement;
import com.sun.wsdl.model.WSDLParseContext;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.impl.FileURIResolverImpl;
import com.sun.wsdl.model.visitor.SAXParseVisitor;
import com.sun.wsdl.model.visitor.SAXParseVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class WSDLDocumentParserFactoryImpl extends WSDLDocumentParserFactory {
    
    /** Creates a new instance of WSDLDocumentParserFactoryImpl */
    public WSDLDocumentParserFactoryImpl() {
    }
    
    /** @see WSDLDocumentParserFactory#load
     */
    public WSDLDocument load(Reader reader, String baseURI) {
    	WSDLDocumentImpl doc = new WSDLDocumentImpl();
    	doc.setBaseURI(baseURI);
    	loadWSDL(doc, baseURI, reader, new WSDLParseContext.DefaultParseContext());
    	
    	return doc;
    }
    
    /** @see WSDLDocumentParserFactory#load
     */
    public void load(WSDLDocument doc,
    				 Reader reader,
					 WSDLParseContext context) {
        
    	//set enable events
    	if(context != null) {
    		doc.setEnableEvents(context.isEnableEvents());
    	}
    	
    	loadWSDL(doc, doc.getBaseURI(), reader, context);
    }
    
    
    public void load(WSDLElement element, Reader reader, WSDLParseContext context) {
    	WSDLDocument wsdlDocument = (WSDLDocument) element.getOwnerDocument();
        String baseURI = null;
        if(wsdlDocument != null) {
        	baseURI = wsdlDocument.getBaseURI();
        }
    	loadWSDL(element, baseURI, reader, context);
	}

	/**
     * @see com.sun.wsdl.model.WSDLDocument#load(java.io.Reader)
     */
    private void loadWSDL(XMLElement element,
    					  String baseURI,
    					  Reader reader, 
						  WSDLParseContext context) {
        
    	BaseURIResolver resolver = context.getBaseURIResolver();
    	//use FileURIResolver by default
    	if(resolver == null) {
    		resolver = new FileURIResolverImpl(null, baseURI);
    	}
    	
    	Object[] params = new Object[] {
            reader,
            resolver,   // Assumed to be file/http based
            null,   // No parent; assume only model JUNit test
        };
        
        loadWSDL(element, baseURI, params, context);
    }
    
    /**
     * 
     */
    private void loadWSDL(XMLElement element, String baseURI, Object[] params, WSDLParseContext context) {
        try {
        	//use default context by default
        	if (context == null) {
        		context = new WSDLParseContext.DefaultParseContext();
        	}
        	
        	
        	SAXParseVisitorService visitorService = new SAXParseVisitorService();
        	visitorService.setWSDLParseContext(context);
            visitorService.setParseImportedXmlSchemas(context.isParseImportedSchemas());
            visitorService.setErrorHandler(context.getErrorHandler());
            traverse(element, baseURI, visitorService, params);
        } catch (Exception e) {
            throw new XMLParseVisitorException("Cannot load WSDL "+ baseURI, e);
        }
    }
    
    /**
     * Traverses the document to perform some work by the visitor
     * that is provided by the service. 
     *
     * @param   s   Visitor service provider.
     * @param   v   Values to prepare the persistor.
     */
    private void traverse(XMLElement element, String baseURI, VisitorService s, Object[] v) {
    	try {
    		removeChildren(element);
    		SAXParseVisitor visitor = (SAXParseVisitor) s.fetch(WSDLVisitor.class, null);
	        visitor.prepare(v);
	        visitor.createXMLReader();
	        element.accept(visitor);
	        visitor.startParsing(baseURI);
    	} catch (Exception e) {
            throw new XMLParseVisitorException("Error parsing wsdl "+ baseURI, e);
        }
    }
    
    private void removeChildren(XMLElement element) {
    	List children = element.getChildren();
		if(children != null) {
			Iterator it = children.iterator();
			while(it.hasNext()) {
				XMLNode child = (XMLNode) it.next();
				element.removeChild(child);
			}
		}
		
    }
}
