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
 * @(#)LazyImportVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.common.visitor.XMLParseVisitorException;
import com.sun.bpel.xml.uri.BaseURIResolver;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LazyImportVisitor extends AbstractVisitor  {
	
	 /** The logger. */
    private static Logger mLogger = Logger.getLogger(LazyImportVisitor.class.getName());
    
	private BaseURIResolver mUriResolver;

	public LazyImportVisitor() {
	}
	
	
	public void prepare(Object[] v) {
		if (v.length > 0 && (v[0] != null) && (v[0] instanceof BaseURIResolver)) {
			setBaseURIResolver((BaseURIResolver) v[0]);
        }
	}
		
	public boolean visit(Import bpelImport) {
		importDocument(bpelImport);
        return true;
    }
	
	/**
     * Imports a document (WSDL or XML Schema).
     * @param wsdlImport the WSDL import element
     */
    public void importDocument(Import bpelImport) {
    	String importType = bpelImport.getImportType();
    	LazyImportVisitorService vService = (LazyImportVisitorService) getVisitorService();
    	BPELParseContext bpelParseContext = vService.getBPELParseContext();
    	
    	if(Import.WSDL_IMPORT_TYPE.equals(importType) && bpelParseContext.isLoadImportedWsdls()) {
    		importWSDLDocument(bpelImport);
    	} else if(Import.XSD_IMPORT_TYPE.equals(importType) && bpelParseContext.isLoadImportedXsds()) {
    		importXMLSchema(bpelImport);
    	} else {
    		throw new XMLParseVisitorException("importType "+ importType + "is not a valid value in "+ bpelImport + "It should be either "+ Import.WSDL_IMPORT_TYPE + " or "+ Import.XSD_IMPORT_TYPE );
    	}
    }

    /**
     * Imports a WSDL document.
     * @param wsdlImport the WSDL import element
     */
    private void importWSDLDocument(Import bpelImport) {
    	String namespace = bpelImport.getNamespace();
    	String location = bpelImport.getLocation();
    	if (location == null) {
            mLogger.severe("Unable to import wsdl document, import location is null " + bpelImport);
            throw new XMLParseVisitorException("Unable to import wsdl document, import location is null " + bpelImport);
        }
    	
    	LazyImportVisitorService vService = (LazyImportVisitorService) getVisitorService();
    	BPELParseContext bpelParseContext = vService.getBPELParseContext();
    	IWSDLResolver wsdlResolver = bpelParseContext.getWSDLResolver();
    	
    	if(wsdlResolver == null) {
    		mLogger.severe("Unable to import wsdl document, must specify WSDL Resolver " + bpelImport);
            throw new XMLParseVisitorException("Unable to import wsdl document, must specify WSDL Resolver " + bpelImport);
    	}
    	
    	try {
	    	WSDLDocument wsdlDocument = wsdlResolver.resolve(namespace, location);
	        
	        if(wsdlDocument == null) {
	        	mLogger.severe("Unable to import wsdl document for import " + bpelImport);
	            throw new XMLParseVisitorException("Unable to import wsdl document for import " + bpelImport);	
	        }
	        
	        bpelImport.setImportedObject(wsdlDocument);
                  
       } catch (EInsightModelException e) {
            mLogger.log(Level.SEVERE, "Unable to import wsdl document for import " + bpelImport, e);
            throw new XMLParseVisitorException("Unable to import wsdl document for import " + bpelImport, e);
        }
        
    }
    
    
    /**
     * Imports a XSD document.
     * @param wsdlImport the xsd import element
     */
    private void importXMLSchema(Import bpelImport) {
    	String namespace = bpelImport.getNamespace();
    	String location = bpelImport.getLocation();
    	if (location == null) {
            mLogger.severe("Unable to import schema document, import location is null " + bpelImport);
            throw new XMLParseVisitorException("Unable to import schema document, import location is null " + bpelImport);
        }
    	
    	LazyImportVisitorService vService = (LazyImportVisitorService) getVisitorService();
    	BPELParseContext bpelParseContext = vService.getBPELParseContext();
    	IXSDResolver xsdResolver = bpelParseContext.getXSDResolver();
    	
    	if(xsdResolver == null) {
    		mLogger.severe("Unable to import schema document, must specify XSD Resolver " + bpelImport);
            throw new XMLParseVisitorException("Unable to import schema document, must specify XSD Resolver " + bpelImport);
    	}
    	
    	try {
	    	XMLSchema xsdDocument = xsdResolver.resolve(namespace, location);
	        
	        if(xsdDocument == null) {
	        	mLogger.severe("Unable to import schema document for import " + bpelImport);
	            throw new XMLParseVisitorException("Unable to import schema document for import " + bpelImport);	
	        }
	        
	        bpelImport.setImportedObject(xsdDocument);
                  
       } catch (EInsightModelException e) {
            mLogger.log(Level.SEVERE, "Unable to import xsd document for import " + bpelImport, e);
            throw new XMLParseVisitorException("Unable to import xsd document for import " + bpelImport, e);
        }
        
    }

	public BaseURIResolver getBaseURIResolver() {
        return mUriResolver;
    }
    
    public void setBaseURIResolver(BaseURIResolver uriResolver) {
        this.mUriResolver = uriResolver;
    }




    public boolean visit(ElseIf d) {
        // TODO Auto-generated method stub
        return false;
    }


    public boolean visit(Else d) {
        // TODO Auto-generated method stub
        return false;
    }


    public boolean visit(ExtensionAssignOperation extensionAssignOperation) {
        // TODO Auto-generated method stub
        return false;
    }


    public boolean visit(SunExtExpression expression) {
        // TODO Auto-generated method stub
        return false;
    }
}
