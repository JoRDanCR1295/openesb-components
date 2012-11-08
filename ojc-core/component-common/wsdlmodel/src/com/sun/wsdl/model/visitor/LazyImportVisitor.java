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

package com.sun.wsdl.model.visitor;

import com.sun.jbi.internationalization.Messages;
import java.io.Reader;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.exolab.castor.net.URILocation;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.reader.SchemaLocation;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.xml.sax.InputSource;

import com.sun.wsdl.model.FastWSDLDefinitions;
import com.sun.wsdl.model.FastWSDLDefinitionsFactory;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLDocumentFactory;
import com.sun.wsdl.model.WSDLDocumentParserFactory;
import com.sun.wsdl.model.common.MessageManager;
import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.wsdl.model.common.util.Namespaces;
import com.sun.wsdl.model.common.visitor.AutonomousVisitor;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.xsd.XMLSchema;
import com.sun.wsdl.model.xsd.XMLSchemaFactory;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LazyImportVisitor extends AbstractVisitor implements AutonomousVisitor {
	
    /** The logger. */
    private static final Messages MESSAGES = 
            Messages.getMessages(LazyImportVisitor.class);
    private static final Logger LOGGER = 
            Messages.getLogger(LazyImportVisitor.class);
   
    /** Message key for MUST_SPECIFY_PARENT_PROJECT_ELEM */
    private static final String MUST_SPECIFY_PARENT_PROJECT_ELEM = "MUST_SPECIFY_PARENT_PROJECT_ELEM";  // Not I18N
    
    /** Message key for MUST_SPECIFY_URI_RESOLVER_FACTORY */
    private static final String MUST_SPECIFY_URI_RESOLVER_FACTORY = "MUST_SPECIFY_URI_RESOLVER_FACTORY";  // Not I18N
    
    /** Message key for THRW_UNRECOGNIZED_START_ELEMENT */
    private static final String THRW_UNRECOGNIZED_START_ELEMENT = "THRW_UNRECOGNIZED_START_ELEMENT";  // Not I18N
    
    /** Message key for THRW_UNRECOGNIZED_END_ELEMENT */
    private static final String THRW_UNRECOGNIZED_END_ELEMENT = "THRW_UNRECOGNIZED_END_ELEMENT";  // Not I18N
    
    /** Message key for ILLEGAL_ELEMENT_ENCOUNTERED */
    private static final String ILLEGAL_ELEMENT_ENCOUNTERED = "ILLEGAL_ELEMENT_ENCOUNTERED";  // Not I18N
    
    /** Message key for THRW_MISSING_REQUIRED_ATTRIBUTE */
    private static final String THRW_MISSING_REQUIRED_ATTRIBUTE = "THRW_MISSING_REQUIRED_ATTRIBUTE";  // Not I18N
    
    /** Message key for THRW_IMPORT_PROBLEM */
    private static final String THRW_IMPORT_PROBLEM = "THRW_IMPORT_PROBLEM";  // Not I18N
    
    
    /** MessageManager for localized strings. */    
    //private MessageManager mMsg = MessageManager.getManager(getClass());
    
	private BaseURIResolver mUriResolver;

	public LazyImportVisitor() {
	}
	
	
	public void prepare(Object[] v) {
		if (v.length > 0 && (v[0] != null) && (v[0] instanceof BaseURIResolver)) {
			setBaseURIResolver((BaseURIResolver) v[0]);
        }
	}
	
	/**
     * @see WSDLVisitor#visit(WSDLDefinitions)
     */
    public boolean visit(WSDLDocument d) {
    	if(d.getDocumentDefinitions() != null) {
    		return visit(d.getDocumentDefinitions());
    	}
    	
    	return false;
    }
    	
	public boolean visit(WSDLDefinitions d) {
        Iterator it = d.getImports().iterator();
        while(it.hasNext()) {
        	Import imp = (Import) it.next();
    		visit(imp);
        }
        
        return true;
    }
    
	
	public boolean visit(Import wsdlImport) {
		importDocument(wsdlImport);
        return true;
    }
	
	/**
     * Imports a document (WSDL or XML Schema).
     * @param wsdlImport the WSDL import element
     */
    public void importDocument(Import wsdlImport) {
    	WSDLDocument document = (WSDLDocument) wsdlImport.getOwnerDocument();
    	
        String namespace = wsdlImport.getNamespaceAttr();
        String location = wsdlImport.getLocation();
        if ((null == location) || (location.trim().length() == 0)) {
            LOGGER.log(Level.FINE,
                    MESSAGES.getString("LazyImportVisitor.UNKNOWN_IMPORT_TYPE_NAMESPACE_x_LOCATN_NOT_SPECIFIED",
                    new Object[] {namespace} ));
        } else {
            Namespaces nsResult = null;
            String docType = null;
            try {
                URILocation foundLoc = null;
                BaseURIResolver resolver = null;
                if (getBaseURIResolver() != null) {
                	resolver = getBaseURIResolver();
                } else {
                    throw new EInsightModelException(
                            MESSAGES.getString("LazyImportVisitor.MUST_SPECIFY_URI_RESOLVER_FACTORY"));
                }

                foundLoc = resolver.resolve(location, wsdlImport.getOwnerDocument().getBaseURI());
                if (null == foundLoc) {
                    foundLoc = resolver.resolveURN(namespace);
                }
                
                Reader rdr;
                if (foundLoc != null) {
                    String foundLocStr = foundLoc.getAbsoluteURI().toLowerCase();
                    
                    if (foundLocStr.endsWith(".bpel")) {
                        docType = "bpel";
                    } else if (foundLocStr.endsWith(".wsdl")) {
                        docType = "wsdl";
                    } else if (foundLocStr.endsWith(".xsd")) {
                        docType = "xsd";
                    } else if ((rdr = foundLoc.getReader()) != null) {
                    	FastWSDLDefinitions fastDef = FastWSDLDefinitionsFactory.getInstance().newFastWSDLDefinitions(foundLoc.getReader(), false);
						if(fastDef != null && fastDef.isWSDL()) {
							docType = "wsdl";
                        } else {
                            docType = "xsd";
                        }
						
                    } else if (foundLoc instanceof SchemaLocation) {
                        docType = "xsd";  // shallow import of XML Schema
                    }
                } else {
                	
                	//can not resolve import to a urn throw exception
                	throw new XMLParseVisitorException(
                                MESSAGES.getString(
                                "LazyImportVisitor.THRW_IMPORT_PROBLEM",
                                new Object[] { 
                                    document.getBaseURI(),
                                    location } ) );
                }
                
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE,
                        MESSAGES.getString(
                        "LazyImportVisitor.x_UNABLE_TO_IMPORT_DOCUMENT_x",
                        new Object[] {
                            document.getBaseURI(),
                            location } ), e);

                throw new XMLParseVisitorException(
                                MESSAGES.getString(
                                "LazyImportVisitor.THRW_IMPORT_PROBLEM",
                                new Object[] {
                                    document.getBaseURI(),
                                    location } ), e);
            }
            
            if ("wsdl".equals(docType)) {
                LOGGER.log(Level.FINE,
                        MESSAGES.getString(
                        "LazyImportVisitor.ATTEMPTING_TO_IMPORT_WSDL_x",
                        new Object[] {location} ));
                importWSDLDocument(wsdlImport);
            } else if ("xsd".equals(docType)) {
            	//do we really want to imported parse xml schemas
            	VisitorService vs = getVisitorService();
                if(vs instanceof LazyImportVisitorService) {
                	LazyImportVisitorService svs = (LazyImportVisitorService) vs;
                	if(!svs.getWSDLParseContext().isParseImportedSchemas()) {
                		return;
                	}
                }
                
                LOGGER.log(Level.FINE,
                        MESSAGES.getString(
                        "LazyImportVisitor.ATTEMPTING_TO_IMPORT_XSD_x",
                        new Object[] {location} ));
                importXMLSchema(wsdlImport);
            } else {
            	 LOGGER.log(Level.SEVERE,
                         MESSAGES.getString(
                         "LazyImportVisitor.x_UNHANDLED_IMPORT_TYPE_NAMESPACE_x_LOCATION_x_MUST_BE_WSDL_XSD",
                         new Object[] {
                            document.getBaseURI(), 
                            namespace,
                            location } ));
                              
                throw new XMLParseVisitorException(
                         MESSAGES.getString(
                        "LazyImportVisitor.x_UNHANDLED_IMPORT_TYPE_NAMESPACE_x_LOCATION_x_MUST_BE_WSDL_XSD",
                         new Object[] {
                            document.getBaseURI(), 
                            namespace,
                            location } ));
            }
        }
    }

    /**
     * Imports a WSDL document.
     * @param wsdlImport the WSDL import element
     */
    public void importWSDLDocument(Import wsdlImport) {
        String namespace = wsdlImport.getNamespaceAttr();
        String location = wsdlImport.getLocation();
        URILocation foundLoc = null;
        String absoluteURI = location;
        BaseURIResolver resolver = null;
        boolean started = false;
        try {
            if (getBaseURIResolver() != null) {
            	resolver = getBaseURIResolver();
            } else {
                throw new EInsightModelException(
                        MESSAGES.getString(
                        "LazyImportVisitor.MUST_SPECIFY_URI_RESOLVER_FACTORY"));
            }
            
                            
            foundLoc = resolver.resolve(location, wsdlImport.getOwnerDocument().getBaseURI());
            if (null == foundLoc) {
                foundLoc = resolver.resolveURN(namespace);
            }
            
            if (foundLoc != null) {
                absoluteURI = foundLoc.getAbsoluteURI();
                
                WSDLDocument owner = (WSDLDocument) wsdlImport.getOwnerDocument();

                WSDLDocument newDoc = null;
                if (null == newDoc) {
                	LazyImportVisitorService vService = (LazyImportVisitorService) getVisitorService();
                    newDoc = WSDLDocumentFactory.getInstance().newWSDLDocument();
                    newDoc.setBaseURI(absoluteURI);
                    WSDLDocumentParserFactory.getInstance().load(newDoc, foundLoc.getReader(), vService.getWSDLParseContext());
                    wsdlImport.setImportedObject(newDoc);
                    
                    LOGGER.log(Level.FINE,
                            MESSAGES.getString(
                            "LazyImportVisitor.ADDED_IMPORTED_DOC_x",
                            new Object[] {newDoc.getDocumentDefinitions()} ));
                    
                    started = true;

                                           // Gotta bail because something screwed up
                    if (null == newDoc) {
                        throw new XMLParseVisitorException(
                            MESSAGES.getString(
                                "LazyImportVisitor.NO_URIRESOLVER_GIVEN_TO_THIS_SAXPARSEVISITOR"));
                    }

                    newDoc.setBaseURI(absoluteURI);

                  
                } 
                

            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE,
                    MESSAGES.getString(
                    "LazyImportVisitor.UNABLE_TO_IMPORT_DOC_x",
                    new Object[] {absoluteURI}), e);
            throw new XMLParseVisitorException(
                    MESSAGES.getString(
                    "LazyImportVisitor.THRW_IMPORT_PROBLEM_x_CANNOT_FIND_DOC_SOURCE",
                    new Object[]{ absoluteURI} ), e );
        }
        
        if (null == foundLoc) {
            LOGGER.log(Level.SEVERE,
                    MESSAGES.getString(
                    "LazyImportVisitor.UNABLE_TO_IMPORT_DOC_CANT_FIND_DOC_SOURCE_x",
                    new Object[]{absoluteURI}));
            
            throw new XMLParseVisitorException(
                MESSAGES.getString(
                    "LazyImportVisitor.THRW_IMPORT_PROBLEM_x_CANNOT_FIND_DOC_SOURCE",
                    new Object[]{ absoluteURI} ));
        }
    }
    /** Does a shallow import of a WSDL document (i.e., tries to find an existing one matching the
     * source location, relative to the specified mount point and base project).  Other WSDL/XSD that
     * this found WSDL imports will not be processed.
     *
     * @param   uriLoc  URI location of the WSDL to be found.
     * @param   wsro    Parent BPEL/WSDL repository object.
     * @param   def     WSDL definitions element of the parent.
     * @param   imp     Import directive element.
     * @throws  RepositoryException     When repository problems occur.
     */
    
    /**
     * Imports an XML Schema.
     * @param wsdImport the WSDL import element
     * @throws XMLParseVisitorException if document to import is invalid
     */
    private void importXMLSchema(Import wsdlImport) {
    	String namespace = wsdlImport.getNamespaceAttr();
        String location = wsdlImport.getLocation();
        URILocation foundLoc = null;
        String absoluteURI = location;
        XMLParseVisitorException toBeThrown = null;
        boolean started = false;
        try {
            if (getBaseURIResolver() == null) {
                throw new EInsightModelException(
                        MESSAGES.getString("LazyImportVisitor.MUST_SPECIFY_URI_RESOLVER_FACTORY"));
            }
            BaseURIResolver resolver = getBaseURIResolver();
            
            foundLoc = resolver.resolve(location, wsdlImport.getOwnerDocument().getBaseURI());
            if (null == foundLoc) {
                foundLoc = resolver.resolveURN(namespace);
            }
            
            if (foundLoc != null) {
                absoluteURI = foundLoc.getAbsoluteURI();
                
                // Shallow import
                {
                    try {
                        InputSource inSrc = new InputSource(foundLoc.getReader());
                        inSrc.setSystemId(absoluteURI);
                        SchemaReader schRdr = new SchemaReader(inSrc);
                        schRdr.setURIResolver(resolver);
                        schRdr.addPropertyChangeListener(resolver);
                        LazyImportVisitorService vService = (LazyImportVisitorService) getVisitorService();
                        schRdr.setValidation(vService.getWSDLParseContext().isValidateSchema());
                        
                        Schema schema = schRdr.read();
                        	
                         XMLSchema xmlSchema  = XMLSchemaFactory.getInstance().newXMLSchema(); 
                         xmlSchema.setSchema(schema);	
                         xmlSchema.setBaseURI(absoluteURI);
                        wsdlImport.setImportedObject(xmlSchema);
                        LOGGER.log(Level.FINE,
                                MESSAGES.getString(
                                "LazyImportVisitor.IMPORTED_SCHEMA_x",
                                new Object[] {xmlSchema.getBaseURI()} ));
                        
                    } catch (Exception e) {
                        toBeThrown = new XMLParseVisitorException(
                                MESSAGES.getString(
                                "LazyImportVisitor.CANNOT_PARSE"), e);
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE,
                    MESSAGES.getString(
                    "LazyImportVisitor.UNABLE_TO_IMPORT_DOC_x",
                    new Object[] {absoluteURI}), e);
            toBeThrown = new XMLParseVisitorException(
                MESSAGES.getString(
                    "LazyImportVisitor.THRW_IMPORT_PROBLEM_x_CANNOT_FIND_DOC_SOURCE_EXCEPTION_x",
                    new Object[] {absoluteURI, e.getMessage()} ), e);
        }
        
        if (toBeThrown != null) {
            throw toBeThrown;
        }
        
        if (null == foundLoc) {
            LOGGER.log(Level.SEVERE,
                    MESSAGES.getString(
                    "LazyImportVisitor.UNABLE_TO_IMPORT_DOC_CANT_FIND_DOC_SOURCE_x",
                    new Object[] {absoluteURI} ));
            
            throw new XMLParseVisitorException(
                MESSAGES.getString(
                    "LazyImportVisitor.THRW_IMPORT_PROBLEM_x_CANNOT_FIND_DOC_SOURCE",
                    new Object[] {absoluteURI} ));
        }
    }

	public BaseURIResolver getBaseURIResolver() {
        return mUriResolver;
    }
    
    public void setBaseURIResolver(BaseURIResolver uriResolver) {
        this.mUriResolver = uriResolver;
    }
    
    
}
