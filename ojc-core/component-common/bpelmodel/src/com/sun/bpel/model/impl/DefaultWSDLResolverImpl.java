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
 * @(#)DefaultWSDLResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;

import org.xml.sax.InputSource;

import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.xml.uri.URILocation;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.wsdl.WSDLParseContext;
import com.sun.bpel.xml.wsdl.impl.WSDLDocumentImpl;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class DefaultWSDLResolverImpl implements IWSDLResolver {

	private Logger mLogger = Logger.getLogger(DefaultWSDLResolverImpl.class.getName());
	
	private String mBaseURI;
	
	private URI mBaseBpelURI;
	
	private BPELParseContext mParseContext;
	
	public DefaultWSDLResolverImpl(String baseURI, BPELParseContext parseContext) throws EInsightModelException {
		if(baseURI == null) {
			throw new EInsightModelException("bpel document baseURI is null." + baseURI);
		}
		
		if(parseContext == null) {
			throw new EInsightModelException("WSDLParseContext is null");
		}
		
		try {
			this.mBaseBpelURI = new URI(baseURI);
		} catch (URISyntaxException ex) {
			throw new EInsightModelException("failed to get file object for "+ baseURI);
		}
		
		this.mBaseURI = baseURI;
		this.mParseContext = parseContext;
	}
	
	public File findWSDLFile(String bpelFilePath, String fileName) {
		//by default assume that wsdl is in same dir as bpel file
		File bpelFile = new File(bpelFilePath);
		File matchedWSDLFile = new File(bpelFile.getParentFile(), fileName);
		
		return matchedWSDLFile;
		
	}


	public URI getBaseBpelURI() {
		return this.mBaseBpelURI;
	}
	
	public WSDLDocument resolve(String publicId, String systemId) throws EInsightModelException {
		try {
	      //by default assume that wsdl is in same dir as bpel file
			if(systemId != null) {
                URILocation redirected = mParseContext.getBaseURIResolver().resolve(systemId, mBaseBpelURI.toString());
                
//				URI matchedXSDURI = null;
//				URI systemIdURI = new URI(systemId);
//				
//				//if not an absolute uri then resolve it with respect to base uri
//				if(!systemIdURI.isAbsolute()) {
//					matchedXSDURI = this.mBaseBpelURI.resolve(systemId);
//	                
//				} else {
//					matchedXSDURI = systemIdURI;
//				}
//				
//                
//				URL url = matchedXSDURI.toURL();
				
//				InputStreamReader reader = new InputStreamReader(url.openStream());
                Reader reader = redirected.getReader();
			    return parseWSDL(reader, new URI(redirected.getAbsoluteURI()).toURL());
		        
			}
        }catch(Throwable ex) {
        	mLogger.log(Level.WARNING, "Failed to resolve publicId "+ publicId + " and systemId "+ systemId + " to a WSDLDocument.", ex);
        	throw new EInsightModelException("Failed to resolve publicId "+ publicId + " and systemId "+ systemId + " to a WSDLDocument.", ex);
        }	
        
        return null;
        
	}
	
	protected WSDLDocument parseWSDL(Reader reader, URL systemIdURL) throws EInsightModelException {
		WSDLDocument document =
		    mParseContext.getCaches().getWSDLCacheForBPEL().get(systemIdURL.toExternalForm());
        if (document != null) {
            return document;
        }
		try {
		    
	        WSDLParseContext context = this.mParseContext.getWSDLParseContext();
	        
	        if(context == null) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine("Using DefaultParseContext for wsdl");
                    }
	        	context = new WSDLParseContext.DefaultParseContext(
	        	        mParseContext.getCatalogResolver(), systemIdURL.toExternalForm());
	        }
	        
	        WSDLReader wsdlReader =
	            WSDL4JExt.newWSDLReader(
	                    mParseContext.getBaseURIResolver(),
	                    mParseContext.getDeferredActionRegistry(),
	                    mParseContext.getCaches().getWSDLCacheForWSDL(),
	                    mParseContext.getCaches().getXSDCacheForWSDL());
	        if (mLogger.isLoggable(Level.FINE)) {
                    wsdlReader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);	            
	        } else {
	            wsdlReader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
	        }
	        InputSource input = new InputSource(reader);
	        input.setSystemId(systemIdURL.toString());
	        Definition wsdlDef = wsdlReader.readWSDL(systemIdURL.toString(), input);
	        document = new WSDLDocumentImpl(systemIdURL.toString(), wsdlDef);
		} catch (Exception ex) {
			mLogger.log(Level.SEVERE, "Failed to parser xsd document ", ex);
			
			throw new EInsightModelException("Failed to parser xsd document "+ systemIdURL, ex );
		}
	    mParseContext.getCaches().getWSDLCacheForBPEL().put(systemIdURL.toExternalForm(), document);
		return document;
	}
}
