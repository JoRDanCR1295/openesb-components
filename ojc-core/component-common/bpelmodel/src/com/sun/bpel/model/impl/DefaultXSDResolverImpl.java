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
 * @(#)DefaultXSDResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.xml.sax.InputSource;

import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.bpel.xml.uri.URILocation;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.bpel.xml.xsd.impl.XMLSchemaImpl;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class DefaultXSDResolverImpl implements IXSDResolver {

	private Logger mLogger = Logger.getLogger(DefaultWSDLResolverImpl.class.getName());
	
	private URI mBaseBpelURI;
	
	private BPELParseContext mParseContext;
	
	public DefaultXSDResolverImpl(String baseURI, BPELParseContext parseContext) throws EInsightModelException {
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
		
		this.mParseContext = parseContext;
	}
	
	public XMLSchema resolve(String publicId, String systemId) throws EInsightModelException {
		XMLSchema xmlSchema = null;
        String resolvedURI = null;
                
		try {
			//by default assume that wsdl is in same dir as bpel file
			if(systemId != null) {
/*				URI matchedXSDURI = null;
				URI systemIdURI = new URI(systemId);
				
				//if not an absolute uri then resolve it with respect to base uri
				if(!systemIdURI.isAbsolute()) {
					matchedXSDURI = this.mBaseBpelURI.resolve(systemId);
	                
				} else {
					matchedXSDURI = systemIdURI;
				}
				
				URL url = matchedXSDURI.toURL();
				resolvedURI = matchedXSDURI.toString();
				
				InputStreamReader reader = new InputStreamReader(url.openStream());
				xmlSchema  = parseXMLSchema(reader, url);
*/
			    URILocation redirected = mParseContext.getBaseURIResolver().resolve(systemId, mBaseBpelURI.toString());
			    Reader reader = redirected.getReader();
			    return parseXMLSchema(reader, new URI(redirected.getAbsoluteURI()).toURL());
			}
		
		} catch (Exception ex) {
			mLogger.log(Level.SEVERE, "Failed to parser xsd document publicId = "+ publicId + " systemId = " + systemId + " resolved URL " + resolvedURI, ex);
			
			throw new EInsightModelException("Failed to parser xsd document publicId = "+ publicId + " systemId = " + systemId + " resolved URL " + resolvedURI, ex );
		}
		
		return xmlSchema;
		
	}

	protected XMLSchema parseXMLSchema(Reader reader, URL systemIdURL) throws EInsightModelException {
		XMLSchema xmlSchema =
		    mParseContext.getCaches().getXSDCacheForBPEL().get(systemIdURL.toExternalForm());
        if (xmlSchema != null) {
            return xmlSchema;
        }
		try {
		    XmlOptions options = new XmlOptions();
		    options.setDocumentSourceName(systemIdURL.toString());
		    options.setEntityResolver(this.mParseContext.getBaseURIResolver());
		    SchemaDocument doc = SchemaDocument.Factory.parse(reader, options);
			if (this.mParseContext.isValidateSchema()) {
			    doc.validate();
			}
			xmlSchema = new XMLSchemaImpl(systemIdURL.toString(), doc.getSchema());
            mParseContext.getDeferredActionRegistry().
                getSchemaDocumentCollector().add(doc.getSchema());
            mParseContext.getDeferredActionRegistry().
                getActionAccepterCollector().add(xmlSchema);
		} catch (Exception ex) {
			mLogger.log(Level.SEVERE, "Failed to parser xsd document ", ex);
			
			throw new EInsightModelException("Failed to parser xsd document "+ systemIdURL, ex );
		}
		
	    mParseContext.getCaches().getXSDCacheForBPEL().put(systemIdURL.toExternalForm(), xmlSchema);
		
		return xmlSchema;
	}
}
