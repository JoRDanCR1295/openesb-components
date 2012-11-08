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
 * @(#)WSDLParseContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.io.File;
import java.io.InputStream;

import org.xml.sax.ErrorHandler;

import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.CatalogResolver;
import com.sun.wsdl.model.uri.FileURIResolverFactory;
import com.sun.wsdl.model.uri.impl.CatalogResolverImpl;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class WSDLParseContext {
	
	private InputStream[] mWSDLExtensionsSchemas = null;
	
	public abstract ErrorHandler getErrorHandler();
	
	public abstract void setErrorHandler(ErrorHandler errorHandler);
	
	public abstract boolean isParseImportedSchemas();
	
	public abstract void setParseImportedSchemas(boolean parse);
	
	public abstract boolean isParseInlineSchema();
	
	public abstract void setParseInlineSchema(boolean parse);
	
	public abstract void setValidateSchema(boolean validate);
	
	public abstract boolean isValidateSchema();
	
	public abstract boolean isParseImportedWsdls();
	
	public abstract void setParseImportedWsdls(boolean parse);

    protected CatalogResolver mCatalogResolver;
    
    protected String mBaseUri;
    
    private BaseURIResolver mURIResolver;
   
   
    public BaseURIResolver getBaseURIResolver() {
        if (mURIResolver == null) {
            mURIResolver = FileURIResolverFactory.getInstance().getURIResolver(mCatalogResolver, mBaseUri);   
        }
        return mURIResolver;

    }    
	
	public InputStream[] getWSDLExtensionSchemas() {
		return mWSDLExtensionsSchemas;
	}
	
	public void setWSDLExtensionSchemas(InputStream[] ins) {
		this.mWSDLExtensionsSchemas = ins;
	}
	
	/**
	 * Enable event firing from XMLELement/XMLNode
	 * @param enable
	 */
	public abstract void setEnableEvents(boolean enable);
	
	public abstract boolean isEnableEvents();
	
	private static WSDLParseContext mWsdlParseContext;
	
	
	
	public static class DefaultParseContext extends WSDLParseContext {
		
		private ErrorHandler mErrorHandler = null;
		
		private boolean mParseImportedSchemas = true;
		
		private boolean mParseInlineSchema = true;
		
		private boolean mValidateSchema = true;
		
		private boolean mParseImportedWsdls = true;
		
//		private BaseURIResolver mURIResolver;
		
		private boolean mEnableEvent = true;
        
        public DefaultParseContext () {
            super ();
        }
        
        public DefaultParseContext (CatalogResolver catalogResolver, String baseuri) {
            super();
            mCatalogResolver = catalogResolver;
            mBaseUri = baseuri;            
        }
		
		public ErrorHandler getErrorHandler() {
			return mErrorHandler;
		}
		
		public void setErrorHandler(ErrorHandler errorHandler) {
			this.mErrorHandler = errorHandler;
		}
		
		public boolean isParseImportedSchemas() {
			return mParseImportedSchemas;
		}
		
		public void setParseImportedSchemas(boolean parse) {
			this.mParseImportedSchemas = parse;
		}
		
		public boolean isParseImportedWsdls() {
			return mParseImportedWsdls;
		}
		
		public void setParseImportedWsdls(boolean parse) {
			this.mParseImportedWsdls = parse;
		}
		
		public boolean isParseInlineSchema() {
			return this.mParseInlineSchema;
		}
		
		public void setParseInlineSchema(boolean parse) {
			this.mParseInlineSchema = parse;
		}
		
		public void setValidateSchema(boolean validate) {
			this.mValidateSchema = validate;
		}
		
		public boolean isValidateSchema() {
			return this.mValidateSchema;
		}
		
		/**
		 * Enable event firing from XMLELement/XMLNode
		 * @param enable
		 */
		public void setEnableEvents(boolean enable) {
			this.mEnableEvent = enable;
		}
		
		public  boolean isEnableEvents() {
			return this.mEnableEvent;
		}
		
}
}
