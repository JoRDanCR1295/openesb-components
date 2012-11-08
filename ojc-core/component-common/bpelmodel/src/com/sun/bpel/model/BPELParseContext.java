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
 * @(#)BPELParseContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.File;
import java.net.URI;

import org.xml.sax.ErrorHandler;

import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.DefaultXmlParserFactory;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.bpel.xml.uri.BaseURIResolver;
import com.sun.bpel.xml.uri.CatalogResolver;
import com.sun.bpel.xml.uri.FileURIResolverFactory;
import com.sun.bpel.xml.uri.impl.CatalogResolverImpl;
import com.sun.bpel.xml.wsdl.WSDLParseContext;
import com.sun.wsdl4j.ext.DeferredActionRegistry;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class BPELParseContext {

    private final static String META_INF = "META-INF";
    private final static String CATALOG_FILE = "catalog.xml";
    private ErrorHandler mErrorHandler = null;
    private WSDLParseContext mWsdlContext = null;
    private boolean mEnableEvents = false;
    private DefaultXmlParserFactory mXmlParserFac = null;
    private BPELDocumentFactory mBPELDocumentFactory = null;
    private IWSDLResolver mWSDLResolver = null;
    private IXSDResolver mXSDResolver = null;
    private boolean mLoadOnlyPartnersAndImports = false;
    private boolean mLoadImportedWsdls = true;
    private boolean mLoadImportedXsds = true;
    private BaseURIResolver mURIResolver;
    private boolean mValidateSchema = true;
    protected CatalogResolver mCatalogResolver;
    protected boolean mHasCatalog = false;
    protected DeferredActionRegistry mDeferredActionRegistry;
    protected ParsingCaches mParsingCaches;
    protected String mBaseUri;


    private void setCatalog (File catalogFile,  String baseLocation, URI catalogBaseLocation) throws Exception {
        mHasCatalog = catalogFile != null && catalogFile.exists();
        mCatalogResolver = new CatalogResolverImpl(catalogFile, catalogBaseLocation);
        mBaseUri = baseLocation;
        mURIResolver = FileURIResolverFactory.getInstance().getURIResolver(mCatalogResolver, baseLocation);
    }

    public void setCatalog (File deploydir, File bpelFile) throws Exception  {
        String baseLocation = bpelFile.toURI().toString();

        String catalogFilePath =  deploydir + File.separator + META_INF + File.separator + CATALOG_FILE;
        File catalogFile =  new File (catalogFilePath);

        if (!catalogFile.exists()) {
            setCatalog(null, baseLocation, null);
        } else {
            URI catalogBaseLocation = catalogFile.getParentFile().toURI();
            setCatalog(catalogFile, baseLocation, catalogBaseLocation);
        }
    }
    
    /**
     * @deprecated Please use setCatalog (File catalogFile,File bpelFile), as the catalog.xml will be under deploy directory if the BPEL is in sub directory
     * 
     **/
    public void setCatalog (File bpelFile) throws Exception  {
    	//using bpelFile.getParentFile() as the deploy directory. 
    	setCatalog (bpelFile.getParentFile(), bpelFile);
    }

	public ErrorHandler getErrorHandler() {
        return mErrorHandler;
    }

	public void setErrorHandler(ErrorHandler errorHandler) {
        mErrorHandler = errorHandler;
    }

    public WSDLParseContext getWSDLParseContext() {
        return mWsdlContext;
    }

	public void setWSDLParseContext(WSDLParseContext context) {
        mWsdlContext = context;
    }

	/**
	 * Enable event firing from XMLELement/XMLNode
	 * @param enable
	 */
	public void setEnableEvents(boolean enable) {
        mEnableEvents = enable;
    }

	public boolean isEnableEvents() {
        return mEnableEvents;
    }

    public void setXMLParserFactory(DefaultXmlParserFactory xmlParserFac) {
        mXmlParserFac = xmlParserFac;
    }

    public DefaultXmlParserFactory getXMLParserFactory() {
        return mXmlParserFac;
    }

	public BPELDocumentFactory getBPELDocumentFactory() {
		return mBPELDocumentFactory;
	}

	public void setBPELDocumentFactory(BPELDocumentFactory documentFactory) {
		mBPELDocumentFactory = documentFactory;
	}

	public void setWSDLResolver(IWSDLResolver wsdlResolver) {
		this.mWSDLResolver = wsdlResolver;
	}

	public IWSDLResolver getWSDLResolver() {
		return this.mWSDLResolver;
	}

	public void setXSDResolver(IXSDResolver xsdResolver) {
		this.mXSDResolver = xsdResolver;
	}

	public IXSDResolver getXSDResolver() {
		return this.mXSDResolver;
	}

	public  BaseURIResolver getBaseURIResolver() {
	    if (mURIResolver == null) {
            mURIResolver = FileURIResolverFactory.getInstance().getURIResolver(mCatalogResolver, mBaseUri);
        }
        return mURIResolver;
	}

	public void setValidateSchema(boolean validate) {
		this.mValidateSchema = validate;
	}

	public boolean isValidateSchema() {
		return this.mValidateSchema;
	}

	public void setLoadOnlyPartnersAndImports(boolean load) {
    	this.mLoadOnlyPartnersAndImports = load;
    }

    public boolean isLoadOnlyPartnersAndImports() {
    	return this.mLoadOnlyPartnersAndImports;
    }

    public void setLoadImportedWsdls(boolean loadWsdls) {
    	this.mLoadImportedWsdls = loadWsdls;
    }

    public boolean isLoadImportedWsdls() {
    	return this.mLoadImportedWsdls;
    }

    public void setLoadImportedXsds(boolean loadXsds) {
    	this.mLoadImportedXsds = loadXsds;
    }

    public boolean isLoadImportedXsds() {
    	return this.mLoadImportedXsds;
    }

    public CatalogResolver getCatalogResolver() {
        return this.mCatalogResolver;
    }

    public boolean hasCatalog() {
        return mHasCatalog;
    }

    /**
     * Sets the deferred action registry so schema documents and other required
     * actions can be collected during loading business processes and WSDLs,
     * and a single schema type system can then be generated later and used by
     * all business processes and WSDLs.
     *
     * @param registry The deferred action registry
     */
    public void setDeferredActionRegistry(DeferredActionRegistry registry) {
        this.mDeferredActionRegistry = registry;
    }

    /**
     * Gets the deferred action registry
     *
     * @see BPELParseContext#setDeferredActionRegistry(DeferredActionRegistry)
     * @return The deferred action registry.
     */
    public DeferredActionRegistry getDeferredActionRegistry() {
        return this.mDeferredActionRegistry;
    }

    /**
     * Sets parsing caches used at BPEL model loading phase.  The caches are
     * used to prevent WSDLs and XSDs from loading multiple times.
     *
     * @param caches The parsing caches used at BPEL model loading phase
     */
    public void setCaches(ParsingCaches caches) {
        this.mParsingCaches = caches;
    }

    /**
     * Gets parsing caches used at BPEL model loading phase.  The caches are
     * used to prevent WSDLs and XSDs from loading multiple times.
     *
     * @return The parsing caches used at BPEL model loading phase.
     */
    public ParsingCaches getCaches() {
        return this.mParsingCaches;
    }

	public static class DefaultParseContext extends BPELParseContext {

        public WSDLParseContext getWSDLParseContext() {
            if (super.getWSDLParseContext() == null) {

               setWSDLParseContext(new WSDLParseContext.DefaultParseContext());
            }
            return super.getWSDLParseContext();
		}

		public DefaultXmlParserFactory getXMLParserFactory() {
			if (super.getXMLParserFactory() == null) {
                setXMLParserFactory(new DefaultXmlParserFactory());
            }

            return super.getXMLParserFactory();
	    }

		public  boolean isEnableEvents() {
			return true;
		}

		public BPELDocumentFactory getBPELDocumentFactory() {
			if (super.getBPELDocumentFactory() == null) {
				setBPELDocumentFactory(BPELDocumentFactory.getInstance());
            }
			return super.getBPELDocumentFactory();
		}
	}
}
