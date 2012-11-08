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
 * @(#)CatalogResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.uri.impl;

import java.io.File;
import java.io.FileReader;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.bpel.model.util.I18n;
import com.sun.bpel.xml.uri.CatalogResolver;
import com.sun.jbi.util.XMLReaderManager;


public class CatalogResolverImpl implements CatalogResolver {
    /**
     * parses the catalogFile and load catalog entries
     * @param catalogFile The catalogFile
     * @param baseLocation The base location which will be prepended when an entry is found for the systemId.
     */
    private URI mBaseLocation;
    private Map mEntryMap = new HashMap();
    
    public CatalogResolverImpl(File catalogFile, URI baseLocation) throws Exception {
        mBaseLocation = baseLocation;
        if (catalogFile != null && catalogFile.exists()) {
            CatalogParserHandler catalogHandler = new CatalogParserHandler(catalogFile
                    .getAbsolutePath(), mEntryMap);
           //Use com.sun.jbi.util.XMLReaderManager to provide fall back class in creating XMLReader           
            XMLReader xr = null;
            try {
                xr = XMLReaderManager.getInstance().getXMLReader();
                xr.setFeature("http://xml.org/sax/features/namespaces", true);
                xr.setContentHandler(catalogHandler);
                xr.setErrorHandler(catalogHandler);
                xr.parse(new InputSource(new FileReader(catalogFile)));
            } catch (Exception e) {
                throw e;
            } finally {
                if (xr != null) {
                    XMLReaderManager.getInstance().releaseXMLReader(xr);
                }
            }
        }
    }
    
    public String resolve(String systemId) {
        if(mBaseLocation == null){
            return systemId;
        }
        String localUri = (String) mEntryMap.get(systemId);
        if (localUri == null) {
            return systemId;
        }
        return mBaseLocation.resolve(localUri).toString();
    }
    
    static class CatalogParserHandler extends DefaultHandler{
        
        /** Log handle */     
        private static final Logger LOGGER = Logger.getLogger(CatalogResolverImpl.class.getName());
        
        private Map mEntryMap;
        private Locator mLocator;
        private String mFileName;
        
        

        public CatalogParserHandler(String fileName, Map mappings) {
            super();
            mFileName = fileName;
            mEntryMap = mappings;
            
            // TODO Auto-generated constructor stub
        }

        /*
         * (non-Javadoc)
         * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
         */
        public void error(SAXParseException e) throws SAXException {
            // TODO Auto-generated method stub
            throw new SAXException(I18n.loc("BPMOD-7001: Parsing error encountered in file :{0}, message :{1}", 
            		mFileName, e.getMessage()));
        }

        /*
         * (non-Javadoc)
         * @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException)
         */
        public void fatalError(SAXParseException e) throws SAXException {
            // TODO Auto-generated method stub
        	throw new SAXException(I18n.loc("BPMOD-7001: Parsing error encountered in file :{0}, message :{1}", 
            		mFileName, e.getMessage()));
        }

        /*
         * (non-Javadoc)
         * @see org.xml.sax.helpers.DefaultHandler#warning(org.xml.sax.SAXParseException)
         */
        public void warning(SAXParseException e) throws SAXException {
            // TODO Auto-generated method stub
            LOGGER.log(Level.WARNING, I18n.loc("BPMOD-6002: Parsing warning in file :{0}, message :{1}", 
            		mFileName, e.getMessage()));
        }

        /*
         * (non-Javadoc)
         * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
         */
        public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
            // TODO Auto-generated method stub
            if (localName.equals("system")) {
                String systemId = null;
                String localuri = null;
                for (int i= 0; i < attributes.getLength(); i++) {
                    String attrLocalName = attributes.getLocalName(i);
                    if (attrLocalName.equals("systemId")) {
                        systemId = attributes.getValue(i);
                    }else if (attrLocalName.equals("uri")) {
                        localuri = attributes.getValue(i);
                    }
                }
                if (systemId == null || localuri == null) {
                   throw new SAXException(I18n.loc("BPMOD-6001: The entry in file :{0}, line :{1} is not valid", 
                		   	mFileName, String.valueOf(mLocator.getLineNumber())));
                }                
                mEntryMap.put(systemId, localuri);
            }
 
        }

        /*
         * (non-Javadoc)
         * @see org.xml.sax.helpers.DefaultHandler#setDocumentLocator(org.xml.sax.Locator)
         */
        public void setDocumentLocator(Locator locator) {
            // TODO Auto-generated method stub
            super.setDocumentLocator(locator);
            
        }
        
    }

}
