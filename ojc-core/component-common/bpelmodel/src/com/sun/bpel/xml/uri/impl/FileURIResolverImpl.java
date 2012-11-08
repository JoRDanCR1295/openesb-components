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
 * @(#)FileURIResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.uri.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.bpel.model.util.I18n;
import com.sun.bpel.xml.EncodingUtil;
import com.sun.bpel.xml.uri.CatalogResolver;
import com.sun.bpel.xml.uri.FileURIResolver;
import com.sun.bpel.xml.uri.LazyReaderProvider;
import com.sun.bpel.xml.uri.URIException;
import com.sun.bpel.xml.uri.URILocation;

/**
 * Implements a File (such as local drive, HTTP) based URI Resolver.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FileURIResolverImpl extends BaseURIResolverImpl implements FileURIResolver {
        
        private static final String UTF_8 = "UTF-8";
    
    /** Key for THRW_IMPORT_INVALID_SCHEME message */
    private static final String THRW_IMPORT_INVALID_SCHEME = "THRW_IMPORT_INVALID_SCHEME";  // Not I18N
    
    /** Key for THRW_CANNOT_FIND_ABS_URI */
    private static final String THRW_CANNOT_FIND_ABS_URI = "THRW_CANNOT_FIND_ABS_URI";  // Not I18N
    
    /** Key for THRW_CANNOT_RESOLVE_URN */
    private static final String THRW_CANNOT_RESOLVE_URN = "THRW_CANNOT_RESOLVE_URN";  // Not I18N
    
    /** Key for THRW_INVALID_PROPERTY_CHANGE */
    private static final String THRW_INVALID_PROPERTY_CHANGE = "THRW_INVALID_PROPERTY_CHANGE";  // Not I18N
    
    /** Key for THRW_SPECIFY_PARENT_PROJECT_ELEM */
    private static final String THRW_SPECIFY_PARENT_PROJECT_ELEM = "THRW_SPECIFY_PARENT_PROJECT_ELEM";  // Not I18N
    
    /** Key for THRW_SELECT_XML_REGISTRY */
    private static final String THRW_SELECT_XML_REGISTRY = "THRW_SELECT_XML_REGISTRY";  // Not I18N
    
    /** Key for ASSUME_WELL_KNOWN_URI */
    private static final String ASSUME_WELL_KNOWN_URI = "ASSUME_WELL_KNOWN_URI";  // Not I18N
    
    /** Key for THRW_REPOSITORY_PROBLEMS */
    private static final String THRW_REPOSITORY_PROBLEMS = "THRW_REPOSITORY_PROBLEMS";  // Not I18N
    
    /** Key for THRW_CANNOT_SHALLOW_IMPORT
     * @since   5.1.0
     */
    private static final String THRW_CANNOT_SHALLOW_IMPORT = "THRW_CANNOT_SHALLOW_IMPORT";  // Not I18N
    
    
    /** The logger */
    private static final Logger LOGGER = Logger.getLogger(FileURIResolverImpl.class.getName());
    
    /** MessageManager for localized strings. */    
    //private MessageManager mMsg = MessageManager.getManager(getClass());
    
    private CatalogResolver mCatalogResolver;
    private String mDocumentBase;
    
    
    /** Creates a new instance of FileURIResolverImpl */
    public FileURIResolverImpl(CatalogResolver catalogResolver, String documentBase) {
        super();
        mCatalogResolver = catalogResolver;
        mDocumentBase = documentBase;
    }
    
    /** @see org.exolab.castor.net.URIResolver */
    public URILocation resolve(String href, String documentBase) throws URIException {
        URILocation uriLoc = null;
        String location = null;
        try {
            URI locationURI;
            String redirected;
            
            if (mCatalogResolver != null) {
                redirected = mCatalogResolver.resolve(href);
            }else {
                redirected = href;
            }
            

            if (documentBase != null) {
                locationURI = (new URI(documentBase)).resolve(redirected);
            } else {
                locationURI = new URI(redirected);
            }
            location = locationURI.toString();

            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPMOD-3000: Resolving location= {0}, Document base= {1}, " + 
                                "Effective location= {2}", redirected, documentBase, location));
            }

            // If doing shallow import for XML Schema, return Castor SchemaLocation so that
            // it won't follow import/include's anymore
            //boolean shallowXSDImport = false;
            
            //if (!shallowXSDImport && isContentAvailableAtURI(locationURI)) {
                
                uriLoc = new FileURILocationImpl(location, computeLazyReaderProvider(locationURI));
            //} 
        } catch (Exception e) {
                LOGGER.log(Level.SEVERE, I18n.loc("BPMOD-7002: Cannot resolve URN: {0}", location), e);
        }
        
        return uriLoc;
    }
    
    private boolean isContentAvailableAtURI(URI uri) {
        boolean result = true;
        try {
                InputStream is = null;
            String scheme = uri.getScheme();
            String location = uri.toString();
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPMOD-3001: Checking content of imported uri: {0}", uri.toString()));
            }
            
                if (("file".equalsIgnoreCase(scheme)) 
                        || ("http".equalsIgnoreCase(scheme))
                        || ("jar".equalsIgnoreCase(scheme))) {
                        URL url = uri.toURL();
                    is = url.openStream();
                }  else if ((new File(location)).exists()) {
                    is = new FileInputStream(location);
                } 
                
                if(is.available() == 0) {
                        result = false;
                }
                
                is.close();
        } catch(Exception ex) {
                //if exception occured then no content is available
                //and URI is invalid.
                result = false;
        }
        return result;
    }
    
    /** Calculates the reader associated with the URI location.
     * @param   locationURI     URI location of document to read.
     * @return  Reader for this URI location.
     * @throws  IOException     When there are I/O problems. 
     * @since   5.1.0
     */
    protected Reader calculateReader(URI locationURI) throws IOException {
        InputStream is = null;
        String scheme = locationURI.getScheme();
        String location = locationURI.toString();
        
        if (("file".equalsIgnoreCase(scheme)) 
                        || ("http".equalsIgnoreCase(scheme))
                        || ("jar".equalsIgnoreCase(scheme))) {
            URL url = locationURI.toURL();
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPMOD-3002: Importing file: {0}", url.toString()));
            }
            is = url.openStream();
            if ("http".equalsIgnoreCase(scheme)) {
                try {
                    is = convertToMemoryBased(is);
                } catch (IOException ioe) {
                    is = url.openStream();
                }
            }
        } else if ((new File(location)).exists()) {
                if (LOGGER.isLoggable(Level.FINE)) {
                        LOGGER.log(Level.FINE, I18n.loc("BPMOD-3002: Importing file: {0}", location));
                }
            is = new FileInputStream(location);
        } else {
            LOGGER.log(Level.WARNING, I18n.loc("BPMOD-6003: Invalid URI scheme: {0}", locationURI.getScheme()));
            
            throw new IOException(I18n.loc("BPMOD-6004: Unable to import document: Invalid URI scheme ({0}); " + 
                        "must be either http or file.  Given location: {1}", locationURI.getScheme(), location));
        }

        return new BufferedReader(EncodingUtil.getUnicodeReader(is));
//        return new BufferedReader(new InputStreamReader(is, UTF_8));
    }
    
    /** Computes the Lazy Reader Provider for the given URI.
     * @param   locationURI     URI location to compute for.
     * @return  A Lazy Reader Provider.
     * @since   5.1.0
     */
    protected LazyReaderProvider computeLazyReaderProvider(final URI locationURI) {
        return new LazyReaderProvider() {
            /** @see com.sun.bpel.xml.uri.LazyReaderProvider#computeReader()
             */
            public Reader computeReader() throws IOException {
                return calculateReader(locationURI);
            }
        };
    }
    
    
    
    /** Reads a URL input stream fully into a memory based input stream (in the
     * event the parsing is slow, the URL connection will time out).
     *
     * @param   inpStr  URL based input stream
     * @return  Memory based input stream.
     * @throws  IOException     When I/O problems occur.
     */
    private InputStream convertToMemoryBased(InputStream inpStr) throws IOException {
        // use a fixed buffer size.  Otherwise the num of available bytes may be 0 because
        // the server is slow in responding
        int num = Math.max(4096, inpStr.available());
        ByteArrayOutputStream baos = new ByteArrayOutputStream(num);
        byte[] bytes = new byte[num];
        int numRead;
        // make sure the read blocks until it's done or fails.
        while ((numRead = inpStr.read(bytes)) > 0) {
            baos.write(bytes, 0, numRead);
        }
        
        inpStr.close();
        return new ByteArrayInputStream(baos.toByteArray());
    }
    
    @Override
    public InputSource resolveEntity(String publicId, String systemId)
            throws SAXException, IOException {
        
        if (mCatalogResolver == null || systemId == null) {
            return null;
        }
        
        String redirected = mCatalogResolver.resolve(systemId);
            
        if (!redirected.equals(systemId)) {
            try {
                URILocation location = resolve(redirected, mDocumentBase);
                return new InputSource(location.getAbsoluteURI());
            } catch (URIException e) {
                new SAXException("Resolving entity failed. systemId = '"
                        + systemId + "', redirected = '"
                        + redirected + "'", e);
            }
        }
        return null;
    }

    /** @see org.exolab.castor.net.URIResolver */
    public URILocation resolveURN(String urn) throws URIException {
        if(urn.equals(WSDLURILocation.WSDL_NAMESPACE)) {
                return new WSDLURILocation();
        }
        String redirected;
        
        if (mCatalogResolver != null) {
            redirected = mCatalogResolver.resolve(urn);
            
        }else {
            redirected = urn;
        }
        if (!redirected.equals(urn)) {
            return resolve(redirected, mDocumentBase);
        }        
        
        URILocation uriLoc = super.resolveURN(urn);  // Delegate to inline types map to resolve if possible
        
        // Try interpretting the urn as a url
        if (null == uriLoc) {
            uriLoc = resolve(urn, null);
        }
        
        // Cannot find it so deem it a well-known namespace and return a well-known URILocation
        if (null == uriLoc) {
                if (LOGGER.isLoggable(Level.FINE)) {
                        LOGGER.log(Level.FINE, I18n.loc("BPMOD-3004: Cannot find URN: {0}.  " + 
                                        "Assuming it is a well-known URI and continuing...", urn));
                }
            uriLoc = new WellKnownURILocationImpl(urn);
        }
        return uriLoc;
    }
    
    /** @see PropertyChangeListener#propertyChange
     */
    public void propertyChange(PropertyChangeEvent evt) {
    }
}
