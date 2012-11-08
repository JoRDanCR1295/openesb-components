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
 * @(#)XSDURIResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import org.exolab.castor.net.URIException;
import org.exolab.castor.net.URILocation;
import org.exolab.castor.net.URIResolver;
import org.exolab.castor.net.util.URIResolverImpl;



/**
 * Resolves a urn representing an XML Schema. All other resolutions are delegated
 * to the default resolver. The XML Schema must be located within the same &lt;types&gt;
 * element in a WSDL document.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XSDURIResolverImpl implements URIResolver {
    
    /** Castor's default implementation. */
    URIResolver mDefaultResolver;
    
    /** Map of other schemas to be used for resolution. */
    Map mMap;
    
    /** The logger. */
    private static final Messages MESSAGES = 
            Messages.getMessages(XSDURIResolverImpl.class);
    private static final Logger LOGGER = 
            Messages.getLogger(XSDURIResolverImpl.class);
    
    /** Default constructor. */
    public XSDURIResolverImpl() {
        mDefaultResolver = new URIResolverImpl();
        mMap = null;
    }
    
    /**
     * Constructor.
     * @param map map of other elements/schemas that may be used to resolve urns; the
     * key of each map entry represents the target namespace for the schema element
     * which is the value for the map entry
     */
    public XSDURIResolverImpl(Map map) {
        this();
        mMap = map;
    }
    
    /** @see org.exolab.castor.net.URIResolver */
    public URILocation resolve(String href, String documentBase) throws URIException {
        return mDefaultResolver.resolve(href, documentBase);
    }
    
    /** @see org.exolab.castor.net.URIResolver */
    public URILocation resolveURN(String urn) throws URIException {
        URILocation uriLocation = null;
        
        if (mMap != null) {
            LOGGER.log(Level.FINE, 
                    MESSAGES.getString("XSDURIResolverImpl.RESOLVING",urn));
            ExtensibilityElement elem = (ExtensibilityElement) mMap.get(urn);
            if (elem != null) {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("XSDURIResolverImpl.FOUND", elem));
                uriLocation = new XSDURILocationImpl(urn, elem);
            }else{
                LOGGER.log(Level.FINE,
                        MESSAGES.getString("XSDURIResolverImpl.FOUND_NULL_EXTNSBILITY_ELEMENT"));  
            }
        }       
        return uriLocation;
    }
    
    /** @see org.exolab.castor.net.URIResolver */
    public void setDefaultFileLocation(String defaultFileLocation) {
        mDefaultResolver.setDefaultFileLocation(defaultFileLocation);
    }
    
   
    
    /** Sets the map of other schemas to be used for resolution.
     * @param map   Map of other schemas to be used for resolution.
     */
    public void setMap(Map map) {
        mMap = map;
    }
}
