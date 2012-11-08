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
 * @(#)ParsingCaches.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.wsdl.Definition;
import javax.wsdl.extensions.schema.Schema;

import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;

/**
 * Parsing caches used at BPEL model loading phase.  The caches are used to
 * prevent WSDLs and XSDs from loading multiple times. 
 * 
 * @author Jun Xu
 */
public class ParsingCaches {

    Map<String, Definition> mWsdlCacheForWSDL =
        new HashMap<String, Definition>();
    
    private Map<String, WSDLDocument> mWsdlCacheForBPEL =
        new HashMap<String, WSDLDocument>();
    
    private Map<String, Schema> mXsdCacheForWSDL =
        new HashMap<String, Schema>();
    
    private Map<String, XMLSchema> mXsdCacheForBPEL =
        new HashMap<String, XMLSchema>();
    
    private Set<String> mXslCache = new HashSet<String>();
    private String mCurrentBaseURI;
    
    /**
     * Gets the cache will be passed to <code>WSDLReader</code> and be
     * used to avoid loading same WSDL definition instance multiple times.
     * 
     * @return The cache used to store the WSDL definitions that are already
     *          loaded
     */
    public Map<String, Definition> getWSDLCacheForWSDL() {
        return mWsdlCacheForWSDL;
    }
    
    /**
     * Gets the cache that is used by BPEL model to avoid loading same
     * <code>WSDLDocument</code> instance multiple times.
     * 
     * @return The cache used to store the <code>WSDLDocument</code>
     *                  instances that are already loaded
     */
    public Map<String, WSDLDocument> getWSDLCacheForBPEL() {
        return mWsdlCacheForBPEL;
    }
    
    /**
     * Gets the cache that is used by BPEL model to avoid loading same XSD
     * multiple times.
     * 
     * @return The cache that is used to store the XSDs that are already
     *                  loaded.
     */
    public Map<String, XMLSchema> getXSDCacheForBPEL() {
        return mXsdCacheForBPEL;
    }    
    
    /**
     * Gets the cache that is used by WSDL model to avoid loading same XSD
     * multiple times.
     * 
     * @return The cache that is used to store the XSDs that are already
     *                  loaded.
     */
    public Map<String, Schema> getXSDCacheForWSDL() {
        return mXsdCacheForWSDL;
    }
    
    public Set<String> getXslCache() {
        return mXslCache;
    }

    /** 
     * Returns the current document's base URI.
     * @return the current document's base URI.
     */
    public String getCurrentBaseURI() {
        return mCurrentBaseURI;
    }

    /**
     * Sets the current document's base URI.
     * @param currentBaseURI The current document's base URI.
     */
    public void setCurrentBaseURI(String currentBaseURI) {
        mCurrentBaseURI = currentBaseURI;
    }
    
}
