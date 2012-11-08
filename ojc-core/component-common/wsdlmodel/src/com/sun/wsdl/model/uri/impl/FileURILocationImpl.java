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
 * @(#)FileURILocationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri.impl;

import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.uri.FileURILocation;
import com.sun.wsdl.model.uri.LazyReaderProvider;
import com.sun.wsdl.model.xsd.CastorSupport;
import com.sun.wsdl.model.xsd.impl.CastorSupportImpl;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import org.exolab.castor.net.URILocation;
import org.exolab.castor.net.util.URIUtils;



/**
 * File-based URILocation implementation for XML documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FileURILocationImpl extends URILocation implements FileURILocation {
    
    /** The XML document element. */
    ExtensibilityElement mElement;
    
    /** The XML document element absolute urn. */
    String mURN;
    
    /** The XML document CLOB. */
    String mClob;
    
    /** The XML document reader. */
    Reader mRdr;
    
    /** The lazy reader provider.
     * @since   5.1.0
     */
    LazyReaderProvider mLazyReaderProvider;
    
    /** Default constructor. */
    public FileURILocationImpl() {
        super();
        mElement = null;
        mURN = null;
        mClob = null;
        mRdr = null;
        mLazyReaderProvider = null;
    }
    
    /** Constructor.
     * @param urn   Absolute URN
     * @param elem  schema element
     */
    public FileURILocationImpl(String urn, ExtensibilityElement elem) {
        this();
        mURN = urn;
        mElement = elem;
    }
    
    /** Constructor.
     * @param   urn     Absolute URN
     * @param   clob    XML CLOB
     */
    public FileURILocationImpl(String urn, String clob) {
        this();
        mURN = urn;
        mClob = clob;
    }
    
    /** Constructor.
     * @param   urn     Absolute URN
     * @param   rdr     Reader to URN.
     */
    public FileURILocationImpl(String urn, Reader rdr) {
        this();
        mURN = urn;
        mRdr = rdr;
    }
    
    /** Constructor.
     * @param   urn     Absolute URN.
     * @param   prov    Lazy reader provider.
     * @since   5.1.0
     */
    public FileURILocationImpl(String urn, LazyReaderProvider prov) {
        this();
        mURN = urn;
        mLazyReaderProvider = prov;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getAbsoluteURI() {
        return mURN;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getBaseURI() {
        return URIUtils.getDocumentBase(mURN);
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public Reader getReader() throws IOException {
        Reader rdr = null;
        if (mElement != null) {
            Writer writer = new StringWriter();
            getInstanceOfCastorSupport(getClass().getClassLoader())
                .serializeElement(mElement, writer);
            String schemaStr = writer.toString();
            rdr = new StringReader(schemaStr);
        } else if (mClob != null) {
            rdr = new StringReader(mClob);
        } else if (mRdr != null) {
            rdr = mRdr;
        } else if (mLazyReaderProvider != null) {
            rdr = mLazyReaderProvider.computeReader();
        }
        return rdr;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getRelativeURI() {
        return URIUtils.getRelativeURI(mURN);
    }
    
    /**
     * Convenience method to provide the <code>CastorSupport</code> object since eWay installation SARs may carry
     * their own old version of com.stc.einsightintegrationapi.jar which used to contain the CastorSupport abstract
     * class (it's now in com.stc.einsightintegrationprivapi.jar), we cannot access the fixed version of CastorSupport.
     *
     * @param   clsLdr      ClassLoader to use.
     * @return  A CastorSupport object.
     */
    protected CastorSupport getInstanceOfCastorSupport(ClassLoader clsLdr) {
        CastorSupport retCastorSupport = null;
        boolean oldAPIJar = true;
        try {
            // getComplexType() was added in 5.0.4
            Method getComplexType = CastorSupport.class.getDeclaredMethod("getComplexType", new Class[0]);
            oldAPIJar = (getComplexType == null);
        } catch (Exception e) {
            oldAPIJar = true;
        }
        
        if (oldAPIJar) {
            retCastorSupport = new CastorSupportImpl();
        } else {
            retCastorSupport = CastorSupport.getInstance(clsLdr);
        }
        
        return retCastorSupport;
    }
}
