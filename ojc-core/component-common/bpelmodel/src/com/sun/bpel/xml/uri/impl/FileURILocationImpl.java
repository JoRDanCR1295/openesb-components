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

package com.sun.bpel.xml.uri.impl;

import com.sun.bpel.xml.uri.FileURILocation;
import com.sun.bpel.xml.uri.LazyReaderProvider;
import com.sun.bpel.xml.uri.URILocation;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;

/**
 * File-based URILocation implementation for XML documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FileURILocationImpl extends URILocation implements FileURILocation {
    
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
        mURN = null;
        mClob = null;
        mRdr = null;
        mLazyReaderProvider = null;
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
        if (mClob != null) {
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
}
