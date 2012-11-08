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
 * @(#)XSDURILocationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.xsd;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;

import com.sun.bpel.xml.uri.URILocation;



/**
 * URILocation implementation for XML Schemas within WSDL &lt;types&gt; elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XSDURILocationImpl extends URILocation {
    
    /** The schema element urn. */
    String mURN;
    
    /** The schema XML CLOB. */
    String mClob;
    
    /** The schema reader. */
    Reader mRdr;
    
    /** Default constructor. */
    public XSDURILocationImpl() {
        super();
        mURN = null;
        mClob = null;
        mRdr = null;
    }
    
    /** Constructor.
     * @param urn URN
     * @param elem schema element
     */
    public XSDURILocationImpl(String urn) {
        this();
        mURN = urn;
    }
    
    /** Constructor.
     * @param   urn     URN
     * @param   clob    XML CLOB
     */
    public XSDURILocationImpl(String urn, String clob) {
        this();
        mURN = urn;
        mClob = clob;
    }
    
    /** Constructor.
     * @param   href    Hyperlink reference.
     * @param   rdr     Reader to URI.
     */
    public XSDURILocationImpl(String href, Reader rdr) {
        this();
        mURN = href;
        mRdr = rdr;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getAbsoluteURI() {
        return mURN;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getBaseURI() {
        return mURN;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public Reader getReader() throws IOException {
        Reader rdr = null;
        if (mClob != null) {
            rdr = new StringReader(mClob);
        } else if (mRdr != null) {
            rdr = mRdr;
        }
        return rdr;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getRelativeURI() {
        return mURN;
    }
}
