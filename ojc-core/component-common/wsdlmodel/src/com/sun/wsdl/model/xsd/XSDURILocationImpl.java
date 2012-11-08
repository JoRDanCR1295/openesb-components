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

package com.sun.wsdl.model.xsd;

import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.xsd.impl.CastorSupportImpl;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import org.exolab.castor.net.URILocation;



/**
 * URILocation implementation for XML Schemas within WSDL &lt;types&gt; elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XSDURILocationImpl extends URILocation {
    
    /** The schema element. */
    ExtensibilityElement mElement;
    
    /** The schema element urn. */
    String mURN;
    
    /** The schema XML CLOB. */
    String mClob;
    
    /** The schema reader. */
    Reader mRdr;
    
    /** Default constructor. */
    public XSDURILocationImpl() {
        super();
        mElement = null;
        mURN = null;
        mClob = null;
        mRdr = null;
    }
    
    /** Constructor.
     * @param urn URN
     * @param elem schema element
     */
    public XSDURILocationImpl(String urn, ExtensibilityElement elem) {
        this();
        mURN = urn;
        mElement = elem;
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
        }
        return rdr;
    }
    
    /** @see org.exolab.castor.net.URILocation */
    public String getRelativeURI() {
        return mURN;
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
