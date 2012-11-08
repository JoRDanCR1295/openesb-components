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
 * @(#)BaseURIResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.uri.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.Map;
import java.util.Stack;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.util.I18n;
import com.sun.bpel.xml.uri.BaseURIResolver;
import com.sun.bpel.xml.xsd.XSDURIResolverImpl;

/**
 * Implements a SeeBeyond base URI Resolver.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BaseURIResolverImpl extends XSDURIResolverImpl implements BaseURIResolver {
    
    /** Holds the parent project element stack. */
    private Stack parentProjElemStack = new Stack();
    
    /** Holds the parameters for the XML Registry Factory. */
    private Object[] xmlRegFacParams = null;
    
    /** EInsight Manager creator */

    
    /** Creates a new instance of BaseURIResolverImpl */
    public BaseURIResolverImpl() {
    }
    
    /** Creates a new instance of BaseURIResolverImpl.
     * @param   map     A map of existing inlined schemas.
     */
    public BaseURIResolverImpl(Map map) {
        super(map);
    }
    
    /** @see EInsightManagerCreatable#initializeEInsightManager
     */
    
    
    /** @see PropertyChangeListener#propertyChange
     */
    public void propertyChange(PropertyChangeEvent evt) {
    }
    
    /** Asserts that an object is of the right type.
     * @param   obj     Object to be tested.  Test is bypassed if <code>null</code>.
     * @param   expectedClass   Expected class.
     * @throws  EInsightModelException  When test fails.
     */
    protected void assertType(Object obj, Class expectedClass) throws EInsightModelException {
        if ((obj != null) && !expectedClass.isInstance(obj)) {
            throw new EInsightModelException(
            		I18n.loc("BPMOD-6000: Expected class {0}, but got class {1}", expectedClass, obj.getClass()));
        }
    }

    public InputSource resolveEntity(String publicId, String systemId)
            throws SAXException, IOException {
        //Let derived class implement this because it is meant for
        //CatalogResolver
        return null;
    }
}
