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

package com.sun.wsdl.model.uri.impl;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.xsd.XSDURIResolverImpl;

import java.beans.PropertyChangeEvent;
import java.util.Map;
import java.util.Stack;
import java.util.logging.Logger;

/**
 * Implements a SeeBeyond base URI Resolver.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BaseURIResolverImpl extends XSDURIResolverImpl implements BaseURIResolver {
    
    private static final Messages MESSAGES =
            Messages.getMessages(BaseURIResolverImpl.class);
    private static final Logger LOGGER = 
            Messages.getLogger(BaseURIResolverImpl.class);
    
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
                    MESSAGES.getString("BaseURIResolverImpl.EXPECTED_x_BUT_GOT_x", 
                    new Object[] {expectedClass , obj.getClass()} ));
        }
    }
}
