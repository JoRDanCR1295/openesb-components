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
 * @(#)PageScopeContextHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.ArrayList;
import java.util.Enumeration;

import org.apache.commons.jxpath.DynamicPropertyHandler;

/**
 * Implementation of the DynamicPropertyHandler interface that provides
 * access to attributes of a PageScopeContext.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class PageScopeContextHandler implements DynamicPropertyHandler {
    
    private static final String[] STRING_ARRAY = new String[0];

    public String[] getPropertyNames(Object pageScope) {
        Enumeration e = ((PageScopeContext) pageScope).getAttributeNames();
        ArrayList list = new ArrayList(16);
        while (e.hasMoreElements()) {
            list.add(e.nextElement());
        }
        return (String[]) list.toArray(STRING_ARRAY);
    }

    public Object getProperty(Object pageScope, String property) {
        return ((PageScopeContext) pageScope).getAttribute(property);
    }

    public void setProperty(Object pageScope, String property, Object value) {
        ((PageScopeContext) pageScope).setAttribute(property, value);
    }
}
