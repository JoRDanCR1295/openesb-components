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
 * @(#)ServletContextHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.Enumeration;
import java.util.HashSet;

import javax.servlet.ServletContext;

import org.apache.commons.jxpath.DynamicPropertyHandler;

/**
 * Implementation of the DynamicPropertyHandler interface that provides
 * access to attributes of a ServletContext.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ServletContextHandler implements DynamicPropertyHandler {
    
    private static final String[] STRING_ARRAY = new String[0];

    public String[] getPropertyNames(Object context) {
        HashSet list = new HashSet(16);
        collectPropertyNames(list, context);
        return (String[]) list.toArray(STRING_ARRAY);
    }
    
    protected void collectPropertyNames(HashSet set, Object bean) {
        Enumeration e = ((ServletContext) bean).getAttributeNames();
        while (e.hasMoreElements()) {
            set.add(e.nextElement());
        }
    }

    public Object getProperty(Object context, String property) {
        return ((ServletContext) context).getAttribute(property);
    }

    public void setProperty(Object context, String property, Object value) {
        ((ServletContext) context).setAttribute(property, value);
    }
}
