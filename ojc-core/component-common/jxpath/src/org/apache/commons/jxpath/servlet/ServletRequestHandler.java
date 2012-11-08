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
 * @(#)ServletRequestHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.Enumeration;
import java.util.HashSet;

import javax.servlet.ServletRequest;

/**
 * Implementation of the DynamicPropertyHandler interface that provides
 * access to attributes and parameters of a ServletRequest.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ServletRequestHandler extends HttpSessionHandler {
    
    protected void collectPropertyNames(HashSet set, Object bean) {
        super.collectPropertyNames(set, bean);
        ServletRequestAndContext handle = (ServletRequestAndContext) bean; 
        ServletRequest servletRequest = handle.getServletRequest();
        Enumeration e = servletRequest.getAttributeNames();
        while (e.hasMoreElements()) {
            set.add(e.nextElement());
        }
        e = servletRequest.getParameterNames();
        while (e.hasMoreElements()) {
            set.add(e.nextElement());
        }
    }
    
    public Object getProperty(Object bean, String property) { 
        ServletRequestAndContext handle = (ServletRequestAndContext) bean; 
        ServletRequest servletRequest = handle.getServletRequest();
        String[] strings = servletRequest.getParameterValues(property);
        if (strings != null) {
            if (strings.length == 0) {
                return null;
            }
            if (strings.length == 1) {
                return strings[0];
            }
            return strings;
        }
        
        Object object = servletRequest.getAttribute(property);
        if (object != null) {
            return object;
        }
        
        return super.getProperty(bean, property);
    }

    public void setProperty(Object request, String property, Object value) {
        ((ServletRequest) request).setAttribute(property, value);
    }
}
