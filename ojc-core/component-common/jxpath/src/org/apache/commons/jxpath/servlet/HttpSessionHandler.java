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
 * @(#)HttpSessionHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.Enumeration;
import java.util.HashSet;

import javax.servlet.http.HttpSession;

import org.apache.commons.jxpath.JXPathException;

/**
 * Implementation of the DynamicPropertyHandler interface that provides
 * access to attributes of a HttpSession.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class HttpSessionHandler extends ServletContextHandler {

    protected void collectPropertyNames(HashSet set, Object bean) {
        HttpSessionAndServletContext handle = 
            (HttpSessionAndServletContext) bean;
        super.collectPropertyNames(set, handle.getServletContext());
        HttpSession session = handle.getSession();
        if (session != null) {
            Enumeration e = session.getAttributeNames();
            while (e.hasMoreElements()) {
                set.add(e.nextElement());
            }
        }
    }
    
    public Object getProperty(Object bean, String property) {
        HttpSessionAndServletContext handle = 
            (HttpSessionAndServletContext) bean;
        HttpSession session = handle.getSession();
        if (session != null) {
            Object object = session.getAttribute(property);
            if (object != null) {
                return object;
            }
        }
        return super.getProperty(handle.getServletContext(), property);
    }

    public void setProperty(Object bean, String property, Object value) {
        HttpSessionAndServletContext handle = 
            (HttpSessionAndServletContext) bean;
        HttpSession session = handle.getSession();
        if (session != null) {
            session.setAttribute(property, value);
        }
        else {
            throw new JXPathException("Cannot set session attribute: "
                    + "there is no session");
        }
    }
}
