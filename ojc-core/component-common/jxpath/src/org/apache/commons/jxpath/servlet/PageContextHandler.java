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
 * @(#)PageContextHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.Enumeration;
import java.util.HashSet;

import javax.servlet.jsp.PageContext;

import org.apache.commons.jxpath.DynamicPropertyHandler;

/**
 * Implementation of the DynamicPropertyHandler interface that provides
 * access to attributes of a PageContext in all scopes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class PageContextHandler implements DynamicPropertyHandler {

    public String[] getPropertyNames(Object pageContext) {
        HashSet list = new HashSet();
        Enumeration e =
            ((PageContext) pageContext).getAttributeNamesInScope(
                PageContext.PAGE_SCOPE);
        while (e.hasMoreElements()) {
            list.add(e.nextElement());
        }
        e =
            ((PageContext) pageContext).getAttributeNamesInScope(
                PageContext.REQUEST_SCOPE);
        while (e.hasMoreElements()) {
            list.add(e.nextElement());
        }
        e =
            ((PageContext) pageContext).getAttributeNamesInScope(
                PageContext.SESSION_SCOPE);
        while (e.hasMoreElements()) {
            list.add(e.nextElement());
        }
        e =
            ((PageContext) pageContext).getAttributeNamesInScope(
                PageContext.APPLICATION_SCOPE);
        while (e.hasMoreElements()) {
            list.add(e.nextElement());
        }
        return (String[]) list.toArray(new String[0]);
    }

    /**
     * Returns <code>pageContext.findAttribute(property)</code>.
     */
    public Object getProperty(Object pageContext, String property) {
        return ((PageContext) pageContext).findAttribute(property);
    }

    public void setProperty(
        Object pageContext,
        String property,
        Object value) 
    {
        ((PageContext) pageContext).setAttribute(
            property,
            value,
            PageContext.PAGE_SCOPE);
    }
}
