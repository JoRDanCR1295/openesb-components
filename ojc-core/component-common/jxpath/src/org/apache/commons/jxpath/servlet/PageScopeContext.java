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
 * @(#)PageScopeContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import java.util.Enumeration;

import javax.servlet.jsp.PageContext;

/**
 * A lightweight wrapper for PageContext that restricts access
 * to attributes of the "page" scope.  This object is needed so that
 * XPath "foo" would lookup the attribute "foo" in all scopes, while
 * "$page/foo" would only look in the "page" scope.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class PageScopeContext {
    private PageContext pageContext;

    public PageScopeContext(PageContext pageContext) {
        this.pageContext = pageContext;
    }

    /**
     * Returns attributes of the pageContext declared in the "page" scope.
     */
    public Enumeration getAttributeNames() {
        return pageContext.getAttributeNamesInScope(PageContext.PAGE_SCOPE);
    }

    public Object getAttribute(String attribute) {
        return pageContext.getAttribute(attribute, PageContext.PAGE_SCOPE);
    }

    public void setAttribute(String attribute, Object value) {
        pageContext.setAttribute(attribute, value, PageContext.PAGE_SCOPE);
    }
}
