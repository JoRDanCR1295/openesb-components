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
 * @(#)Constants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

/**
 * String constants for this package.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public final class Constants {

    /**
     * Variable name for ServletContext.
     */
    public static final String APPLICATION_SCOPE = "application";

    /**
     * Variable name for HttpSession.
     */
    public static final String SESSION_SCOPE = "session";

    /**
     * Variable name for ServletRequest.
     */
    public static final String REQUEST_SCOPE = "request";

    /**
     * Variable name for PageContext.
     */
    public static final String PAGE_SCOPE = "page";

    /**
     * Attribute  name used in page context, requst, session, and servlet
     * context to store the corresponding JXPathContext.
     */
    public static final String JXPATH_CONTEXT =
        "org.apache.commons.jxpath.JXPATH_CONTEXT";

}
