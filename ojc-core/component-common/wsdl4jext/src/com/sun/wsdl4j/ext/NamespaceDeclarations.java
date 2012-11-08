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
 * @(#)WSDL4JExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.util.Map;

/**
 * The instance of this class hold namespace declarations.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public interface NamespaceDeclarations {

    public static final String XMLNS_PREFIX = "xmlns";
    public static final String XMLNS_URI = "http://www.w3.org/2000/xmlns/";
    
    /**
     * Gets all namespace declarations held by this instance.
     * 
     * @return All namespace declarations held by this instance. The keys in
     *         the returned map are namespace prefixes, while the values are
     *         namespace URIs. The prefix of the default namespace is an
     *         empty string ("").
     */
    public Map<String, String> getAll();
    
    /**
     * Looks up a namespace URI using a namespace prefix.
     * 
     * @param prefix The string contains a namespace prefix. A <code>null</code>
     *               value or an empty string ("") indicates the intention of
     *               looking up the default namespace.
     * @return The namespace URI correspondent to the prefix.
     */
    public String lookUpNamespaceURI(String prefix);
    
    /**
     * Looks up a namespace prefix using a namespace URI. If multiple prefixes
     * are found for the namespace URI, which one will be returned is not
     * defined. But one thing to be sure is that one of them will be returned. 
     * 
     * @param uri The string contains a namespace URI.
     * @return The namespace prefix correspondent to the URI. For default
     *         namespace, if no other prefixes have been declared for that
     *         namespace URI, an empty string ("") will be returned.
     */
    public String lookUpNamespacePrefix(String uri);
    
    /**
     * Checks if this instance does not contain any namespace declarations.
     * 
     * @return <code>true</code> if this instance does not contain any
     *         namespace declarations, otherwise <code>false</code>.
     */
    public boolean isEmpty();
}
