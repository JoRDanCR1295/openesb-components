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
 * @(#)XPathNamespaceContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.NamespaceContext;

import com.sun.jbi.common.descriptor.JbiDescriptor;

/**
 * Implements the {@link NamespaceContext} interface and includes the 
 * JBI descriptor namespace using the prefix &quot;jbi&quot;.
 * 
 * @author Kevan Simpson
 */
public class JbiNamespaceContext implements NamespaceContext {
	public static final String JBI_PREFIX = "jbi";
	
    private Map<String, String> mNsMap;
    
    /**
     * Constructs a {@link NamespaceContext} with the {@link #JBI_PREFIX}
     * mapped to the {@link JbiDescriptor#JBI_NS JBI namespace URI}.
     */
    public JbiNamespaceContext() {
        mNsMap = new HashMap<String, String>();
        addNamespace(JBI_PREFIX, JbiDescriptor.JBI_NS);
    }
    
    /**
     * Maps a namespace URI to the specified prefix.
     * @param prefix The specified prefix.
     * @param uri The specified namespace.
     */
    public void addNamespace(String prefix, String uri) {
        mNsMap.put(prefix, uri);
    }

    /** @see javax.xml.namespace.NamespaceContext#getNamespaceURI(java.lang.String) */
    public String getNamespaceURI(String prefix) {
        return mNsMap.get(prefix);
    }

    /** @see javax.xml.namespace.NamespaceContext#getPrefix(java.lang.String) */
    public String getPrefix(String namespaceURI) {
        if (namespaceURI != null) {
            for (String p : mNsMap.keySet()) {
                String ns = mNsMap.get(p);
                if (namespaceURI.equals(ns)) {
                    return p;
                }
            }
        }
        return null;
    }

    /** @see javax.xml.namespace.NamespaceContext#getPrefixes(java.lang.String) */
    public Iterator<String> getPrefixes(String namespaceURI) {
        List<String> prefixes = new ArrayList<String>();
        if (namespaceURI != null) {
            for (String p : mNsMap.keySet()) {
                String ns = mNsMap.get(p);
                if (namespaceURI.equals(ns)) {
                    prefixes.add(p);
                }
            }
        }
        return prefixes.iterator();    
    }
}
