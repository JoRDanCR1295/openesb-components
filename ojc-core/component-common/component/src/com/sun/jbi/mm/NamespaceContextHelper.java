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
 * @(#)NamespaceContextHelper.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mm;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;

/**
 * Helper class that implements the <a href="http://java.sun.com/javase/6/docs/api/javax/xml/namespace/NamespaceContext.html">
 * NamespaceContext</a> inteface.
 *
 * @author sunsoabi_edwong
 * @since 2.2
 */
public class NamespaceContextHelper implements NamespaceContext {

    private Map<String, String> map;
    private String defaultNS = XMLConstants.NULL_NS_URI;

    private NamespaceContextHelper() {
        super();
    }

    /**
     * Constructs a <code>NamespaceContextHelper</code> object.
     * @param map Map of prefix (key) bound to namespace URI (value)
     */
    public NamespaceContextHelper(Map<String, String> map) {
        this(map, XMLConstants.NULL_NS_URI);
    }

    /**
     * Constructs a <code>NamespaceContextHelper</code> object.
     * @param map Map of prefix (key) bound to namespace URI (value)
     * @param defaultNS Default namespace for the XML document
     * (<code>null</code> will map to <code>XMLConstants.NULL_NS_URI</code>)
     */
    public NamespaceContextHelper(Map<String, String> map,
            String defaultNS) {
        super();
        this.map = map;
        if (defaultNS != null) {
            this.defaultNS = defaultNS;
        }
    }

    /**
     * See <a href="http://java.sun.com/javase/6/docs/api/javax/xml/namespace/NamespaceContext.html#getNamespaceURI(java.lang.String)">
     * javax.xml.namespace.NamespaceContext#getNamespaceURI(java.lang.String)</a>
     */
    public String getNamespaceURI(String prefix) {
        if (null == prefix) {
            throw new IllegalArgumentException("prefix cannot be null");
        }
        String nsURI = XMLConstants.NULL_NS_URI;

        if (XMLConstants.DEFAULT_NS_PREFIX.equals(prefix)) {
            return defaultNS;
        } else if (map != null) {
            String uri = map.get(prefix);
            if (uri != null) {
                nsURI = uri;
            }
        }

        if (XMLConstants.XML_NS_PREFIX.equals(prefix)) {
            nsURI = XMLConstants.XML_NS_URI;
        } else if (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix)) {
            nsURI = XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
        }

        return nsURI;
    }

    /**
     * See <a href="http://java.sun.com/javase/6/docs/api/javax/xml/namespace/NamespaceContext.html#getPrefix(java.lang.String)">
     * javax.xml.namespace.NamespaceContext.html#getPrefix(java.lang.String)</a>
     */
    public String getPrefix(String namespaceURI) {
        if (null == namespaceURI) {
            throw new IllegalArgumentException("namespaceURI cannot be null");
        }
        String prefix = null;

        if (defaultNS.equals(namespaceURI)) {
            prefix = XMLConstants.DEFAULT_NS_PREFIX;
        } else if (map != null) {
            for (Map.Entry<String, String> me : map.entrySet()) {
                if (namespaceURI.equals(me.getValue())) {
                    prefix = me.getKey();
                    break;
                }
            }
        }

        if (XMLConstants.XML_NS_URI.equals(namespaceURI)) {
            prefix = XMLConstants.XML_NS_PREFIX;
        } else if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI)) {
            prefix = XMLConstants.XMLNS_ATTRIBUTE;
        }
        
        return prefix;
    }

    /**
     * See <a href="http://java.sun.com/javase/6/docs/api/javax/xml/namespace/NamespaceContext.html#getPrefixes(java.lang.String)">
     * javax.xml.namespace.NamespaceContext#getPrefixes(java.lang.String)</a>
     */
    public Iterator getPrefixes(String namespaceURI) {
        if (null == namespaceURI) {
            throw new IllegalArgumentException("namespaceURI cannot be null");
        }

        ArrayList<String> prefixes = new ArrayList<String>();

        if (XMLConstants.XML_NS_URI.equals(namespaceURI)) {
            prefixes.add(XMLConstants.XML_NS_PREFIX);
        } else if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI)) {
            prefixes.add(XMLConstants.XMLNS_ATTRIBUTE);
        } else {
            if (defaultNS.equals(namespaceURI)) {
                prefixes.add(XMLConstants.DEFAULT_NS_PREFIX);
            }
            if (map != null) {
                for (Map.Entry<String, String> me : map.entrySet()) {
                    if (me.getValue().equals(namespaceURI)) {
                        if (!prefixes.contains(me.getKey())) {
                            prefixes.add(me.getKey());
                        }
                    }
                }
            }
        }

        return prefixes.iterator();
    }

}
