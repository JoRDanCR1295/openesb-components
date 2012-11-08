/*
 * @(#)NamespacePrefixGenerator.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.schema;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;
import net.jcip.annotations.Immutable;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
@Immutable
public class NamespacePrefixGenerator {
    
    private static final String NAMESPACE_PREFIX_WITHOUT_NUMBER = "ns";
    
    private final Set<QName> elementNames;
    
    /**
     * Maps a namespace to its prefix.
     */
    private final Map<String, String> prefixes;
    
    /**
     * Creates a <code>NamespacePrefixGenerator</code> for the specified elements.
     * 
     * @param elements set of elements.
     */
    public NamespacePrefixGenerator(Set<QName> elementNames) {
        if (elementNames == null) {
            throw new NullPointerException("Elements must not be null");
        }

        if (elementNames.isEmpty()) {
            throw new IllegalArgumentException("Elements must be non-empty");
        }

        /* Defensive copy. Note that QName is immutable. */
        this.elementNames = new LinkedHashSet<QName>();
        this.elementNames.addAll(elementNames);

        prefixes = new HashMap<String, String>();
        generatePrefixes();
    }

    private void generatePrefixes() {
        assert prefixes != null;
        
        int nextNamespaceNumber = 1;
        for (QName qName : elementNames) {
            final String namespace = qName.getNamespaceURI();
            
            if (!prefixes.containsKey(namespace)) {
                prefixes.put(namespace, NAMESPACE_PREFIX_WITHOUT_NUMBER + nextNamespaceNumber);
                nextNamespaceNumber++;
            }
        }
    }

    public int registeredNamespaces() {
        return prefixes.size();
    }
    
    public String getPrefixedLocalName(QName elementName) {
        if (!elementNames.contains(elementName)) {
            throw new IllegalArgumentException(
                    "This element was not registered with this prefix generator: " + elementName);
        }
        
        final String prefix = prefixes.get(elementName.getNamespaceURI());
        
        return prefix + ":" + elementName.getLocalPart();
    }

    /**
     * Returns a list of all registered namespaces.
     * 
     * @return list of all registered namespaces.
     */
    public List<String> getNamespaces() {
        List<String> result = new ArrayList<String>();

        for (String namespace : prefixes.keySet()) {
            result.add(namespace);
        }

        return Collections.unmodifiableList(result);
    }
    
    /**
     * Returns prefixes and their corresponding namespaces.
     * 
     * @return pair {prefix, namespace} for all registered namespaces.
     */
    public Map<String, String> getPrefixes() {
        Map<String, String> result = new HashMap<String, String>();
        
        for (String namespace : prefixes.keySet()) {
            result.put(prefixes.get(namespace), namespace);
        }

        return Collections.unmodifiableMap(result);
    }
}
