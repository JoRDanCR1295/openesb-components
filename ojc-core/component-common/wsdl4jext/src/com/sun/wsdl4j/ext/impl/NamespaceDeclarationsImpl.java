package com.sun.wsdl4j.ext.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import com.sun.wsdl4j.ext.NamespaceDeclarations;

public class NamespaceDeclarationsImpl implements NamespaceDeclarations {
    
    private Map<String, String> _prefixNsMap = new HashMap<String, String>();
    
    private NamespaceDeclarationsImpl() {
        //Suppress default constructor from public use
    }
    
    /**
     * Creates a namespace declarations instance that loads all namespace
     * declarations from a DOM element and its ancestors.
     *  
     * @param domElem The element to load namespace declarations from
     * @return A <code>NamespaceDeclarations</code> instance
     */
    public static NamespaceDeclarations newInstance(Element domElem) {
        NamespaceDeclarationsImpl declarations =
            new NamespaceDeclarationsImpl();
        
        List<Element> ancestorList = new ArrayList<Element>();
        Element e = domElem;
        ancestorList.add(e);
        while (e.getParentNode() instanceof Element) {
            e = (Element) e.getParentNode();
            ancestorList.add(e);
        }
        Element[] ancestors = ancestorList.toArray(new Element[0]);
        for (int i = ancestors.length - 1; i >= 0; i--) {
            NamedNodeMap attrMap = ancestors[i].getAttributes();
            for(int j = 0; j < attrMap.getLength(); j++) {
                String nodeName = attrMap.item(j).getNodeName();
                String prefix = attrMap.item(j).getPrefix();
                String localName = attrMap.item(j).getLocalName();
                if (XMLNS_PREFIX.equals(prefix)) {
                    declarations._prefixNsMap.put(
                            localName, attrMap.item(j).getNodeValue());
                } else if (XMLNS_PREFIX.equals(nodeName)) {
                    declarations._prefixNsMap.put(
                            "", attrMap.item(j).getNodeValue());
                }
            }
        }
        return declarations;
    }

    public Map<String, String> getAll() {
        return Collections.unmodifiableMap(_prefixNsMap);
    }

    public String lookUpNamespacePrefix(String uri) {
        Set<Entry<String, String>> entrySet = _prefixNsMap.entrySet();
        for (Entry<String, String> entry : entrySet) {
            if (uri.equals(entry.getValue())) {
                return entry.getKey();
            }
        }
        return null;
    }

    public String lookUpNamespaceURI(String prefix) {
        return _prefixNsMap.get(prefix == null ? "" : prefix);
    }
    
    public boolean isEmpty() {
        return _prefixNsMap.isEmpty();
    }
}
