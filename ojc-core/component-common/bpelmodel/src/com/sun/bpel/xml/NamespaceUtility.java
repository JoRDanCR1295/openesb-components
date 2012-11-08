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
 * @(#)NamespaceUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml;

import java.util.Iterator;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.bpel.xml.common.model.XMLElement;

/**
 *
 * @author Sun Microsystems
 *
 */
public class NamespaceUtility {
    
    private static final String NAMESPACE_PREFIX_START  = "ns";
    
    /**
     * Generate a prefix which is not available in prefix to namespace map.
     * If a prefix already exist for given namespace, it will not be considered.
     * This method always generate a new prefix.
     * @param namespace namespace for which a prefix needs to be generated.
     * @param prefixToNamespaceMap map of prefix as the key and namespace as the value.
     * @return a new generated prefix for given namespace.
     */
    public static String generatePrefix(String namespace, Map prefixToNamespaceMap) {
        String prefixToGenerate = null;
        
        String tempPrefix = NAMESPACE_PREFIX_START;
        int counter = 0;
        if(prefixToNamespaceMap != null) {
            while(prefixToGenerate == null) {
                String prefix = (String) prefixToNamespaceMap.get(tempPrefix);
                if(prefix == null) {
                    prefixToGenerate = tempPrefix;
                } else {
                    tempPrefix = NAMESPACE_PREFIX_START + counter++;
                }
            }
        }
        
        return prefixToGenerate;
    }
    
    
    public static boolean isPrefixExists(String prefix, Map prefixToNamespaceMap) {
        boolean prefixExists = false;
        if(prefixToNamespaceMap != null) {
            String ns = (String) prefixToNamespaceMap.get(prefix);
            if(ns != null) {
                prefixExists = true;
            }
        }
        
        return prefixExists;
    }
    
    public static QName getQName(String namespaceURI, String localName, String prefix) {
        return new QName(namespaceURI, localName, prefix);
    }
    
    public static QName getQName(String namespaceURI, String localName) {
        return new QName(namespaceURI, localName);
    }
    
    public static QName getQName(String localName) {
        return new QName(localName);
    }
    
    public static QName getQNameFromString(String nameString) {
        
        String namespaceURI = getNamespaceURI(nameString);
        String prefix = getPrefix(nameString);
        String localName = getLocalName(nameString);
        
        if(localName == null) {
            return null;
        }
        if (prefix == null) {
            if (namespaceURI == null) {
                return new QName(localName);
            } else {
                return new QName(namespaceURI, localName);
            }
        }
        return  new QName(namespaceURI, localName, prefix);
    }
    
    public static QName getQNameFromURIAndString(String uri, String str) {
        String prefix = getPrefix(str);
        String localName = getLocalName(str);
        if (prefix == null) {
            return new QName(uri, localName);
        }
        return new QName(uri, localName, prefix);
    }
    
    public static QName resolveAndGetQName(String qNameString, XMLElement element) {
        String namespaceURI = getNamespaceURI(qNameString);
        String prefix = getPrefix(qNameString);
        String localName = getLocalName(qNameString);
        if (localName == null) {
            return null;
        }
        if (namespaceURI == null) {
            if(element.getOwnerDocument() != null) {
            	// first get the DefaultNamespace of the element( mostly provided by the "xmln:" construct) 
              	namespaceURI = element.getOwnerDocument().getTargetNamespace();
            	// if null then get the targetNamespace of the owner.
            	if(namespaceURI == null) {
                  	namespaceURI = element.getDefaultNamespace();
                }
            }
        }
        if (prefix != null) {// if prefix is not null reslove it from the element using prefix
            namespaceURI = element.getNamespace(prefix);
        }
        if (prefix == null) {
            if (namespaceURI == null) {
                return new QName(localName);
            } else {
                return new QName(namespaceURI, localName);
            }
        }
        return  new QName(namespaceURI, localName, prefix);

    }
    
    public static QName getQNameFromURIAndQualifiedString(String namespaceURI, String qName) {
        String prefix = getPrefix(qName);
        String localName = getLocalName(qName);
        return new QName(namespaceURI, localName, prefix);
    }
    
    public static String getNamespaceURI(String qName) {
        if(qName == null || qName.trim().equals("")) {
            return null;
        }
        
        String namespace = null;
        int sIndex = qName.indexOf('{');
        int eIndex = qName.lastIndexOf('}');
        
        if(sIndex != -1
                && eIndex != -1) {
            namespace = qName.substring(sIndex+1, eIndex);
        }
        
        return namespace;
    }
    
    public static String getPrefix(String qName) {
        if(qName == null || qName.trim().equals("")) {
            return null;
        }
        
        int index = qName.indexOf('{');
        //if { then we have namespace
        if(index != -1) {
            return null;
        }
        
        index = qName.lastIndexOf(':');
        
        return ((index > 0) ? qName.substring(0, index) : null);
    }
    
    public static String getLocalName(String qName) {
        if(qName == null || qName.trim().equals("")) {
            return null;
        }
        
        //first check if qName is {namespace}localName
        int	index = qName.lastIndexOf('}');
        
        if(index == -1) {
            index = qName.lastIndexOf(':');
        }
        
        return ((index < 0) ? qName : qName.substring(index + 1));
    }
    
    /**
     * Returns the string representation of the QName. If the prefix is
     * available, then it will be [prefix]:[localName], for example,
     * tns:foo. If the prefix is not available and there is a namespace URI,
     * then it will be {[namespaceURI]}[localName], for example,
     * {http://schemas.xmlsoap.org/wsdl/}message. If neither the prefix not
     * the namespace URI are present, then it will just be the local name.
     * @return the QName's string representation
     */
    public static String getQNameAsString(QName qName) {
        String name = (null == qName.getLocalPart()) ? "" : qName.getLocalPart();
        
        if (qName.getNamespaceURI() != null) {
            return '{' + qName.getNamespaceURI() + '}' + name;
        } else if (qName.getPrefix() != null) {
            return qName.getPrefix() + ':' + name;
        } else {
            return name;
        }
    }
    
}
