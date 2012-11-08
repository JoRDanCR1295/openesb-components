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
 * @(#)NamespaceResolver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri;


import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * The reference implementation of JXPathContext.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class NamespaceResolver implements Cloneable {
    
    protected HashMap namespaceMap = new HashMap();
    protected HashMap reverseMap;
    protected NodePointer pointer;
    private boolean sealed;
    
    /**
     * Registers a namespace prefix.
     * 
     * @param prefix A namespace prefix
     * @param namespaceURI A URI for that prefix
     */
    public void registerNamespace(String prefix, String namespaceURI) {
        namespaceMap.put(prefix, namespaceURI);
        reverseMap = null;
    }
    
    /**
     * Register a namespace for the expression context.
     */
    public void setNamespaceContextPointer(NodePointer pointer) {
        this.pointer = pointer;
    }
    
    public Pointer getNamespaceContextPointer() {
        return pointer;
    }
    
    /**
     * Given a prefix, returns a registered namespace URI. If the requested
     * prefix was not defined explicitly using the registerNamespace method,
     * JXPathContext will then check the context node to see if the prefix is
     * defined there. See
     * {@link #setNamespaceContextPointer(Pointer) setNamespaceContextPointer}.
     * 
     * @param prefix The namespace prefix to look up
     * @return namespace URI or null if the prefix is undefined.
     */
    public String getNamespaceURI(String prefix) {
        String uri = (String) namespaceMap.get(prefix);
        if (uri == null && pointer != null) {
            uri = pointer.getNamespaceURI(prefix);
        }
//        System.err.println("For prefix " + prefix + " URI=" + uri);
        return uri;
    }
    
    public String getPrefix(String namespaceURI) {
        if (reverseMap == null) {
            reverseMap = new HashMap();
            NodeIterator ni = pointer.namespaceIterator();
            if (ni != null) {
                for (int position = 1; ni.setPosition(position); position++) {
                    NodePointer nsPointer = ni.getNodePointer();
                    QName qname = nsPointer.getName();
                    reverseMap.put(qname.getPrefix(), qname.getName());
                }
            }
            Iterator it = namespaceMap.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry entry = (Map.Entry) it.next();
                reverseMap.put(entry.getValue(), entry.getKey());
            }
        }
        String prefix = (String) reverseMap.get(namespaceURI);
        return prefix;
    }
    
    public boolean isSealed() {
        return sealed;
    }
    
    public void seal() {
        sealed = true;
    }
    
    public Object clone() {
        try {
            return super.clone();
        }
        catch (CloneNotSupportedException e) {
            // Of course, it's supported.
            e.printStackTrace();
            return null;
        }
    }
}
