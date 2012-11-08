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
 * @(#)JDOMNamespacePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.jdom;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * Represents a namespace node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMNamespacePointer extends NodePointer {
    private String prefix;
    private String namespaceURI;

    public JDOMNamespacePointer(NodePointer parent, String prefix) {
        super(parent);
        this.prefix = prefix;
    }

    public JDOMNamespacePointer(
            NodePointer parent,
            String prefix,
            String namespaceURI) 
    {
        super(parent);
        this.prefix = prefix;
        this.namespaceURI = namespaceURI;
    }

    public QName getName() {
        return new QName(prefix);
    }

    public Object getBaseValue() {
        return null;
    }
    
    public boolean isCollection() {
        return false;
    }
    
    public int getLength() {
        return 1;
    }    

    public Object getImmediateNode() {
        return getNamespaceURI();
    }

    public String getNamespaceURI() {
        if (namespaceURI == null) {
            namespaceURI = parent.getNamespaceURI(prefix);
        }
        return namespaceURI;
    }

    public boolean isLeaf() {
        return true;
    }

    /**
     * Throws UnsupportedOperationException.
     */
    public void setValue(Object value) {
        throw new UnsupportedOperationException("Cannot modify a namespace");
    }

    public String asPath() {
        StringBuffer buffer = new StringBuffer();
        if (parent != null) {
            buffer.append(parent.asPath());
            if (buffer.length() == 0
                || buffer.charAt(buffer.length() - 1) != '/') {
                buffer.append('/');
            }
        }
        buffer.append("namespace::");
        buffer.append(prefix);
        return buffer.toString();
    }

    public int hashCode() {
        return prefix.hashCode();
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof JDOMNamespacePointer)) {
            return false;
        }

        JDOMNamespacePointer other = (JDOMNamespacePointer) object;
        return prefix.equals(other.prefix);
    }

    public int compareChildNodePointers(
        NodePointer pointer1,
        NodePointer pointer2) 
    {
        // Won't happen - namespaces don't have children
        return 0;
    }
 }
