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
 * @(#)JDOMAttributePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.jdom;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.util.TypeUtils;
import org.jdom.Attribute;

/**
 * A Pointer that points to a DOM node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMAttributePointer extends NodePointer {
    private Attribute attr;

    public JDOMAttributePointer(NodePointer parent, Attribute attr) {
        super(parent);
        this.attr = attr;
    }

    public QName getName() {
        return new QName(
            JDOMNodePointer.getPrefix(attr),
            JDOMNodePointer.getLocalName(attr));
    }

    public String getNamespaceURI() {
        String uri = attr.getNamespaceURI();
        if (uri != null && uri.equals("")) {
            uri = null;
        }
        return uri;
    }

    public Object getValue() {
        return attr.getValue();
    }    
    
    public Object getBaseValue() {
        return attr;
    }
    
    public boolean isCollection() {
        return false;
    }

    public int getLength() {
        return 1;
    }    

    public Object getImmediateNode() {
        return attr;
    }

    public boolean isActual() {
        return true;
    }

    public boolean isLeaf() {
        return true;
    }

    /**
     * Sets the value of this attribute.
     */
    public void setValue(Object value) {
        attr.setValue((String) TypeUtils.convert(value, String.class));
    }

    public void remove() {
        attr.getParent().removeAttribute(attr);
    }

    /**
     */
    public String asPath() {
        StringBuffer buffer = new StringBuffer();
        if (parent != null) {
            buffer.append(parent.asPath());
            if (buffer.length() == 0
                || buffer.charAt(buffer.length() - 1) != '/') {
                buffer.append('/');
            }
        }
        buffer.append('@');
        buffer.append(getName());
        return buffer.toString();
    }

    public int hashCode() {
        return System.identityHashCode(attr);
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof JDOMAttributePointer)) {
            return false;
        }

        JDOMAttributePointer other = (JDOMAttributePointer) object;
        return attr == other.attr;
    }

    public int compareChildNodePointers(
            NodePointer pointer1,
            NodePointer pointer2) 
    {
        // Won't happen - attributes don't have children
        return 0;
    }
}
