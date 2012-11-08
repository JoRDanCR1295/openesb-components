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
 * @(#)DOMAttributePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dom;

import org.apache.commons.jxpath.ri.Compiler;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.compiler.NodeTypeTest;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.util.TypeUtils;
import org.w3c.dom.Attr;

/**
 * A Pointer that points to a DOM node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DOMAttributePointer extends NodePointer {
    private Attr attr;

    public DOMAttributePointer(NodePointer parent, Attr attr) {
        super(parent);
        this.attr = attr;
    }

    public QName getName() {
        return new QName(
            DOMNodePointer.getPrefix(attr),
            DOMNodePointer.getLocalName(attr));
    }

    public String getNamespaceURI() {
        String prefix = DOMNodePointer.getPrefix(attr);
        if (prefix == null) {
            return null;
        }
        return parent.getNamespaceURI(prefix);
    }

    public Object getValue() {
        String value = attr.getValue();
        if (value == null) {
            return null;
        }
        if (value.equals("") && !attr.getSpecified()) {
            return null;
        }
        return value;
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

    public boolean testNode(NodeTest nodeTest) {
        return nodeTest == null
            || ((nodeTest instanceof NodeTypeTest)
                && ((NodeTypeTest) nodeTest).getNodeType()
                    == Compiler.NODE_TYPE_NODE);
    }

    /**
     * Sets the value of this attribute.
     */
    public void setValue(Object value) {
        attr.setValue((String) TypeUtils.convert(value, String.class));
    }

    public void remove() {
        attr.getOwnerElement().removeAttributeNode(attr);
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

        if (!(object instanceof DOMAttributePointer)) {
            return false;
        }

        DOMAttributePointer other = (DOMAttributePointer) object;
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
