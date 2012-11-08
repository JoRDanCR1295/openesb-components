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
 * @(#)LangAttributePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * A Pointer that points to the "lang" attribute of a JavaBean. The value
 * of the attribute is based on the locale supplied to it in the constructor.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class LangAttributePointer extends NodePointer {
    public LangAttributePointer(NodePointer parent) {
        super(parent);
    }

    public QName getName() {
        return new QName("xml", "lang");
    }

    public String getNamespaceURI() {
        return null;
    }

    public boolean isCollection() {
        return false;
    }

    public int getLength() {
        return 1;
    }

    public Object getBaseValue() {
        return parent.getLocale().toString().replace('_', '-');
    }

    public Object getImmediateNode() {
        return getBaseValue();
    }

    public boolean isLeaf() {
        return true;
    }

    /**
     * Throws UnsupportedOperationException.
     */
    public void setValue(Object value) {
        throw new UnsupportedOperationException(
                "Cannot change locale using the 'lang' attribute");
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
        buffer.append("@xml:lang");
        return buffer.toString();
    }

    public int hashCode() {
        return 0;
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof LangAttributePointer)) {
            return false;
        }

        return true;
    }

    public boolean testNode(NodeTest test) {
        return false;
    }

    public int compareChildNodePointers(
        NodePointer pointer1,
        NodePointer pointer2)
    {
        // Won't happen - lang attributes don't have children
        return 0;
    }
}
