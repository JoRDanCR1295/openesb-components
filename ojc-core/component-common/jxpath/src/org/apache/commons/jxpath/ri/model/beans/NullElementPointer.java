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
 * @(#)NullElementPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * Used when there is a need to construct a Pointer for a collection element
 * that does not exist.  For example, if the path is "foo[3]", but the
 * collection "foo" only has one element or is empty or is null, the
 * NullElementPointer can be used to capture this situation without putting a
 * regular NodePointer into an invalid state.  Just create a NullElementPointer
 * with index 2 (= 3 - 1) and a "foo" pointer as the parent.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class NullElementPointer extends CollectionPointer {

    public NullElementPointer(NodePointer parent, int index) {
        super(parent, (Object) null);
        this.index = index;
    }

    public QName getName() {
        return null;
    }

    public Object getBaseValue() {
        return null;
    }

    public Object getImmediateNode() {
        return null;
    }
    
    public boolean isLeaf() {
        return true;
    }    
    
    public boolean isCollection() {
        return false;
    }

    public PropertyPointer getPropertyPointer() {
        return new NullPropertyPointer(this);
    }

    public NodePointer getValuePointer() {
        return new NullPointer(this, getName());
    }

    public void setValue(Object value) {
        throw new UnsupportedOperationException(
            "Collection element does not exist: " + this);
    }

    public boolean isActual() {
        return false;
    }

    public boolean isContainer() {
        return true;
    }

    public NodePointer createPath(JXPathContext context) {
        return parent.createChild(context, null, index);
    }
    
    public NodePointer createPath(JXPathContext context, Object value) {
        return parent.createChild(context, null, index, value);
    }

    public int hashCode() {
        return getImmediateParentPointer().hashCode() + index;
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof NullElementPointer)) {
            return false;
        }

        NullElementPointer other = (NullElementPointer) object;
        return getImmediateParentPointer() == other.getImmediateParentPointer() 
            && index == other.index;
    }

    public int getLength() {
        return 0;
    }
    
    public String asPath() {
        StringBuffer buffer = new StringBuffer();
        NodePointer parent = getImmediateParentPointer();
        if (parent != null) {
            buffer.append(parent.asPath());
        }
        if (index != WHOLE_COLLECTION) {
            // Address the list[1][2] case
            if (parent != null && parent.getIndex() != WHOLE_COLLECTION) {
                buffer.append("/.");
            }
            else if (parent != null
                    && parent.getImmediateParentPointer() != null
                    && parent.getImmediateParentPointer().getIndex() != 
                            WHOLE_COLLECTION) 
            {
                buffer.append("/.");
            }
            buffer.append("[").append(index + 1).append(']');
        }

        return buffer.toString();
    }    
}
