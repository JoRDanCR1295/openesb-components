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
 * @(#)NullPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import java.util.Locale;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * @author Dmitri Plotnikov
 * @version  
 */
public class NullPointer extends PropertyOwnerPointer {
    private QName name;
    private String id;

    public NullPointer(QName name, Locale locale) {
        super(null, locale);
        this.name = name;
    }

    /**
     * Used for the root node
     */
    public NullPointer(NodePointer parent, QName name) {
        super(parent);
        this.name = name;
    }

    public NullPointer(Locale locale, String id) {
        super(null, locale);
        this.id = id;
    }

    public QName getName() {
        return name;
    }

    public Object getBaseValue() {
        return null;
    }
    
    public boolean isCollection() {
        return false;
    }

    public boolean isLeaf() {
        return true;
    }        

    public boolean isActual() {
        return false;
    }

    public PropertyPointer getPropertyPointer() {
        return new NullPropertyPointer(this);
    }

    public NodePointer createPath(JXPathContext context, Object value) {
        if (parent != null) {
            return parent.createPath(context, value).getValuePointer();
        }
        else {
            throw new UnsupportedOperationException(
                "Cannot create the root object: " + asPath());
        }
    }

    public NodePointer createPath(JXPathContext context) {
        if (parent != null) {
            return parent.createPath(context).getValuePointer();
        }
        else {
            throw new UnsupportedOperationException(
                "Cannot create the root object: " + asPath());
        }
    }

    public NodePointer createChild(
        JXPathContext context,
        QName name,
        int index) 
    {
        return createPath(context).createChild(context, name, index);
    }

    public NodePointer createChild(
        JXPathContext context,
        QName name, 
        int index,
        Object value) 
    {
        return createPath(context).createChild(context, name, index, value);
    }

    public int hashCode() {
        return name == null ? 0 : name.hashCode();
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof NullPointer)) {
            return false;
        }

        NullPointer other = (NullPointer) object;
        return (name == null && other.name == null)
            || (name != null && name.equals(other.name));
    }

    public String asPath() {
        if (id != null) {
            return "id(" + id + ")";
        }

        if (parent != null) {
            return super.asPath();
        }
        return "null()";
    }

    public int getLength() {
        return 0;
    }
}
