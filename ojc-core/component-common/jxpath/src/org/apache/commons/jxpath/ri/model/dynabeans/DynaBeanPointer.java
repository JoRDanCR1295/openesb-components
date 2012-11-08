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
 * @(#)DynaBeanPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dynabeans;

import java.util.Locale;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.beans.PropertyOwnerPointer;
import org.apache.commons.jxpath.ri.model.beans.PropertyPointer;


/**
 * A Pointer that points to a DynaBean.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DynaBeanPointer extends PropertyOwnerPointer {
    private QName name;
    private DynaBean dynaBean;

    public DynaBeanPointer(QName name, DynaBean dynaBean, Locale locale) {
        super(null, locale);
        this.name = name;
        this.dynaBean = dynaBean;
    }

    /**
     * @param name is the name given to the first node
     */
    public DynaBeanPointer(NodePointer parent, QName name, DynaBean dynaBean) {
        super(parent);
        this.name = name;
        this.dynaBean = dynaBean;
    }

    public PropertyPointer getPropertyPointer() {
        return new DynaBeanPropertyPointer(this, dynaBean);
    }

    public QName getName() {
        return name;
    }

    /**
     * Returns the bean itself
     */
    public Object getBaseValue() {
        return dynaBean;
    }

    public Object getImmediateNode() {
        return dynaBean;
    }

    public boolean isCollection() {
        return false;
    }

    /**
     * Returns 1.
     */
    public int getLength() {
        return 1;
    }

    public boolean isLeaf() {
        return false;
    }

    public int hashCode() {
        return name == null ? 0 : name.hashCode();
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof DynaBeanPointer)) {
            return false;
        }

        DynaBeanPointer other = (DynaBeanPointer) object;
        if (parent != other.parent) {
            if (parent == null || !parent.equals(other.parent)) {
                return false;
            }
        }

        if ((name == null && other.name != null)
            || (name != null && !name.equals(other.name))) {
            return false;
        }

        int iThis = (index == WHOLE_COLLECTION ? 0 : index);
        int iOther = (other.index == WHOLE_COLLECTION ? 0 : other.index);
        if (iThis != iOther) {
            return false;
        }

        return dynaBean == other.dynaBean;
    }

    /**
     * If there's a parent - parent's path, otherwise "/".
     */
    public String asPath() {
        if (parent != null) {
            return super.asPath();
        }
        return "/";
    }
}
