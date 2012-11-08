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
 * @(#)BeanPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import java.util.Locale;

import org.apache.commons.jxpath.JXPathBeanInfo;
import org.apache.commons.jxpath.JXPathIntrospector;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * A Pointer that points to a JavaBean or a collection. It is either
 * the first element of a path or a pointer for a property value.
 * Typically there is a BeanPropertyPointer between two BeanPointers
 * in the chain.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class BeanPointer extends PropertyOwnerPointer {
    private QName name;
    private Object bean;
    private JXPathBeanInfo beanInfo;

    public BeanPointer(
            QName name,
            Object bean,
            JXPathBeanInfo beanInfo,
            Locale locale) 
    {
        super(null, locale);
        this.name = name;
        this.bean = bean;
        this.beanInfo = beanInfo;
    }

    /**
     * @param name is the name given to the first node
     */
    public BeanPointer(
            NodePointer parent,
            QName name,
            Object bean,
            JXPathBeanInfo beanInfo) 
    {
        super(parent);
        this.name = name;
        this.bean = bean;
        this.beanInfo = beanInfo;
    }

    public PropertyPointer getPropertyPointer() {
        return new BeanPropertyPointer(this, beanInfo);
    }

    public QName getName() {
        return name;
    }

    /**
     * Returns the bean itself
     */
    public Object getBaseValue() {
        return bean;
    }

    /**
     * Returns false
     */
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
        Object value = getNode();
        return value == null
            || JXPathIntrospector.getBeanInfo(value.getClass()).isAtomic();
    }

    public int hashCode() {
        return name == null ? 0 : name.hashCode();
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof BeanPointer)) {
            return false;
        }

        BeanPointer other = (BeanPointer) object;
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

        if (bean instanceof Number
            || bean instanceof String
            || bean instanceof Boolean) {
            return bean.equals(other.bean);
        }
        return bean == other.bean;
    }

    /**
     * If the pointer has a parent, then parent's path.
     * If the bean is null, "null()".
     * If the bean is a primitive value, the value itself.
     * Otherwise - an empty string.
     */
    public String asPath() {
        if (parent != null) {
            return super.asPath();
        }
        else if (bean == null) {
            return "null()";
        }
        else if (bean instanceof Number) {
            String string = bean.toString();
            if (string.endsWith(".0")) {
                string = string.substring(0, string.length() - 2);
            }
            return string;
        }
        else if (bean instanceof Boolean) {
            return ((Boolean) bean).booleanValue() ? "true()" : "false()";
        }
        else if (bean instanceof String) {
            return "'" + bean + "'";
        }
        return "/";
    }
}
