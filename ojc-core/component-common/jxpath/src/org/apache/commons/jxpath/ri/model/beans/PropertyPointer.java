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
 * @(#)PropertyPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.JXPathIntrospector;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.util.ValueUtils;

/**
 * A pointer allocated by a PropertyOwnerPointer to represent the value of
 * a property of the parent object.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class PropertyPointer extends NodePointer {
    public static final int UNSPECIFIED_PROPERTY = Integer.MIN_VALUE;

    protected int propertyIndex = UNSPECIFIED_PROPERTY;
    protected Object bean;

    /**
     * Takes a javabean, a descriptor of a property of that object and
     * an offset within that property (starting with 0).
     */
    public PropertyPointer(NodePointer parent) {
        super(parent);
    }

    public int getPropertyIndex() {
        return propertyIndex;
    }

    public void setPropertyIndex(int index) {
        if (propertyIndex != index) {
            propertyIndex = index;
            setIndex(WHOLE_COLLECTION);
        }
    }

    public Object getBean() {
        if (bean == null) {
            bean = getImmediateParentPointer().getNode();
        }
        return bean;
    }

    public QName getName() {
        return new QName(null, getPropertyName());
    }

    public abstract String getPropertyName();

    public abstract void setPropertyName(String propertyName);

    public abstract int getPropertyCount();

    public abstract String[] getPropertyNames();

    protected abstract boolean isActualProperty();

    public boolean isActual() {
        if (!isActualProperty()) {
            return false;
        }

        return super.isActual();
    }

    private static final Object UNINITIALIZED = new Object();

    private Object value = UNINITIALIZED;
    public Object getImmediateNode() {
        if (value == UNINITIALIZED) {
            if (index == WHOLE_COLLECTION) {
                value = ValueUtils.getValue(getBaseValue());
            }
            else {
                value = ValueUtils.getValue(getBaseValue(), index);
            }
        }
        return value;
    }
    
    public boolean isCollection() {
        Object value = getBaseValue();
        return value != null && ValueUtils.isCollection(value);
    }
    
    public boolean isLeaf() {
        Object value = getNode();
        return value == null
            || JXPathIntrospector.getBeanInfo(value.getClass()).isAtomic();
    }    

    /**
     * If the property contains a collection, then the length of that
     * collection, otherwise - 1.
     */
    public int getLength() {
        return ValueUtils.getLength(getBaseValue());
    }


    /**
     * Returns a NodePointer that can be used to access the currently
     * selected property value.
     */
    public NodePointer getImmediateValuePointer() {
        return NodePointer.newChildNodePointer(
            this,
            getName(),
            getImmediateNode());
    }

    public NodePointer createPath(JXPathContext context) {
        if (getImmediateNode() == null) {
            AbstractFactory factory = getAbstractFactory(context);
            int inx = (index == WHOLE_COLLECTION ? 0 : index);
            boolean success =
                factory.createObject(
                    context,
                    this,
                    getBean(),
                    getPropertyName(),
                    inx);
            if (!success) {
                throw new JXPathException(
                    "Factory "
                        + factory
                        + " could not create an object for path: "
                        + asPath());
            }
        }
        return this;
    }

    public NodePointer createPath(JXPathContext context, Object value) {
        // If neccessary, expand collection
        if (index != WHOLE_COLLECTION && index >= getLength()) {
            createPath(context);
        }
        setValue(value);            
        return this;
    }

    public NodePointer createChild(
        JXPathContext context,
        QName name,
        int index,
        Object value) 
    {
        PropertyPointer prop = (PropertyPointer) clone();
        if (name != null) {
            prop.setPropertyName(name.toString());
        }
        prop.setIndex(index);
        return prop.createPath(context, value);
    }

    public NodePointer createChild(
        JXPathContext context,
        QName name,
        int index) 
    {
        PropertyPointer prop = (PropertyPointer) clone();
        if (name != null) {
            prop.setPropertyName(name.toString());
        }
        prop.setIndex(index);
        return prop.createPath(context);
    }

    public int hashCode() {
        return getImmediateParentPointer().hashCode() + propertyIndex + index;
    }

    public boolean equals(Object object) {
        if (object == this) {
            return true;
        }

        if (!(object instanceof PropertyPointer)) {
            return false;
        }

        PropertyPointer other = (PropertyPointer) object;
        if (parent != other.parent) {
            if (parent == null || !parent.equals(other.parent)) {
                return false;
            }
        }

        if (getPropertyIndex() != other.getPropertyIndex()
            || !getPropertyName().equals(other.getPropertyName())) {
            return false;
        }

        int iThis = (index == WHOLE_COLLECTION ? 0 : index);
        int iOther = (other.index == WHOLE_COLLECTION ? 0 : other.index);
        return iThis == iOther;
    }

    public int compareChildNodePointers(
        NodePointer pointer1,
        NodePointer pointer2) 
    {
        return getValuePointer().compareChildNodePointers(pointer1, pointer2);
    }
    
    private AbstractFactory getAbstractFactory(JXPathContext context) {
        AbstractFactory factory = context.getFactory();
        if (factory == null) {
            throw new JXPathException(
                "Factory is not set on the "
                    + "JXPathContext - cannot create path: "
                    + asPath());
        }
        return factory;
    }
}
