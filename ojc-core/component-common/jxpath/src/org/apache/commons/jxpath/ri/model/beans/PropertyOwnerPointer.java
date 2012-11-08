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
 * @(#)PropertyOwnerPointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import java.util.Locale;

import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.ri.Compiler;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.compiler.NodeNameTest;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.compiler.NodeTypeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.util.ValueUtils;

/**
 * A pointer describing a node that has properties, each of which could be
 * a collection.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class PropertyOwnerPointer extends NodePointer {

    public NodeIterator childIterator(
        NodeTest test,
        boolean reverse,
        NodePointer startWith) 
    {
        if (test == null) {
            return createNodeIterator(null, reverse, startWith);
        }
        else if (test instanceof NodeNameTest) {
            NodeNameTest nodeNameTest = (NodeNameTest) test;
            QName testName = nodeNameTest.getNodeName();
            String property;
            if (!isDefaultNamespace(testName.getPrefix())) {
                return null;
            }
            else if (nodeNameTest.isWildcard()) {
                property = null;
            }
            else {
                property = testName.getName();
            }
            return createNodeIterator(property, reverse, startWith);
        }
        else if (test instanceof NodeTypeTest) {
            if (((NodeTypeTest) test).getNodeType()
                == Compiler.NODE_TYPE_NODE) {
                return createNodeIterator(null, reverse, startWith);
            }
        }
        return null;
    }

    public NodeIterator createNodeIterator(
                String property,
                boolean reverse,
                NodePointer startWith) 
    {
        return new PropertyIterator(this, property, reverse, startWith);
    }

    public NodeIterator attributeIterator(QName name) {
        return new BeanAttributeIterator(this, name);
    }

    protected PropertyOwnerPointer(NodePointer parent, Locale locale) {
        super(parent, locale);
    }

    protected PropertyOwnerPointer(NodePointer parent) {
        super(parent);
    }

    public void setIndex(int index) {
        if (this.index != index) {
            super.setIndex(index);
            value = UNINITIALIZED;
        }
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

    public abstract QName getName();

    /**
     * Throws an exception if you try to change the root element, otherwise
     * forwards the call to the parent pointer.
     */
    public void setValue(Object value) {
        this.value = value;
        if (parent.isContainer()) {
            parent.setValue(value);
        }
        else if (parent != null) {
            if (index == WHOLE_COLLECTION) {
                throw new UnsupportedOperationException(
                    "Cannot setValue of an object that is not "
                        + "some other object's property");
            }
            else {
                throw new JXPathException(
                    "The specified collection element does not exist: " + this);
            }
        }
        else {
            throw new UnsupportedOperationException(
                "Cannot replace the root object");
        }
    }

    /**
     * If this is a root node pointer, throws an exception; otherwise
     * forwards the call to the parent node.
     */
    public void remove() {
        this.value = null;
        if (parent != null) {
            parent.remove();
        }
        else {
            throw new UnsupportedOperationException(
                "Cannot remove an object that is not "
                    + "some other object's property or a collection element");
        }
    }

    public abstract PropertyPointer getPropertyPointer();
    
    /**
     * @return true if the property owner can set a property "does not exist".
     *         A good example is a Map. You can always assign a value to any
     *         key even if it has never been "declared".
     */
    public boolean isDynamicPropertyDeclarationSupported() {
        return false;
    }

    public int compareChildNodePointers(
        NodePointer pointer1,
        NodePointer pointer2) 
    {
        int r =
            pointer1.getName().toString().compareTo(
                pointer2.getName().toString());
        if (r != 0) {
            return r;
        }
        return pointer1.getIndex() - pointer2.getIndex();
    }
}
