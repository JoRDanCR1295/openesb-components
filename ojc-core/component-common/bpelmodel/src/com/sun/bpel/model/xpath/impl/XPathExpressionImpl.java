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
 * @(#)XPathExpressionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathVisitable;
import com.sun.bpel.model.xpath.XPathVisitor;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;


/**
 * Default implementation of the XPathExpression interface.
 * 
 * @author Sun Microsystems
 * @version 
 */
public abstract class XPathExpressionImpl
    implements XPathExpression, XPathVisitable {
    
    /** List of child expressions. */
    protected List mChildren;
    
    
    /** Constructor. */
    protected XPathExpressionImpl() {
        mChildren = new ArrayList();
    }
    
    /**
     * Gets the list of child expressions.
     * @return a collection of child expressions
     */
    public Collection getChildren() {
        return Collections.unmodifiableCollection(mChildren);
    }
    
    
    /**
     * Gets the number of children expressions.
     * @return the count of children expressions
     */
    public int getChildCount() {
        return mChildren.size();
    }
    
    
    /**
     * Gets the child expression at the specified location.
     * @param index the index of the child to get
     * @return the child
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    public XPathExpression getChild(int index)
        throws IndexOutOfBoundsException {
        return (XPathExpression) mChildren.get(index);
    }
    
    
    /**
     * Adds a child expression.
     * @param child to be added
     */
    public void addChild(XPathExpression child) {
        mChildren.add(child);
    }
    
    
    /**
     * Removes a child expression.
     * @param child to be removed
     * @return <code>true</code> if the child was found and removed
     */
    public boolean removeChild(XPathExpression child) {
        return mChildren.remove(child);
    }
    
    
    /**
     * Removes all the child expressions.
     */
    public void clearChildren() {
        mChildren.clear();
    }

    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void callVisitor(XPathVisitor visitor) {
        // do nothing -- must be subclassed
    }
    
    
    /**
     * String representation.
     * @return the string representation
     */
    public String getExpressionString() {
        XPathVisitor visitor = new ExpressionWriter();
        callVisitor(visitor);
        return ((ExpressionWriter) visitor).getString();
    }
}
