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
 * @(#)XPathExpression.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath;

import java.util.Collection;


/**
 * <p>
 * Represents an XPath expression.
 * <p>
 * An expression may have children representing steps in a location path or
 * arguments to operations and functions. The children expressions are
 * represented as an ordered collection.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XPathExpression {
    
    /**
     * Gets the list of child expressions.
     * @return a collection of child expressions
     */
    Collection getChildren();
    
    
    /**
     * Gets the number of children expressions.
     * @return the count of children expressions
     */
    int getChildCount();
    
    
    /**
     * Gets the child expression at the specified location.
     * @param index the index of the child to get
     * @return the child
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    XPathExpression getChild(int index) throws IndexOutOfBoundsException;
    
    
    /**
     * Adds a child expression.
     * @param child to be added
     */
    void addChild(XPathExpression child);
    
    
    /**
     * Removes a child expression.
     * @param child to be removed
     * @return <code>true</code> if the child was found and removed
     */
    boolean removeChild(XPathExpression child);
    
    
    /**
     * Removes all the child expressions.
     */
    void clearChildren();
    
    
    /**
     * Gets the string representation of the expression.
     * @return the string representation
     */
    String getExpressionString();
}
