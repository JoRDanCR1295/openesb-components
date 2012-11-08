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
 * @(#)CompiledExpression.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.Iterator;

/**
 * Represents a compiled XPath. The interpretation of compiled XPaths
 * may be faster, because it bypasses the compilation step. The reference
 * implementation of JXPathContext also globally caches some of the
 * results of compilation, so the direct use of JXPathContext is not
 * always less efficient than the use of CompiledExpression.
 * <p>
 * Use CompiledExpression only when there is a need to evaluate the
 * same expression multiple times and the CompiledExpression can be
 * conveniently cached.
 * <p>
 * To acqure a CompiledExpression, call {@link JXPathContext#compile
 * JXPathContext.compile}
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface CompiledExpression {

    /**
     * Evaluates the xpath and returns the resulting object. Primitive
     * types are wrapped into objects.
     */
    Object getValue(JXPathContext context);

    /**
     * Evaluates the xpath, converts the result to the specified class and
     * returns the resulting object.
     */
    Object getValue(JXPathContext context, Class requiredType);

    /**
     * Modifies the value of the property described by the supplied xpath.
     * Will throw an exception if one of the following conditions occurs:
     * <ul>
     * <li>The xpath does not in fact describe an existing property
     * <li>The property is not writable (no public, non-static set method)
     * </ul>
     */
    void setValue(JXPathContext context, Object value);

    /**
     * Creates intermediate elements of
     * the path by invoking an AbstractFactory, which should first be
     * installed on the context by calling "setFactory".
     */
    Pointer createPath(JXPathContext context);

    /**
     * The same as setValue, except it creates intermediate elements of
     * the path by invoking an AbstractFactory, which should first be
     * installed on the context by calling "setFactory".
     * <p>
     * Will throw an exception if one of the following conditions occurs:
     * <ul>
     * <li>Elements of the xpath aleady exist, by the path does not in
     *  fact describe an existing property
     * <li>The AbstractFactory fails to create an instance for an intermediate
     * element.
     * <li>The property is not writable (no public, non-static set method)
     * </ul>
     */
    Pointer createPathAndSetValue(JXPathContext context, Object value);

    /**
     * Traverses the xpath and returns a Iterator of all results found
     * for the path. If the xpath matches no properties
     * in the graph, the Iterator will not be null.
     */
    Iterator iterate(JXPathContext context);

    /**
     * Traverses the xpath and returns a Pointer.
     * A Pointer provides easy access to a property.
     * If the xpath matches no properties
     * in the graph, the pointer will be null.
     */
    Pointer getPointer(JXPathContext context, String xpath);

    /**
     * Traverses the xpath and returns an Iterator of Pointers.
     * A Pointer provides easy access to a property.
     * If the xpath matches no properties
     * in the graph, the Iterator be empty, but not null.
     */
    Iterator iteratePointers(JXPathContext context);

    /**
     * Remove the graph element described by this expression
     */
    void removePath(JXPathContext context);

    /**
     * Remove all graph elements described by this expression
     */
    void removeAll(JXPathContext context);
}
