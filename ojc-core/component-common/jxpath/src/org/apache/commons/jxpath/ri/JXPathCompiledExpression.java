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
 * @(#)JXPathCompiledExpression.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri;

import java.util.Iterator;

import org.apache.commons.jxpath.ri.compiler.Expression;
import org.apache.commons.jxpath.CompiledExpression;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;

/**
 *
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JXPathCompiledExpression implements CompiledExpression {

    private String xpath;
    private Expression expression;

    public JXPathCompiledExpression(String xpath, Expression expression) {
        this.xpath = xpath;
        this.expression = expression;
    }

    protected String getXPath() {
        return xpath;
    }

    protected Expression getExpression() {
        return expression;
    }

    public String toString() {
        return xpath;
    }
    
    /**
     * @see CompiledExpression#getValue(JXPathContext)
     */
    public Object getValue(JXPathContext context) {
        return ((JXPathContextReferenceImpl) context).
                    getValue(xpath, expression);
    }

    /**
     * @see CompiledExpression#getValue(JXPathContext, Class)
     */
    public Object getValue(JXPathContext context, Class requiredType) {
        return ((JXPathContextReferenceImpl) context).
                    getValue(xpath, expression, requiredType);
    }

    /**
     * @see CompiledExpression#setValue(JXPathContext, Object)
     */
    public void setValue(JXPathContext context, Object value) {
        ((JXPathContextReferenceImpl) context).
                    setValue(xpath, expression, value);
    }

    /**
     * @see CompiledExpression#createPath(JXPathContext)
     */
    public Pointer createPath(JXPathContext context) {
        return ((JXPathContextReferenceImpl) context).
                    createPath(xpath, expression);
    }

    /**
     * @see CompiledExpression#createPathAndSetValue(JXPathContext, Object)
     */
    public Pointer createPathAndSetValue(JXPathContext context, Object value) {
        return ((JXPathContextReferenceImpl) context).
                    createPathAndSetValue(xpath, expression, value);
    }

    /**
     * @see CompiledExpression#iterate(JXPathContext)
     */
    public Iterator iterate(JXPathContext context) {
        return ((JXPathContextReferenceImpl) context).
                    iterate(xpath, expression);
    }

    /**
     * @see CompiledExpression#getPointer(JXPathContext, String)
     */
    public Pointer getPointer(JXPathContext context, String xpath) {
        return ((JXPathContextReferenceImpl) context).
                    getPointer(xpath, expression);
    }

    /**
     * @see CompiledExpression#iteratePointers(JXPathContext)
     */
    public Iterator iteratePointers(JXPathContext context) {
        return ((JXPathContextReferenceImpl) context).
                    iteratePointers(xpath, expression);
    }

    /**
     * @see CompiledExpression#removePath(JXPathContext)
     */
    public void removePath(JXPathContext context) {
        ((JXPathContextReferenceImpl) context).removePath(xpath, expression);
    }

    /**
     * @see CompiledExpression#removeAll(JXPathContext)
     */
    public void removeAll(JXPathContext context) {
        ((JXPathContextReferenceImpl) context).removeAll(xpath, expression);
    }
}
