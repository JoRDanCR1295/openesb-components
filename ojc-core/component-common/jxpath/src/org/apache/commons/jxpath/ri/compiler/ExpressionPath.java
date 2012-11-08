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
 * @(#)ExpressionPath.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.axes.InitialContext;
import org.apache.commons.jxpath.ri.axes.NodeSetContext;
import org.apache.commons.jxpath.ri.axes.PredicateContext;
import org.apache.commons.jxpath.ri.axes.SimplePathInterpreter;
import org.apache.commons.jxpath.ri.axes.UnionContext;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * An  element of the parse tree that represents an expression path, which is a
 * path that starts with an expression like a function call: <code>getFoo(.)
 * /bar</code>.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ExpressionPath extends Path {

    private Expression expression;
    private Expression predicates[];

    private boolean basicKnown = false;
    private boolean basic;

    public ExpressionPath(
        Expression expression,
        Expression[] predicates,
        Step[] steps) 
    {
        super(steps);
        this.expression = expression;
        this.predicates = predicates;
    }

    public Expression getExpression() {
        return expression;
    }

    /**
     * Predicates are the expressions in brackets that may follow
     * the root expression of the path.
     */
    public Expression[] getPredicates() {
        return predicates;
    }

    /**
     * Returns true if the root expression or any of the
     * predicates or the path steps are context dependent.
     */
    public boolean computeContextDependent() {
        if (expression.isContextDependent()) {
            return true;
        }
        if (predicates != null) {
            for (int i = 0; i < predicates.length; i++) {
                if (predicates[i].isContextDependent()) {
                    return true;
                }
            }
        }
        return super.computeContextDependent();
    }

    /**
     * Recognized paths formatted as <code>$x[3]/foo[2]</code>.  The
     * evaluation of such "simple" paths is optimized and streamlined.
     */
    public boolean isSimpleExpressionPath() {
        if (!basicKnown) {
            basicKnown = true;
            basic = isSimplePath() && areBasicPredicates(getPredicates());
        }
        return basic;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        if (expression instanceof CoreOperation
            || expression instanceof ExpressionPath
            || expression instanceof LocationPath) {
            buffer.append('(');
            buffer.append(expression);
            buffer.append(')');
        }
        else {
            buffer.append(expression);
        }
        if (predicates != null) {
            for (int i = 0; i < predicates.length; i++) {
                buffer.append('[');
                buffer.append(predicates[i]);
                buffer.append(']');
            }
        }

        Step steps[] = getSteps();
        if (steps != null) {
            for (int i = 0; i < steps.length; i++) {
                buffer.append("/");
                buffer.append(steps[i]);
            }
        }
        return buffer.toString();
    }

    public Object compute(EvalContext context) {
        return expressionPath(context, false);
    }

    public Object computeValue(EvalContext context) {
        return expressionPath(context, true);
    }

    /**
     * Walks an expression path (a path that starts with an expression)
     */
    protected Object expressionPath(
        EvalContext evalContext,
        boolean firstMatch) 
    {
        Object value = expression.compute(evalContext);
        EvalContext context;
        if (value instanceof InitialContext) {
            // This is an optimization. We can avoid iterating through a 
            // collection if the context bean is in fact one.
            context = (InitialContext) value;
        }
        else if (value instanceof EvalContext) {
            // UnionContext will collect all values from the "value" context
            // and treat the whole thing as a big collection.
            context =
                new UnionContext(
                    evalContext,
                    new EvalContext[] {(EvalContext) value });
        }
        else {
            context = evalContext.getRootContext().getConstantContext(value);
        }

        if (firstMatch
            && isSimpleExpressionPath()
            && !(context instanceof NodeSetContext)) {
            EvalContext ctx = context;
            NodePointer ptr = (NodePointer) ctx.getSingleNodePointer();
            if (ptr != null
                && (ptr.getIndex() == NodePointer.WHOLE_COLLECTION
                    || predicates == null
                    || predicates.length == 0)) {
                return SimplePathInterpreter.interpretSimpleExpressionPath(
                    evalContext,
                    ptr,
                    predicates,
                    getSteps());
            }
        }
        if (predicates != null) {
            for (int j = 0; j < predicates.length; j++) {
                context = new PredicateContext(context, predicates[j]);
            }
        }
        if (firstMatch) {
            return getSingleNodePointerForSteps(context);
        }
        else {
            return evalSteps(context);
        }
    }
}
