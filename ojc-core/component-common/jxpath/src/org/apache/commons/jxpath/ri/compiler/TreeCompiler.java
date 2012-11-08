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
 * @(#)TreeCompiler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.Compiler;
import org.apache.commons.jxpath.ri.QName;

/**
 * @author Dmitri Plotnikov
 * @version  
 */
public class TreeCompiler implements Compiler {

    private static final QName QNAME_NAME = new QName(null, "name");

    public Object number(String value) {
        return new Constant(new Double(value));
    }

    public Object literal(String value) {
        return new Constant(value);
    }

    public Object qname(String prefix, String name) {
        return new QName(prefix, name);
    }

    public Object sum(Object[] arguments) {
        return new CoreOperationAdd(toExpressionArray(arguments));
    }

    public Object minus(Object left, Object right) {
        return new CoreOperationSubtract(
            (Expression) left,
            (Expression) right);
    }

    public Object multiply(Object left, Object right) {
        return new CoreOperationMultiply((Expression) left, (Expression) right);
    }

    public Object divide(Object left, Object right) {
        return new CoreOperationDivide((Expression) left, (Expression) right);
    }

    public Object mod(Object left, Object right) {
        return new CoreOperationMod((Expression) left, (Expression) right);
    }

    public Object lessThan(Object left, Object right) {
        return new CoreOperationLessThan((Expression) left, (Expression) right);
    }

    public Object lessThanOrEqual(Object left, Object right) {
        return new CoreOperationLessThanOrEqual(
            (Expression) left,
            (Expression) right);
    }

    public Object greaterThan(Object left, Object right) {
        return new CoreOperationGreaterThan(
            (Expression) left,
            (Expression) right);
    }

    public Object greaterThanOrEqual(Object left, Object right) {
        return new CoreOperationGreaterThanOrEqual(
            (Expression) left,
            (Expression) right);
    }

    public Object equal(Object left, Object right) {
        if (isNameAttributeTest((Expression) left)) {
            return new NameAttributeTest((Expression) left, (Expression) right);
        }
        else {
            return new CoreOperationEqual(
                (Expression) left,
                (Expression) right);
        }
    }

    public Object notEqual(Object left, Object right) {
        return new CoreOperationNotEqual(
            (Expression) left,
            (Expression) right);
    }

    public Object minus(Object argument) {
        return new CoreOperationNegate((Expression) argument);
    }

    public Object variableReference(Object qName) {
        return new VariableReference((QName) qName);
    }

    public Object function(int code, Object[] args) {
        return new CoreFunction(code, toExpressionArray(args));
    }

    public Object function(Object name, Object[] args) {
        return new ExtensionFunction((QName) name, toExpressionArray(args));
    }

    public Object and(Object arguments[]) {
        return new CoreOperationAnd(
            toExpressionArray(arguments));
    }

    public Object or(Object arguments[]) {
        return new CoreOperationOr(
            toExpressionArray(arguments));
    }

    public Object union(Object[] arguments) {
        return new CoreOperationUnion(
            toExpressionArray(arguments));
    }

    public Object locationPath(boolean absolute, Object[] steps) {
        return new LocationPath(absolute, toStepArray(steps));
    }

    public Object expressionPath(
        Object expression,
        Object[] predicates,
        Object[] steps) 
    {
        return new ExpressionPath(
            (Expression) expression,
            toExpressionArray(predicates),
            toStepArray(steps));
    }

    public Object nodeNameTest(Object qname) {
        return new NodeNameTest((QName) qname);
    }

    public Object nodeTypeTest(int nodeType) {
        return new NodeTypeTest(nodeType);
    }

    public Object processingInstructionTest(String instruction) {
        return new ProcessingInstructionTest(instruction);
    }

    public Object step(int axis, Object nodeTest, Object[] predicates) {
        return new Step(
            axis,
            (NodeTest) nodeTest,
            toExpressionArray(predicates));
    }

    private Expression[] toExpressionArray(Object[] array) {
        Expression expArray[] = null;
        if (array != null) {
            expArray = new Expression[array.length];
            for (int i = 0; i < expArray.length; i++) {
                expArray[i] = (Expression) array[i];
            }
        }
        return expArray;
    }

    private Step[] toStepArray(Object[] array) {
        Step stepArray[] = null;
        if (array != null) {
            stepArray = new Step[array.length];
            for (int i = 0; i < stepArray.length; i++) {
                stepArray[i] = (Step) array[i];
            }
        }
        return stepArray;
    }

    private boolean isNameAttributeTest(Expression arg) {
        if (!(arg instanceof LocationPath)) {
            return false;
        }

        Step[] steps = ((LocationPath) arg).getSteps();
        if (steps.length != 1) {
            return false;
        }
        if (steps[0].getAxis() != Compiler.AXIS_ATTRIBUTE) {
            return false;
        }
        NodeTest test = steps[0].getNodeTest();
        if (!(test instanceof NodeNameTest)) {
            return false;
        }
        if (!((NodeNameTest) test).getNodeName().equals(QNAME_NAME)) {
            return false;
        }
        return true;
    }
}
