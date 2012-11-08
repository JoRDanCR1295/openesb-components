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
 * @(#)ExpressionWriter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.XPathCoreFunction;
import com.sun.bpel.model.xpath.XPathCoreOperation;
import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathExpressionPath;
import com.sun.bpel.model.xpath.XPathExtensionFunction;
import com.sun.bpel.model.xpath.XPathLocationPath;
import com.sun.bpel.model.xpath.XPathNumericLiteral;
import com.sun.bpel.model.xpath.XPathStringLiteral;
import com.sun.bpel.model.xpath.XPathVisitor;

import java.util.Iterator;

/**
 * Implements the XPathVisitor interface to generate a string representation of an expression.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class ExpressionWriter implements XPathVisitor {

    /** The string buffer. */
    private StringBuffer mBuffer;

    /** Constructor. */
    public ExpressionWriter() {
        mBuffer = new StringBuffer();
    }

    /**
     * Gets the string representation of the expression.
     * 
     * @return the string representation
     */
    public String getString() {
        return mBuffer.toString();
    }

    /**
     * Visits an location step.
     * 
     * @param locationStep to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(LocationStep locationStep) {
        mBuffer.append('/');

        mBuffer.append(locationStep.getString());
        XPathExpression[] predicates = locationStep.getPredicates();
        if (predicates != null) {
            for (int j = 0, length = predicates.length; j < length; j++) {
                mBuffer.append('[');
                mBuffer.append(predicates[j].getExpressionString());
                mBuffer.append(']');
            }
        }

        return true;
    }

    /**
     * Visits a string literal.
     * 
     * @param stringLiteral to visit
     * @return must be false since string literals don't have children
     */
    public boolean visit(XPathStringLiteral stringLiteral) {

        if (stringLiteral.isVariable()) {
            mBuffer.append("$" + stringLiteral.getValue());
            return false;
        }

        // quotes in literal strings for xpath 1.0 basically work like this:
        // 1. we can either quote strings with single or double quotes
        // 2. quote the string with single quotes if the string contains double quotes
        // i.e. 'the "correct" way', 'the 'incorrect' way'
        // 3. quote the string with double quotes if the string contains single quotes
        // i.e. "the 'correct' way", "the "incorrect" way"
        // - josh

        String literal = stringLiteral.getValue();
        boolean isStringQuoted = false;
        if (literal.length() >= 2) {
            if (literal.startsWith("'") && literal.endsWith("'")) {
                isStringQuoted = true;
            } else if (literal.startsWith("\"") && literal.endsWith("\"")) {
                isStringQuoted = true;
            }
        }

        if (isStringQuoted) {
            // if literal is already quoted, do not quote the literal
            mBuffer.append(literal);
        } else {
            if (literal.indexOf("'") >= 0) {
                // string contains a single-quote,
                // it must be quoted with double-quotes
                mBuffer.append("\"");
                mBuffer.append(literal);
                mBuffer.append("\"");
            } else {
                // quote the string with single-quotes by default
                mBuffer.append("'");
                mBuffer.append(literal);
                mBuffer.append("'");
            }
        }

        return false;
    }

    /**
     * Visits a numeric literal.
     * 
     * @param numericLiteral to visit
     * @return must be false since numeric literals don't have children
     */
    public boolean visit(XPathNumericLiteral numericLiteral) {
        mBuffer.append(numericLiteral.getValue().toString());
        return false;
    }

    /**
     * Visits a location path.
     * 
     * @param locationPath to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(XPathLocationPath locationPath) {
        LocationStep[] steps = locationPath.getSteps();
        if (locationPath.getAbsolute()) {
            mBuffer.append('/');
        }
        for (int i = 0; i < steps.length; i++) {
            if (i != 0) {
                mBuffer.append('/');
            }
            mBuffer.append(steps[i].getString());
            XPathExpression[] predicates = steps[i].getPredicates();
            if (predicates != null) {
                for (int j = 0, length = predicates.length; j < length; j++) {
                    mBuffer.append('[');
                    mBuffer.append(predicates[j].getExpressionString());
                    mBuffer.append(']');
                }
            }
        }
        return true;
    }

    /**
     * Visits a expression path.
     * 
     * @param locationPath to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(XPathExpressionPath expressionPath) {
        XPathExpression rootExpression = expressionPath.getRootExpression();
        if (rootExpression != null) {
            mBuffer.append(rootExpression.getExpressionString());
        }

        LocationStep[] steps = expressionPath.getSteps();

        for (int i = 0; i < steps.length; i++) {
            mBuffer.append('/');
            mBuffer.append(steps[i].getString());
            XPathExpression[] predicates = steps[i].getPredicates();
            if (predicates != null) {
                for (int j = 0, length = predicates.length; j < length; j++) {
                    mBuffer.append('[');
                    mBuffer.append(predicates[j].getExpressionString());
                    mBuffer.append(']');
                }
            }
        }
        return true;
    }

    /**
     * Visits a core operation.
     * 
     * @param coreOperation to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(XPathCoreOperation coreOperation) {
        if (XPathCoreOperation.OP_NEGATIVE == coreOperation.getOperator()) {
            mBuffer.append(coreOperation.getSign());
            mBuffer.append(coreOperation.getChild(0).getExpressionString());
        } else {
            mBuffer.append(" ( ");
            mBuffer.append(coreOperation.getChild(0).getExpressionString());
            mBuffer.append(' ');
            mBuffer.append(coreOperation.getSign());
            mBuffer.append(' ');
            mBuffer.append(coreOperation.getChild(1).getExpressionString());
            mBuffer.append(" ) ");
        }
        return false;
    }

    /**
     * Visits a core function.
     * 
     * @param coreFunction to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(XPathCoreFunction coreFunction) {
        mBuffer.append(coreFunction.getName());
        mBuffer.append('(');
        for (Iterator iter = coreFunction.getChildren().iterator(); iter.hasNext();) {
            XPathExpression expr = (XPathExpression) iter.next();
            mBuffer.append(expr.getExpressionString());
            if (iter.hasNext()) {
                mBuffer.append(", ");
            }
        }
        mBuffer.append(')');
        return false;
    }

    /**
     * Visits an extension function.
     * 
     * @param extensionFunction to visit
     * @return true means the child expressions can be visited
     */
    public boolean visit(XPathExtensionFunction extensionFunction) {
        mBuffer.append(extensionFunction.getName());
        mBuffer.append('(');
        for (Iterator iter = extensionFunction.getChildren().iterator(); iter.hasNext();) {
            XPathExpression expr = (XPathExpression) iter.next();
            mBuffer.append(expr.getExpressionString());
            if (iter.hasNext()) {
                mBuffer.append(", ");
            }
        }
        mBuffer.append(')');
        return false;
    }
}
