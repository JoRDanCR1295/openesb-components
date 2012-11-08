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
 * @(#)CoreOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;

/**
 * The common subclass for tree elements representing core operations like "+",
 * "- ", "*" etc.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class CoreOperation extends Operation {
        
    public CoreOperation(Expression args[]) {
        super(args);
    }

    public Object compute(EvalContext context) {
        return computeValue(context);
    }

    public abstract Object computeValue(EvalContext context);
    
    /**
     * Returns the XPath symbol for this operation, e.g. "+", "div", etc.
     */
    public abstract String getSymbol();
    
    /**
     * Returns true if the operation is not sensitive to the order of arguments,
     * e.g. "=", "and" etc, and false if it is, e.g. "&lt;=", "div".
     */
    protected abstract boolean isSymmetric();
    
    /**
     * Computes the precedence of the operation.
     */
    protected abstract int getPrecedence();
    
    public String toString() {
        if (args.length == 1) {
            return getSymbol() + parenthesize(args[0], false);
        }
        else {
            StringBuffer buffer = new StringBuffer();
            for (int i = 0; i < args.length; i++) {
                if (i > 0) {
                    buffer.append(' ');
                    buffer.append(getSymbol());
                    buffer.append(' ');
                }
                buffer.append(parenthesize(args[i], i == 0));
            }
            return buffer.toString();
        }
    }
    
    private String parenthesize(Expression expression, boolean left) {
        if (!(expression instanceof CoreOperation)) {
            return expression.toString();
        }
        CoreOperation op = (CoreOperation) expression;
        int myPrecedence = getPrecedence();
        int thePrecedence = op.getPrecedence();

        boolean needParens = true;
        if (myPrecedence < thePrecedence) {
            needParens = false;
        }
        else if (myPrecedence == thePrecedence) {
            if (isSymmetric()) {
                needParens = false;
            }
            else {
                needParens = !left;
            }
        }

        if (needParens) {
            return "(" + expression.toString() + ")";
        }
        else {
            return expression.toString();
        }
    }    
}
