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
 * @(#)XPathCoreOperationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;


import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.visitor.XPathVisitor;


/**
 * Represents a core XPath operation.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XPathCoreOperationImpl
    extends XPathOperatorOrFunctionImpl
    implements XPathCoreOperation {
        
    /** The operator code. */
    int mOperator;
    
    
    /**
     * Constructor. Instantiates a new XPathCoreOperation with the given code.
     * @param operator the operator code
     */
    public XPathCoreOperationImpl(int operator) {
        super();
        setOperator(operator);
    }
    
    
    /**
     * Gets the operator code.
     * @return the operator code
     */
    public int getOperator() {
        return mOperator;
    }
    
    
    /**
     * Sets the operator code.
     * @param operator the operator code
     */
    public void setOperator(int operator) {
        mOperator = operator;
    }
    
    
    /**
     * Gets the name of the operator.
     * @return the operator name
     */
    public String getName() {
        int code = getOperator();

        switch (code) {
        case XPathCoreOperation.OP_SUM:
            return "addition";
        case XPathCoreOperation.OP_MINUS:
            return "subtraction";
        case XPathCoreOperation.OP_MULT:
            return "multiplication";
        case XPathCoreOperation.OP_DIV:
            return "division";
        case XPathCoreOperation.OP_MOD:
            return "remainder";
        case XPathCoreOperation.OP_NEGATIVE:
            return "negative";
        case XPathCoreOperation.OP_AND:
            return "and";
        case XPathCoreOperation.OP_OR:
            return "or";
        case XPathCoreOperation.OP_EQ:
            return "equal";
        case XPathCoreOperation.OP_NE:
            return "not_equal";
        case XPathCoreOperation.OP_LT:
            return "lesser_than";
        case XPathCoreOperation.OP_LE:
            return "lesser_or_equal";
        case XPathCoreOperation.OP_GT:
            return "greater_than";
        case XPathCoreOperation.OP_GE:
            return "greater_or_equal";
        }

        return null;
    }
    
    
    /**
     * Gets the operator sign.
     * @return the operator sign
     */
    public String getSign() {
        int code = getOperator();

        switch (code) {
        case XPathCoreOperation.OP_SUM:
            return "+";
        case XPathCoreOperation.OP_MINUS:
            return "-";
        case XPathCoreOperation.OP_MULT:
            return "*";
        case XPathCoreOperation.OP_DIV:
            return "div";
        case XPathCoreOperation.OP_MOD:
            return "mod";
        case XPathCoreOperation.OP_NEGATIVE:
            return "-";
        case XPathCoreOperation.OP_AND:
            return "and";
        case XPathCoreOperation.OP_OR:
            return "or";
        case XPathCoreOperation.OP_EQ:
            return "=";
        case XPathCoreOperation.OP_NE:
            return "!=";
        case XPathCoreOperation.OP_LT:
            return "<";
        case XPathCoreOperation.OP_LE:
            return "<=";
        case XPathCoreOperation.OP_GT:
            return ">";
        case XPathCoreOperation.OP_GE:
            return ">=";
        }

        return null;
    }

    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void accept(XPathVisitor visitor) {
        visitor.visit(this);
    }
}
