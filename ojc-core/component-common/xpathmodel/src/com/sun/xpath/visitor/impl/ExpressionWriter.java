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

package com.sun.xpath.visitor.impl;


import java.util.Iterator;

import com.sun.xpath.LocationStep;
import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathExpressionPath;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathFunctionPropertyParam;
import com.sun.xpath.XPathFunctionVariableParam;
import com.sun.xpath.XPathLocationPath;
import com.sun.xpath.XPathNumericLiteral;
import com.sun.xpath.XPathStringLiteral;
import com.sun.xpath.XPathVariableReference;
import com.sun.xpath.visitor.AbstractXPathVisitor;


/**
 * Implements the XPathVisitor interface to generate a string representation
 * of an expression.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class ExpressionWriter extends AbstractXPathVisitor  {
    
    /** The string buffer. */
    private StringBuffer mBuffer;
    
    
    /** Constructor. */
    public ExpressionWriter() {
        mBuffer = new StringBuffer();
    }
    
    
    /**
     * Gets the string representation of the expression.
     * @return the string representation
     */
    public String getString() {
        return mBuffer.toString();
    }
    

	/**
     * Visits an location step.
     * @param locationStep to visit
     */
    public void visit(LocationStep locationStep) {
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
	    
    }

    public void visit(XPathVariableReference vReference) {
	    mBuffer.append("$" + vReference.getVariableName());
    }
    
    /**
     * Visits a string literal.
     * @param stringLiteral to visit
     */
    public void visit(XPathStringLiteral stringLiteral) {
        
        // quotes in literal strings for xpath 1.0 basically work like this:
        // 1. we can either quote strings with single or double quotes
        // 2. quote the string with single quotes if the string contains double quotes
        //    i.e.   'the "correct" way',   'the 'incorrect' way'
        // 3. quote the string with double quotes if the string contains single quotes
        //    i.e.   "the 'correct' way",   "the "incorrect" way"
        // - josh
        
        String literal = stringLiteral.getValue();
        boolean isStringQuoted = false;
        if (literal.length() >= 2) {
            if        (literal.startsWith("'")  && literal.endsWith("'")) {
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
        
    }
    
    
    /**
     * Visits a numeric literal.
     * @param numericLiteral to visit
     */
    public void visit(XPathNumericLiteral numericLiteral) {
        mBuffer.append(numericLiteral.getValue().toString());
    }
    
    
    /**
     * Visits a location path.
     * @param locationPath to visit
     */
    public void visit(XPathLocationPath locationPath) {
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
        
       }
    
    

    /**
     * Visits a expression path.
     * @param locationPath to visit
     */
    public void visit(XPathExpressionPath expressionPath) {
        XPathExpression rootExpression = expressionPath.getRootExpression();
        if(rootExpression != null) {
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
    }
    
    /**
     * Visits a core operation.
     * @param coreOperation to visit
     */
    public void visit(XPathCoreOperation coreOperation) {
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
         
    }
    
    
    /**
     * Visits a core function.
     * @param coreFunction to visit
     */
    public void visit(XPathCoreFunction coreFunction) {
        mBuffer.append(coreFunction.getName());
        mBuffer.append('(');
        for (Iterator iter = coreFunction.getChildren().iterator();
                iter.hasNext();) {
            XPathExpression expr = (XPathExpression) iter.next();
            mBuffer.append(expr.getExpressionString());
            if (iter.hasNext()) {
                mBuffer.append(", ");
            }
        }
        mBuffer.append(')');
    }
    
    
    /**
     * Visits an extension function.
     * @param extensionFunction to visit
     */
    public void visit(XPathExtensionFunction extensionFunction) {
        mBuffer.append(extensionFunction.getName());
        mBuffer.append('(');
        for (Iterator iter = extensionFunction.getChildren().iterator();
                iter.hasNext();) {
            XPathExpression expr = (XPathExpression) iter.next();
            mBuffer.append(expr.getExpressionString());
            if (iter.hasNext()) {
                mBuffer.append(", ");
            }
        }
        mBuffer.append(')');
        
    }

    /*
	 * (non-Javadoc)
	 * @see com.sun.xpath.visitor.XPathVisitor#visit(com.sun.xpath.XPathFunctionVariableParam)
	 */
	public void visit(XPathFunctionVariableParam variableParam) {
		String val = variableParam.getValue();
		//TODO: Should this be passed to the visit(XPathStringLiteral) function
		// to take care of xpath1.0 literal quote styles??
		mBuffer.append(val);
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.xpath.visitor.XPathVisitor#visit(com.sun.xpath.XPathFunctionPropertyParam)
	 */
	public void visit(XPathFunctionPropertyParam propertyParam) {
		String propValue = propertyParam.getValue();
		//TODO: Should this be passed to the visit(XPathStringLiteral) function
		// to take care of xpath1.0 literal quote styles??
		mBuffer.append(propValue);
	}

}
