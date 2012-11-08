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
 * @(#)XPathVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.visitor;

import com.sun.xpath.LocationStep;
import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpressionPath;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathFunctionPropertyParam;
import com.sun.xpath.XPathFunctionVariableParam;
import com.sun.xpath.XPathLocationPath;
import com.sun.xpath.XPathNumericLiteral;
import com.sun.xpath.XPathPredicateExpression;
import com.sun.xpath.XPathStringLiteral;
import com.sun.xpath.XPathVariableReference;

/**
 * Visitor interface.
 * 
 * @author Sun Microsystems
 * @version 
 */
public interface XPathVisitor {

	/**
     * Visits an location step.
     * @param locationStep to visit
     */
    void visit(LocationStep locationStep);
    
    /**
     * Visits a string literal.
     * @param stringLiteral to visit
     * @return must be false since string literals don't have children
     */
    void visit(XPathStringLiteral stringLiteral);
    
    
    /**
     * Visits a numeric literal.
     * @param numericLiteral to visit
     */
    void visit(XPathNumericLiteral numericLiteral);
    
    
    /**
     * Visits a location path.
     * @param locationPath to visit
     */
    void visit(XPathLocationPath locationPath);
    
    /**
     * Visits a expression path.
     * @param expressionPath to visit
     */
    void visit(XPathExpressionPath expressionPath);
    
    
    /**
     * Visits a core operation.
     * @param coreOperation to visit
     */
    void visit(XPathCoreOperation coreOperation);
    
    
    /**
     * Visits a core function.
     * @param coreFunction to visit
     */
    void visit(XPathCoreFunction coreFunction);
    
    
    /**
     * Visits an extension function.
     * @param extensionFunction to visit
     */
    void visit(XPathExtensionFunction extensionFunction);
    
    /**
     * Visits a Variable
     * @param vReference
     */
    void visit(XPathVariableReference vReference);
    
    /**
     * visit a predicate (predicates are inside [] in a location/expression path)
     * @param predicate
     */
    void visit(XPathPredicateExpression predicate);
    
    void visit(XPathFunctionVariableParam variableParam);
    
    void visit(XPathFunctionPropertyParam propertyParam);
    
}
