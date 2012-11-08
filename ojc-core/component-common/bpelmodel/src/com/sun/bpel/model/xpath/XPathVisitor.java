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

package com.sun.bpel.model.xpath;

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
     * @return true means the child expressions can be visited
     */
    boolean visit(LocationStep locationStep);
    
    /**
     * Visits a string literal.
     * @param stringLiteral to visit
     * @return must be false since string literals don't have children
     */
    boolean visit(XPathStringLiteral stringLiteral);
    
    
    /**
     * Visits a numeric literal.
     * @param numericLiteral to visit
     * @return must be false since numeric literals don't have children
     */
    boolean visit(XPathNumericLiteral numericLiteral);
    
    
    /**
     * Visits a location path.
     * @param locationPath to visit
     * @return true means the child expressions can be visited
     */
    boolean visit(XPathLocationPath locationPath);
    
    /**
     * Visits a expression path.
     * @param expressionPath to visit
     * @return true means the child expressions can be visited
     */
    boolean visit(XPathExpressionPath expressionPath);
    
    
    /**
     * Visits a core operation.
     * @param coreOperation to visit
     * @return true means the child expressions can be visited
     */
    boolean visit(XPathCoreOperation coreOperation);
    
    
    /**
     * Visits a core function.
     * @param coreFunction to visit
     * @return true means the child expressions can be visited
     */
    boolean visit(XPathCoreFunction coreFunction);
    
    
    /**
     * Visits an extension function.
     * @param extensionFunction to visit
     * @return true means the child expressions can be visited
     */
    boolean visit(XPathExtensionFunction extensionFunction);
    
    
}
