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
 * @(#)AbstractXPathVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.visitor;

import java.util.Collection;
import java.util.Iterator;

import com.sun.xpath.LocationStep;
import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathFunctionPropertyParam;
import com.sun.xpath.XPathFunctionVariableParam;
import com.sun.xpath.XPathLocationPath;
import com.sun.xpath.XPathNumericLiteral;
import com.sun.xpath.XPathOperationOrFuntion;
import com.sun.xpath.XPathPredicateExpression;
import com.sun.xpath.XPathStringLiteral;
import com.sun.xpath.XPathVariableReference;

/**
 * 
 * @author Sun Microsystems
 * 
 */
public abstract class AbstractXPathVisitor implements XPathVisitor {

    public void visit(LocationStep locationStep) {
    }

    public void visit(XPathCoreFunction coreFunction) {
    }

    public void visit(XPathCoreOperation coreOperation) {
    }

    public void visit(XPathExtensionFunction extensionFunction) {
    }

    public void visit(XPathLocationPath locationPath) {
    }

    public void visit(XPathNumericLiteral numericLiteral) {
    }

    public void visit(XPathStringLiteral stringLiteral) {
    }

    public void visit(XPathVariableReference vReference) {
    }

    public void visit(XPathPredicateExpression predicate) {
	XPathExpression predicateExpression = predicate.getPredicate();
	if (predicateExpression != null) {
	    predicateExpression.accept(this);
	}
    }

    protected void visitChildren(XPathOperationOrFuntion expr) {
	Collection children = expr.getChildren();
	if (children != null) {
	    Iterator it = children.iterator();
	    while (it.hasNext()) {
		XPathExpression child = (XPathExpression) it.next();
		child.accept(this);

	    }
	}
    }

    /*
         * (non-Javadoc)
         * 
         * @see com.sun.xpath.visitor.XPathVisitor#visit(com.sun.xpath.XPathFunctionVariableParam)
         */
    public void visit(XPathFunctionVariableParam variableParam) {
    }

    /*
         * (non-Javadoc)
         * 
         * @see com.sun.xpath.visitor.XPathVisitor#visit(com.sun.xpath.XPathFunctionPropertyParam)
         */
    public void visit(XPathFunctionPropertyParam propertyParam) {
    }

}
