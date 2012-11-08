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
 * @(#)VariablePropertyVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.bpel.model.visitor;

import java.util.ArrayList;
import java.util.List;

import com.sun.xpath.LocationStep;
import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathExpressionPath;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathFunctionPropertyParam;
import com.sun.xpath.XPathLocationPath;
import com.sun.xpath.XPathPredicateExpression;
import com.sun.xpath.visitor.AbstractXPathVisitor;

/**
 * This visitor extracts all properties that maybe present in the expression in the form
 * of the getVariableProperty() function. Supports properties defined in getVariableProperty()
 * function that are defined a perdicate nodes, etc. 
 * ex: Some examples are shown. 
 * <code>
 * $OrderOperationIn.orderPart/itemDetail[ns1:getVariableProperty('xsdIntVar', 'ns0:intProp')]
 * ns2:getVariableProperty('IntPropValOperationIn', 'ns0:intProp') 
                + ns2:getVariableProperty('MessageXsdTypeVar', 'ns0:intProp')
 * </code>
 * @author pVarghese
 *
 */
public class VariablePropertyVisitor extends AbstractXPathVisitor {

    List<PropertyParamObj> propertyList = new ArrayList<PropertyParamObj>();

    public void visit(XPathCoreOperation coreOperation) {
	visitChildren(coreOperation);
    }

    public void visit(XPathCoreFunction coreFunction) {
	visitChildren(coreFunction);
    }

    public void visit(XPathExtensionFunction extensionFunction) {
	visitChildren(extensionFunction);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.xpath.visitor.XPathVisitor#visit(com.sun.xpath.XPathFunctionPropertyParam)
     */
    public void visit(XPathFunctionPropertyParam propertyParam) {
	String property = propertyParam.getExpressionString();
	String variable = propertyParam.getVariableName();
	// propertyMap.put(property, variable);
	PropertyParamObj obj = new PropertyParamObj(property, variable);
	propertyList.add(obj);
    }

    /**
     * Visits an location step.
     * @param locationStep to visit
     */
    public void visit(LocationStep locationStep) {
	XPathPredicateExpression[] predicates = locationStep.getPredicates();
	if (predicates != null) {
	    for (int j = 0, length = predicates.length; j < length; j++) {
		XPathPredicateExpression predicateExpression = predicates[j];
		predicateExpression.accept(this);
	    }
	}

    }

    /**
     * Visits a location path.
     * @param locationPath to visit
     */
    public void visit(XPathLocationPath locationPath) {
	LocationStep[] steps = locationPath.getSteps();
	for (int i = 0; i < steps.length; i++) {
	    XPathPredicateExpression[] predicates = steps[i].getPredicates();
	    if (predicates != null) {
		for (int j = 0, length = predicates.length; j < length; j++) {
		    XPathPredicateExpression predicateExpression = predicates[j];
		    predicateExpression.accept(this);
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
	if (rootExpression != null) {
	    // TODO: Is it needed to visit the root expression for variable properties??
	    // cant think of a scenario where this maybe needed.
	}

	LocationStep[] steps = expressionPath.getSteps();

	for (int i = 0; i < steps.length; i++) {
	    XPathPredicateExpression[] predicates = steps[i].getPredicates();
	    if (predicates != null) {
		for (int j = 0, length = predicates.length; j < length; j++) {
		    XPathPredicateExpression predicateExpression = predicates[j];
		    predicateExpression.accept(this);
		}
	    }
	}
    }

    public List<PropertyParamObj> getPropertyList() {
	return propertyList;
    }

    public static class PropertyParamObj {
	public String propertyName = null;

	public String variableName = null;

	public PropertyParamObj(String property, String variable) {
	    propertyName = property;
	    variableName = variable;
	}
    }


}
