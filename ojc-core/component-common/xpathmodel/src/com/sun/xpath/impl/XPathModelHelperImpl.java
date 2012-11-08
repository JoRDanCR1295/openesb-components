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
 * @(#)XPathModelHelperImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;

import com.sun.xpath.AbstractXPathModelHelper;
import com.sun.xpath.LocationStep;
import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathExpressionPath;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathLocationPath;
import com.sun.xpath.XPathModel;
import com.sun.xpath.XPathNumericLiteral;
import com.sun.xpath.XPathPredicateExpression;
import com.sun.xpath.XPathPredicateNumericLiteral;
import com.sun.xpath.XPathStringLiteral;
import com.sun.xpath.XPathVariableReference;
import com.sun.xpath.common.function.extension.GetContainerDataFunction;
import com.sun.xpath.common.function.extension.GetVariablePropertyFunction;
import com.sun.xpath.common.function.extension.impl.GetContainerDataFunctionImpl;
import com.sun.xpath.common.function.extension.impl.GetVariablePropertyFunctionImpl;

import org.apache.commons.jxpath.ri.compiler.VariableReference;


/**
 * XPathModel helper class.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XPathModelHelperImpl extends AbstractXPathModelHelper {

    /**
     * Instantiates a new XPathModel object.
     * @return a new XPathModel object instance
     */
    public XPathModel newXPathModel() {
        return new XPathModelImpl();
    }
    
    
    /**
     * Instantiates a new XPathStringLiteral object.
     * @param value the value
     * @return a new XPathStringLiteral object instance
     */
    public XPathStringLiteral newXPathStringLiteral(String value) {
        return new XPathStringLiteralImpl(value);
    }
   
    /**
     * Instantiates a new XPathVariableReference object of the type variable.
     * @param value the value
     * @return a new XPathVariableReference object instance
     */    
    public XPathVariableReference newXPathVariableReference(VariableReference vReference) {

        return new XPathVariableReferenceImpl(vReference);
    }

    
    /**
     * Instantiates a new XPathPredicateExpression object for given expression.
     * @param expression which is a predicate expression
     * @return a new XPathPredicateExpression object instance
     */
    public XPathPredicateExpression newXPathPredicateExpression(XPathExpression expression) {
		return new XPathPredicateExpressionImpl(expression);
	}


	/**
     * Instantiates a new XPathNumericLiteral object.
     * @param value the value
     * @return a new XPathNumericLiteral object instance
     */
    public XPathNumericLiteral newXPathNumericLiteral(Number value) {
        return new XPathNumericLiteralImpl(value);
    }
    
   /**
     * Instantiates a new XPathPredicateNumericLiteral object.
     * @param value the value
     * @return a new XPathPredicateNumericLiteral object instance
     */
    public XPathPredicateNumericLiteral newXPathPredicateNumericLiteral(Long value) {
        return new XPathPredicateNumericLiteralImpl(value);
    }
        
    /**
     * Instantiates a new XPathCoreFunction object.
     * @param function the function code
     * @return a new XPathCoreFunction object instance
     */
    public XPathCoreFunction newXPathCoreFunction(int function) {
        return new XPathCoreFunctionImpl(function);
    }
    
    
    /**
     * Instantiates a new XPathExtension Function object.
     * @param name the function name
     * @return a new XPathExtensionFunction object instance
     */
    public XPathExtensionFunction newXPathExtensionFunction(
                                                            String name) {
        String functionName = name;
        int colon = name.indexOf(':');
        if (colon != -1) {
        	functionName = name.substring(colon + 1, name.length());
        }
    	
    	if(functionName.equals(GetContainerDataFunction.NAME)) {
    		return new GetContainerDataFunctionImpl(name);
    	} else if (functionName.equals(GetVariablePropertyFunction.NAME)) {
    		return new GetVariablePropertyFunctionImpl(name);
    	} else {
    		return new XPathExtensionFunctionImpl(name);
    	}
    }
    
    
    /**
     * Instantiates a new XPathCoreOperation object.
     * @param code the operation code
     * @return a new XPathCoreOperatoin object instance
     */
    public XPathCoreOperation newXPathCoreOperation(int code) {
        return new XPathCoreOperationImpl(code);
    }
    
    
    /**
     * Instantiates a new XPathLocationPath object.
     * @param steps the steps
     * @return a new XPathLocationPath object instance
     */
    public XPathLocationPath newXPathLocationPath(LocationStep[] steps) {
        return new XPathLocationPathImpl(steps);
    }

    /**
     * Instantiates a new XPathExpressionPath object.
     * @param rootExpression root expression if any
     * @param steps the steps
     * @return a new XPathLocationPath object instance
     */
    public XPathExpressionPath newXPathExpressionPath(XPathExpression rootExpression, 
    												 	   LocationStep[] steps) {
    	
    	return new XPathExpressionPathImpl(rootExpression, steps, false);
    }
    
    /**
     * Determines if a function name is valid. Assumes the function name is
     * not one of the core functions.
     * @param functionName the name of the function
     * @return true if the function name is valid, false otherwise
     */
    public boolean isValidFunction(String functionName) {
        return XPathModelImpl.isValidFunction(functionName);
    }
}
