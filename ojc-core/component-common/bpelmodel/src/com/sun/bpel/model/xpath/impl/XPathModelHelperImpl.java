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

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.AbstractXPathModelHelper;
import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.XPathCoreFunction;
import com.sun.bpel.model.xpath.XPathCoreOperation;
import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathExpressionPath;
import com.sun.bpel.model.xpath.XPathExtensionFunction;
import com.sun.bpel.model.xpath.XPathLocationPath;
import com.sun.bpel.model.xpath.XPathModel;
import com.sun.bpel.model.xpath.XPathNumericLiteral;
import com.sun.bpel.model.xpath.XPathPredicateNumericLiteral;
import com.sun.bpel.model.xpath.XPathStringLiteral;


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
     * Instantiates a new XPathStringLiteral object of the type variable.
     * @param value the value
     * @return a new XPathStringLiteral object instance
     */    
    public XPathStringLiteral newXPathVariableReference(String variable) {

        return new XPathStringLiteralImpl(variable, true);
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
        return new XPathExtensionFunctionImpl(name);
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
