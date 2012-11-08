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
 * @(#)GetVariablePropertyFunctionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.xpath.common.function.extension.impl;

//import org.apache.commons.jxpath.ri.compiler.Expression;

import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathFunctionPropertyParam;
import com.sun.xpath.XPathFunctionVariableParam;
import com.sun.xpath.XPathStringLiteral;
import com.sun.xpath.common.function.extension.GetVariablePropertyFunction;
import com.sun.xpath.impl.XPathExtensionFunctionImpl;
import com.sun.xpath.impl.XPathFunctionPropertyParamImpl;
import com.sun.xpath.impl.XPathFunctionVariableParamImpl;

/**
 * @author pVarghese
 *
 */
public class GetVariablePropertyFunctionImpl extends XPathExtensionFunctionImpl
		implements GetVariablePropertyFunction {
	
	String mVariableName = null;

	public GetVariablePropertyFunctionImpl(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

/*	public void processArguments(Expression[] arguments) {
		// TODO Auto-generated method stub
		
	}
*/
	
    /**
     * Adds a child expression.
     * @param child to be added
     */
    public void addChild(XPathExpression child) {
    	if (!(child instanceof XPathStringLiteral)) {
    		throw new RuntimeException
    			("Arguments to function getVariableProperty has to be String literals");
    	}
    	XPathStringLiteral literal = (XPathStringLiteral) child;
    	int childCount = getChildCount();
    	String arg = literal.getValue();
    	if (childCount == 0) { // process first argument.
    		mVariableName = arg;
    		String val = VAR_PREFIX + arg;
    		literal = new XPathFunctionVariableParamImpl(val);
    		
    	} else {
    		// process for the second param, which is the property name. 
    		// no need to check for the number of parameters as it is validated in the 
    		// XPathModel.validateFunction call. 
    		literal = new XPathFunctionPropertyParamImpl(arg, mVariableName);
    	}
    	super.addChild(literal);
    }
	
}
