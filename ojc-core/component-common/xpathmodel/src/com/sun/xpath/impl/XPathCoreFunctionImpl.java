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
 * @(#)XPathCoreFunctionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;

import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.common.function.core.visitor.XPathCoreFunctionVisitor;
import com.sun.xpath.visitor.XPathVisitor;



/**
 * Represents a core XPath function.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XPathCoreFunctionImpl
    extends XPathOperatorOrFunctionImpl
    implements XPathCoreFunction {
        
    /** The function code. */
    int mFunction;
    
    
    /**
     * Constructor. Instantiates a new XPathCoreFunction with the given code.
     * @param function the function code
     */
    public XPathCoreFunctionImpl(int function) {
        super();
        setFunction(function);
    }
    
    
    /**
     * Gets the function code.
     * @return the function code
     */
    public int getFunction() {
        return mFunction;
    }
    
    
    /**
     * Sets the function code.
     * @param function the function code
     */
    public void setFunction(int function) {
        mFunction = function;
    }
    
    
    /**
     * Gets the name of the function.
     * @return the function name or null if invalid
     */
    public String getName() {
        int code = getFunction();

        switch (code) {
        case XPathCoreFunction.FUNC_LAST:
            return "last";
        case XPathCoreFunction.FUNC_POSITION:
            return "position";
        case XPathCoreFunction.FUNC_COUNT:
            return "count";
        case XPathCoreFunction.FUNC_ID:
            return "id";
        case XPathCoreFunction.FUNC_LOCAL_NAME:
            return "local-name";
        case XPathCoreFunction.FUNC_NAMESPACE_URI:
            return "namespace-uri";
        case XPathCoreFunction.FUNC_NAME:
            return "name";
        case XPathCoreFunction.FUNC_STRING:
            return "string";
        case XPathCoreFunction.FUNC_CONCAT:
            return "concat";
        case XPathCoreFunction.FUNC_STARTS_WITH:
            return "starts-with";
        case XPathCoreFunction.FUNC_CONTAINS:
            return "contains";
        case XPathCoreFunction.FUNC_SUBSTRING_BEFORE:
            return "substring-before";
        case XPathCoreFunction.FUNC_SUBSTRING_AFTER:
            return "substring-after";
        case XPathCoreFunction.FUNC_SUBSTRING:
            return "substring";
        case XPathCoreFunction.FUNC_STRING_LENGTH:
            return "string-length";
        case XPathCoreFunction.FUNC_NORMALIZE_SPACE:
            return "normalize-space";
        case XPathCoreFunction.FUNC_TRANSLATE:
            return "translate";
        case XPathCoreFunction.FUNC_BOOLEAN:
            return "boolean";
        case XPathCoreFunction.FUNC_NOT:
            return "not";
        case XPathCoreFunction.FUNC_TRUE:
            return "true";
        case XPathCoreFunction.FUNC_FALSE:
            return "false";
        case XPathCoreFunction.FUNC_LANG:
            return "lang";
        case XPathCoreFunction.FUNC_NUMBER:
            return "number";
        case XPathCoreFunction.FUNC_SUM:
            return "sum";
        case XPathCoreFunction.FUNC_FLOOR:
            return "floor";
        case XPathCoreFunction.FUNC_CEILING:
            return "ceiling";
        case XPathCoreFunction.FUNC_ROUND:
            return "round";
        case XPathCoreFunction.FUNC_NULL:
            return "null";
        case XPathCoreFunction.FUNC_KEY:
            return "key";
        case XPathCoreFunction.FUNC_FORMAT_NUMBER:
            return "format-number";
        case XPathCoreFunction.FUNC_EXISTS:
        	return "exists";
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


	public void accept(XPathCoreFunctionVisitor visitor) {
		//do nothing
		
	}
    
    
}
