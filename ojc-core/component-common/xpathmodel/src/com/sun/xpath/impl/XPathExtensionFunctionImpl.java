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
 * @(#)XPathExtensionFunctionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;


import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.common.function.extension.visitor.XPathExtensionFunctionVisitor;
import com.sun.xpath.visitor.XPathVisitor;


/**
 * Represents a extension XPath function.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XPathExtensionFunctionImpl
    extends XPathOperatorOrFunctionImpl
    implements XPathExtensionFunction {
        
    /** The function name. */
    String mName;
    
    
    /**
     * Constructor.
     * Instantiates a new XPathExtensionFunction with the given name.
     * @param name the function name
     */
    public XPathExtensionFunctionImpl(String name) {
        super();
        setName(name);
    }
    
    
    /**
     * Gets the name of the function.
     * @return the function name
     */
    public String getName() {
        return mName;
    }
    
    
    /**
     * Sets the function name.
     * @param name the new function name
     */
    public void setName(String name) {
        mName = name;
    }

    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void accept(XPathVisitor visitor) {
        visitor.visit(this);
        
    }


	public void accept(XPathExtensionFunctionVisitor visitor) {
		//do nothing
	}
    
    
}
