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
 * @(#)XPathExpressionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;


import com.sun.xpath.XPathExpression;
import com.sun.xpath.visitor.XPathVisitor;
import com.sun.xpath.visitor.impl.ExpressionWriter;


/**
 * Default implementation of the XPathExpression interface.
 * 
 * @author Sun Microsystems
 * @version 
 */
public abstract class XPathExpressionImpl
    implements XPathExpression {
    
    
    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void accept(XPathVisitor visitor) {
        // do nothing -- must be subclassed
    }
    
    
    /**
     * String representation.
     * @return the string representation
     */
    public String getExpressionString() {
        XPathVisitor visitor = new ExpressionWriter();
        accept(visitor);
        return ((ExpressionWriter) visitor).getString();
    }
    
    public String toString() {
    	return getExpressionString();
    }
}
