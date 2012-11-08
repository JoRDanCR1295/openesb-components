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
 * @(#)XPathNumericLiteralImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.XPathNumericLiteral;
import com.sun.bpel.model.xpath.XPathVisitable;
import com.sun.bpel.model.xpath.XPathVisitor;


/**
 * Represents a numeric literal.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XPathNumericLiteralImpl
    extends XPathExpressionImpl
    implements XPathNumericLiteral, XPathVisitable {
    
    /** The numeric literal value. */
    Number mValue;
    
    
    /**
     * Constructor. Instantiates with the given value.
     * @param value the numeric literal value
     */
    public XPathNumericLiteralImpl(Number value) {
        super();
        setValue(value);
    }
    
    
    /**
     * Gets the value.
     * @return the numeric literal value
     */
    public Number getValue() {
        return mValue;
    }
    
    
    /**
     * Sets the value.
     * @param value the numeric literal value
     */
    public void setValue(Number value) {
        mValue = value;
    }
    

    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void callVisitor(XPathVisitor visitor) {
        visitor.visit(this);
    }
}
