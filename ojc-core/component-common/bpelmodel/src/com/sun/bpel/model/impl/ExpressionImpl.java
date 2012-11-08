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
 * @(#)ExpressionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.Expression;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ExpressionImpl extends BaseElementImpl implements Expression {

	/** Creates a new instance of ExpressionImpl */
    public ExpressionImpl() {
        super();
        initExpression();
    }
    
    /** Creates a new instance of ExpressionImpl.
     * @param   d   Owner document.
     */
    public ExpressionImpl(XMLDocument d) {
        super(d);
        initExpression();
    }
    
    /** Initializes this class.
     */
    protected void initExpression() {
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.EXPRESSIONLANGUAGE, String.class, false, null)
        };
   }
    
	public String getExpressionLanguage() {
		return xmlAttrs[EXPRESSIONLANGUAGE].getValue();
	}
	
	public void setExpressionLanguage(String expressionLanguage) {
		setAttribute(EXPRESSIONLANGUAGE, expressionLanguage);
	}

}
