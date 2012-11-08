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
 * @(#)QueryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.bpel.impl;

import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.impl.WSDLElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class QueryImpl extends WSDLElementImpl implements Query {

	/** Creates a new instance of QueryImpl */
    public QueryImpl() {
        super();
        initQuery();
    }
    
    /** Creates a new instance of QueryImpl.
     * @param   d   Owner document.
     */
    public QueryImpl(XMLDocument d) {
        super(d);
        initQuery();
    }
    
    /** Initializes this class.
     */
    private void initQuery() {
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.QUERYLANGUAGE, String.class, false, null)
        };
   }
    
	public String getQueryLanguage() {
		return xmlAttrs[QUERYLANGUAGE].getValue();
	}
	
	public void setQueryLanguage(String expressionLanguage) {
		setAttribute(QUERYLANGUAGE, expressionLanguage);
	}

    /** @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.visitor.Visitor)
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);

        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }

        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }    
}
