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
 * @(#)LiteralImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import org.w3c.dom.Element;

import com.sun.bpel.model.Literal;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LiteralImpl extends BaseElementImpl implements Literal {
    Element mChildElement;
    
	/** @see com.sun.bpel.model.wsdlmodel.impl.XMLNodeImpl#getValue() */
    public String getValue() {
        String value = super.getValue();
        return (value == null) ? "" : value;
    }

	/** Creates a new instance of LiteralImpl */
    public LiteralImpl() {
        super();
        initLiteral();
    }
    
    /** Creates a new instance of LiteralImpl.
     * @param   d   Owner document.
     */
    public LiteralImpl(XMLDocument d) {
        super(d);
        initLiteral();
    }
    
    /** Initializes this class.
     */
    private void initLiteral() {
        setLocalName(Literal.TAG);
    }
 	
 	 /** @see com.sun.bpel.xml.common.model.XMLNode#accept(
     *  com.sun.bpel.xml.common.model.visitor.Visitor)
     */
    public boolean accept(Visitor w) {
    	BPELVisitor v = morphVisitor(w);
        return v.visit(this);
    }

    /** @see com.sun.bpel.model.Literal#getEII()
     */
    public Element getEII() {
        return mChildElement;
    }

    /** @see com.sun.bpel.model.Literal#setEII(org.w3c.dom.Element)
     */
    public void setEII(Element eII) {
        mChildElement = eII;
        setValue(null);
    }
    
    
}
