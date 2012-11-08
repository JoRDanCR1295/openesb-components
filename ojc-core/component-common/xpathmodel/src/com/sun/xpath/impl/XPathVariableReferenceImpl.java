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
 * @(#)XPathVariableReferenceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;

import javax.xml.namespace.QName;

import com.sun.xpath.XPathVariableReference;
import com.sun.xpath.visitor.XPathVisitor;

import org.apache.commons.jxpath.ri.compiler.VariableReference;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class XPathVariableReferenceImpl extends XPathExpressionImpl implements XPathVariableReference {

	private VariableReference mVaribleReference;
	
	public XPathVariableReferenceImpl(VariableReference variableReference) {
		this.mVaribleReference = variableReference;
	}

	public QName getVariableName() {
		org.apache.commons.jxpath.ri.QName jxpathQName = this.mVaribleReference.getVariableName();
		String prefix = jxpathQName.getPrefix();
		String localName = jxpathQName.getName();
		if(prefix == null) {
			prefix = "";
		}
		
		if(localName == null) {
			localName = "";
		}
		return new QName(null, localName, prefix);
		
	}

	public VariableReference getVariableReference() {
		return this.mVaribleReference;
	}
	
	/**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void accept(XPathVisitor visitor) {
    	visitor.visit(this);
    }
	
}
