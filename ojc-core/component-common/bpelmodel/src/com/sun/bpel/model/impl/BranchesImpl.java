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
 * @(#)BranchesImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Branches;
import com.sun.bpel.model.Expression.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class BranchesImpl extends ExpressionImpl implements Branches {

	/** Creates a new instance of BranchesImpl */
    public BranchesImpl() {
        super();
        initBranches();
    }
    
    /** Creates a new instance of BranchesImpl.
     * @param   d   Owner document.
     */
    public BranchesImpl(XMLDocument d) {
        super(d);
        initBranches();
    }
    
    private void initBranches() {
    	setLocalName(Branches.TAG);
    	xmlAttrs = new XMLAttribute[] {
                new XMLAttributeImpl(Branches.ATTR.EXPRESSIONLANGUAGE, String.class, false, null),
                new XMLAttributeImpl(Branches.ATTR.COUNT_COMPLETED_BRANCHES_ONLY, String.class, true, new String[] {"yes", "no"})
            };
    	
    }

	public String getCountCompletedBranchesOnly() {
		return xmlAttrs[COUNT_COMPLETED_BRANCHES_ONLY].getValue();
	}

	public void setCountCompletedBranchesOnly(String completedBranches) {
		setAttribute(COUNT_COMPLETED_BRANCHES_ONLY, completedBranches);
	}
    
    
	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
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
