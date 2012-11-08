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
 * @(#)AssignImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;

import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;assign&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ExtensionAssignOperationImpl extends ActivityImpl implements ExtensionAssignOperation {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -6872032980020371555L;

    private SunExtExpression expression;


    /** Creates a new instance of AssignImpl */
    public ExtensionAssignOperationImpl() {
        super();
        initExtensionAssignOperation();
    }
    
    /** Creates a new instance of AssignImpl.
     * @param   d   Owner document.
     */
    public ExtensionAssignOperationImpl(XMLDocument d) {
        super(d);
        initExtensionAssignOperation();
    }
    
    /** Initialize this class. */
    private void initExtensionAssignOperation() {
        setLocalName(ExtensionAssignOperation.TAG);
        childrenTags = new String[] { SunExtExpression.TAG };
    }

    /** @see XMLNode#accept
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

    public SunExtExpression getSunExtExpression() {
        return expression;
    }

    public void setSunExtExpression(SunExtExpression e) {
        expression = e;
        
    }
}
