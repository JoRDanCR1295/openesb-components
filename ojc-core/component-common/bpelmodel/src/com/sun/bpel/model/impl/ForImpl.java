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
 * @(#)ForImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.For;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ForImpl extends ExpressionImpl implements For {
	/** Creates a new instance of UntilImpl */
    public ForImpl() {
        super();
        initUntil();
    }
    
    /** Creates a new instance of UntilImpl.
     * @param   d   Owner document.
     */
    public ForImpl(XMLDocument d) {
        super(d);
        initUntil();
    }
    
    /** Initializes this class.
     */
    private void initUntil() {
        setLocalName(For.TAG);
        super.initExpression();
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
         v.visit(this);
        return true;
    }
}
