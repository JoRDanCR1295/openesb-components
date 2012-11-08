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
 * @(#)CompensateImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements a &lt;compensate&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CompensateImpl extends ActivityImpl implements Compensate {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 9011271486060409329L;
    
    /** Creates a new instance of CompensateImpl */
    public CompensateImpl() {
        super();
        initCompensate();
    }
    
    /** Create a new instance of CompensateImpl.
     * @param   d   Owner document.
     */
    public CompensateImpl(XMLDocument d) {
        super(d);
        initCompensate();
    }
    
    /** Initializes this class.
     */
    private void initCompensate() {
        setLocalName(Compensate.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[SCOPE] = new XMLAttributeImpl(Compensate.ATTR.SCOPE, String.class, true, null);
    }
    
    /** Getter for the scope attribute.
     * @return  Value of scope attribute.
     */
    public String getScope() {
        return xmlAttrs[SCOPE].getValue();
    }
    
    /** Setter for the scope attribute.
     * @param   s   Value of scope attribute.
     */
    public void setScope(String s) {
        setAttribute(SCOPE, s);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
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
