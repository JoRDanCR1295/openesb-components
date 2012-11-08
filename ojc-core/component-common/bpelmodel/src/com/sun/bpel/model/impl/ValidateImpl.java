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
 * @(#)ValidateImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
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
public class ValidateImpl extends ActivityImpl implements Validate {

          
    /** 
     * Creates a new instance of Validate.
     */
    public ValidateImpl() {
        super();
        initValidate();
    }
    
    /** Creates a new instance of Validate.
     * @param   d   Owner document.
     */
    public ValidateImpl(XMLDocument d) {
        super(d);
        initValidate();
    }
    
    /** Initializes this class.
     */
    private void initValidate() {
        setLocalName(Validate.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[VARIABLES] = new XMLAttributeImpl(Validate.ATTR.VARIABLES, String.class, false, null);
    }

        public List getBPELVariables() {
                List variables = new ArrayList();
                String variablesString = getVariables();
                if(variables != null) {
                        StringTokenizer st = new StringTokenizer(variablesString, " ");
                        while(st.hasMoreTokens()) {
                                String variable = st.nextToken();
                                Variable var = BPELHelper.getMatchingVariable(variable, this);
                                if(var != null) {
                                        variables.add(var);
                                }
                        }
                }
                return variables;
        }

        public String getVariables() {
                return xmlAttrs[VARIABLES].getValue();
        }

        public void setVariables(String variables) {
                setAttribute(VARIABLES, variables);
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
