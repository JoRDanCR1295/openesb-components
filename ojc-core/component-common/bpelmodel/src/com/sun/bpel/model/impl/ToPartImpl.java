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
 * @(#)ToPartImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.ToPart;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

import javax.wsdl.Part;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class ToPartImpl extends BPELElementImpl implements ToPart {
    
    private Part mPart;
    
    private Variable mFromVariable;
    
    /** Creates a new instance of ToPartImpl */
    public ToPartImpl() {
    	super();
    	initToPart();
    }
   
    /** Creates a new instance of ToPartImpl */
    public ToPartImpl(XMLDocument d) {
    	super(d);
    	initToPart();
    }
    
    /** Initializes this class.
     */
    private void initToPart() {
        setLocalName(ToPart.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ToPart.ATTR.PART, String.class, false,
                                 null),
            new XMLAttributeImpl(ToPart.ATTR.FROM_VARIABLE, String.class, false,
                                 null),
            
        };
    }
    
    
    
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }
    
    public void setPart(String part) {
        String oldPart = getPart();
        setAttribute(PART, part);
        //if part is changed we need to clear out cached part object
        //so that next call to getWSDLPart can find new Part object
        if(part == null || !part.equals(oldPart)) {
        	this.mPart = null;
        }
    }
    
    public String getFromVariable() {
        return xmlAttrs[FROM_VARIABLE].getValue();
    }
    
    public void setFromVariable(String variable) {
        String oldFromVariable = getFromVariable();
        setAttribute(FROM_VARIABLE, variable);
        //if fromVariable is changed we need to clear out cached toVariable object
        //so that next call to getBPELFromVariable can find new toVariable object
        if(variable == null || !variable.equals(oldFromVariable)) {
        	this.mFromVariable = null;
        }
    }
    
    public Part getWSDLPart() {
        return this.mPart;
    }
    
    public Variable getBPELFromVariable() {
        return this.mFromVariable;
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
