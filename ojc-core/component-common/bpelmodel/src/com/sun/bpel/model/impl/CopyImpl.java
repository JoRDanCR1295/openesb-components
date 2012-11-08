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
 * @(#)CopyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Copy;
import com.sun.bpel.model.From;
import com.sun.bpel.model.To;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;



/**
 * Implements the &lt;copy&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CopyImpl extends BPELElementImpl implements Copy {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -2318811789893555292L;
    
    /** Holds the from element */
    private From from;
        
    /** Holds the list of to elements */
    private To to;
    
    /** Creates a new instance of CopyImpl */
    public CopyImpl() {
        super();
        initCopy();
    }
    
    /** Creates a new instance of CopyImpl.
     * @param   d   Owner document.
     */
    public CopyImpl(XMLDocument d) {
        super(d);
        initCopy();
    }
    
    /** Initializes this class.
     */
    private void initCopy() {
        setLocalName(Copy.TAG);
        xmlAttrs = new XMLAttribute[] {
                new XMLAttributeImpl(ATTR.IGNORE_MISSING_FROM_DATA, String.class, true, null),
                new XMLAttributeImpl(ATTR.BINARY_COPY, String.class, true, Copy.BINARY_COPY_ENUM_VALS)
            };
        childrenTags = new String[] {
            From.TAG,
            To.TAG
        };
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof From) {
            setFrom((From) c);
        } else if (c instanceof To) {
            setTo((To) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof From) {
            setFrom(null);
        } else if (c instanceof To) {
            setTo(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** Getter for element from.
     * @return Value of the element.
     *
     */
    public From getFrom() {
        return from;
    }
    
    /** Setter for element from.
     * @param f New value of the element.
     *
     */
    public void setFrom(From f) {
        super.replaceChild(1, from, f);
        from = f;
    }
    
    /** Getter for element to.
     * @return Value of the element.
     *
     */
    public To getTo() {
        return to;
    }
    
    /** Setter for element to.
     * @param t New value of the element.
     *
     */
    public void setTo(To t) {
        super.replaceChild(2, to, t);
        to = t;
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
    
    /** Getter for property ignoreMissingFromData.
     * @return Value of property faultName.
     *
     */
    public String getIgnoreMissingFromData() {
    	return xmlAttrs[IGNORE_MISSING_FROM_DATA].getValue();
    }
    
    /** Setter for property ignoreMissingFromData.
     * @param faultName New value of property ignoreMissingFromData.
     *
     */
    public void setIgnoreMissingFromData(String ignoreMissingFromData) {
    	setAttribute(IGNORE_MISSING_FROM_DATA, ignoreMissingFromData);
    }

    /*
     * @see com.sun.bpel.model.Copy#getBinaryCopy()
     */
    public String getBinaryCopy() {
	return xmlAttrs[BINARY_COPY].getValue();
    }

    /*
     * @see com.sun.bpel.model.Copy#setBinaryCopy(java.lang.String)
     */
    public void setBinaryCopy(String binaryCopy) {
	setAttribute(BINARY_COPY, binaryCopy);
    }
}
