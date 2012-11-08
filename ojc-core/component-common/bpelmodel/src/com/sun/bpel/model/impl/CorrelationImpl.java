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
 * @(#)CorrelationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;correlation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CorrelationImpl extends BPELElementImpl implements Correlation {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 3845756355397144340L;
    
    private CorrelationSet mCorrSet = null;
    
    /** Constructs new instance of correlation element.
     */
    public CorrelationImpl() {
        super();
        initCorrelation();
    }
    
    /** Constructs new instance of correlation element
     * @param   d   Owner document.
     */
    public CorrelationImpl(XMLDocument d) {
        super(d);
        initCorrelation();
    }
    
    /** Initializes this class.
     */
    private void initCorrelation() {
        setLocalName(Correlation.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.SET, String.class, false, null),
            new XMLAttributeImpl(ATTR.INITIATE, String.class,
                true, ATTR.INITIATE_ENUM_VALS),
            new XMLAttributeImpl(ATTR.PATTERN, String.class, true,
                    ATTR.PATTERN_ENUM_VALS)
        };
        
    }
    
    /** Getter for property set.
     * @return Value of property set.
     *
     */
    public String getSet() {
        return xmlAttrs[SET].getValue();
    }
    
    /** Setter for property set.
     * @param set New value of property set.
     *
     */
    public void setSet(String set) {
        setAttribute(SET, set);
    }
    
    /** Setter for property set.
     * @param qName New qName of property set.
     * @param set   New value of property set.
     *
     */
    public void setSet(String qName, String set) {
        setAttribute(SET, qName, set);
    }
    
    /** Getter for property initiation.
     * @return Value of property initiation.
     *
     */
    public String getInitiate() {
        return xmlAttrs[INITIATE].getValue();
    }
    
    /** Setter for property initiation.
     * @param initiation New value of property initiation.
     *
     */
    public void setInitiate(String initiation) {
        setAttribute(INITIATE, initiation);
    }
    
    /** Getter for property pattern.
     * @return Value of property pattern.
     *
     */
    public String getPattern() {
        return xmlAttrs[PATTERN].getValue();
    }
    
    /** Setter for property pattern.
     * @param pattern New value of property pattern.
     *
     */
    public void setPattern(String pattern) {
        setAttribute(PATTERN, pattern);
    }
    
    /** Setter for property pattern.
     * @param qName        New qName of property pattern.
     * @param pattern   New value of property pattern.
     *
     */
    public void setPattern(String qName, String pattern) {
        setAttribute(PATTERN, qName, pattern);
    }

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        return v.visit(this);
    }

    /** @see com.sun.bpel.model.Correlation#getBPELCorrelationSet()
     */
    public CorrelationSet getBPELCorrelationSet() {
        if (mCorrSet != null) {
            return mCorrSet;
        }
        if (Utility.isEmpty(getSet())) {
            return null;
        }
        String setName = getSet();
        mCorrSet = BPELHelper.getMatchingCorrelationSet(setName, this);
        return mCorrSet;
    }
   
}
