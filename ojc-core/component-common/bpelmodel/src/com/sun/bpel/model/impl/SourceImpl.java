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
 * @(#)SourceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.Source.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;source&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SourceImpl extends BPELElementImpl implements Source {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 4157115525297860539L;
    
    private Link mLink;
    
    /** Instantiates a source element model.
     */
    public SourceImpl() {
        super();
        initSource();
    }
    
    /** Instantiates a source element model.
     * @param   d   Owner document.
     */
    public SourceImpl(XMLDocument d) {
        super(d);
        initSource();
    }
    
    /** Initializes this class.
     */
    private void initSource() {
        setLocalName(Source.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.LINK_NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.TRANSITION_CONDITION, String.class, true,
                                 null)
        };
    }
    
    /** Getter for property linkName.
     * @return Value of property linkName.
     *
     */
    public String getLinkName() {
        return xmlAttrs[LINK_NAME].getValue();
    }
    
    /** Setter for property linkName.
     * @param linkName New value of property linkName.
     *
     */
    public void setLinkName(String linkName) {
        String oldLink = getLinkName();
        setAttribute(LINK_NAME, linkName);
        //if linkName is changed we need to clear out cached link object
        //so that next call to getBPELLink can find new link object
        if(linkName != null && !linkName.equals(oldLink)) {
        	this.mLink = null;
        }
    }
    
    
    /** Getter for property transitionCondition.
     * @return Value of property transitionCondition.
     *
     */
    public String getTransitionCondition() {
        return xmlAttrs[TRANSITION_CONDITION].getValue();
    }
    
    /** Setter for property transitionCondition.
     * @param transitionCondition New value of property transitionCondition.
     *
     */
    public void setTransitionCondition(String transitionCondition) {
        setAttribute(TRANSITION_CONDITION, transitionCondition);
    }
    
    /** Setter for property transitionCondition.
     * @param qName               New qName of property transitionCondition.
     * @param transitionCondition New value of property transitionCondition.
     *
     */
    public void setTransitionCondition(String qName,
                                       String transitionCondition) {
        setAttribute(TRANSITION_CONDITION, qName, transitionCondition);
    }
    
    
    public Link getBPELLink() {
    	if(this.mLink != null) {
			return this.mLink;
		}
		
		if (BPELHelper.isValueAbsent(getLinkName())) {
			return null;
		}
		 
		this.mLink = BPELHelper.getFlowLink(getLinkName(), this);
		return this.mLink;
		
	}
    
    

	public void setBPELLink(Link link) {
		//update the partnerLink attribute value
		if(link != null) {
			setLinkName(link.getName());
		} else {
			setLinkName(null);
		}
		
		this.mLink = link;
	}

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
