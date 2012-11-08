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
 * @(#)TargetImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;target&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class TargetImpl extends BPELElementImpl implements Target {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 9000075209694296465L;
   
    private Link mLink;
    
    /** Instantiates a target element model.
     */
    public TargetImpl() {
        super();
        initTarget();
    }
    
    /** Instantiates a target element model.
     * @param   d   Owner document.
     */
    public TargetImpl(XMLDocument d) {
        super(d);
        initTarget();
    }
    
    /** Initializes this class.
     */
    private void initTarget() {
        setLocalName(Target.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.LINK_NAME, String.class, false, null)
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
        setAttribute(LINK_NAME, linkName);
    }
    
    /** Setter for property linkName.
     * @param qName    New qName of property linkName.
     * @param linkName New value of property linkName.
     *
     */
    public void setLinkName(String qName, String linkName) {
        setAttribute(LINK_NAME, qName, linkName);
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
