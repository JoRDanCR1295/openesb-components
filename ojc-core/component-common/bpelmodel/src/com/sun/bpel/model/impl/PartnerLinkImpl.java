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
 * @(#)PartnerLinkImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;


import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;


/**
 * Implements the &lt;partner&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PartnerLinkImpl extends BPELElementImpl implements PartnerLink {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -6858487069293762115L;
    
    private PartnerLinkType mServiceLinkType;
    
    private PortType mMyRolePortType;
    
    private PortType mPartnerRolePortType;
    
    /** Creates a new instance of PartnerLinkImpl */
    public PartnerLinkImpl() {
        super();
        initPartner();
    }
    
    /** Constructs a new instance of partner
     * @param   d   Owner document.
     */
    public PartnerLinkImpl(XMLDocument d) {
        super(d);
        initPartner();
    }

    /** Initializes this class.
     */
    private void initPartner() {
        setLocalName(PartnerLink.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.PARTNER_LINK_TYPE, String.class, false,
                                 null),
            new XMLAttributeImpl(ATTR.MY_ROLE, String.class, true, null),
            new XMLAttributeImpl(ATTR.PARTNER_ROLE, String.class, true, null)
        };
    }
    
    /** Getter for property name.
     * @return Value of property name.
     *
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for property name.
     * @param name New value of property name.
     *
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
   
    
    /** Getter for property serviceLinkType.
     * @return Value of property serviceLinkType.
     *
     */
    public QName getPartnerLinkType() {
        String partnerLinkType = xmlAttrs[PARTNER_LINK_TYPE].getValue();
        if(partnerLinkType != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
        			partnerLinkType, this);
        }
        
        return null;
    }
    
    public void setPartnerLinkType(QName partnerLinkType) {
    	QName oldPartnerLinkType = getPartnerLinkType();
    	if(partnerLinkType != null) {
    		setAttribute(PARTNER_LINK_TYPE, 
    				com.sun.bpel.xml.NamespaceUtility.getQNameAsString(partnerLinkType));
    	} else {
    		setAttribute(PARTNER_LINK_TYPE, null);
    	}
    	
    	//if partnerLinkType is changed we need to clear out cached myRole and partnerRole portType object
        //so that next call to getMyRolePortType or getPartnerRolePortType can find new portType object
        if(partnerLinkType == null || !partnerLinkType.equals(oldPartnerLinkType)) {
        	this.mMyRolePortType = null;
        	this.mPartnerRolePortType = null;
        }
    }
    
    
    /** Getter for property myRole.
     * @return Value of property myRole.
     *
     */
    public String getMyRole() {
        return xmlAttrs[MY_ROLE].getValue();
    }
    
    /** Setter for property myRole.
     * @param myRole New value of property myRole.
     *
     */
    public void setMyRole(String myRole) {
        String oldMyRole = getMyRole();
    	setAttribute(MY_ROLE, myRole);
        
    	//if myRole is changed we need to clear out cached myRole portType object
        //so that next call to getMyRolePortType can find new portType object
        if(myRole == null || !myRole.equals(oldMyRole)) {
        	this.mMyRolePortType = null;
        }
    }
    
    
    /** Getter for property partnerRole.
     * @return Value of property partnerRole.
     *
     */
    public String getPartnerRole() {
        return xmlAttrs[PARTNER_ROLE].getValue();
    }
    
    /** Setter for property partnerRole.
     * @param partnerRole New value of property partnerRole.
     *
     */
    public void setPartnerRole(String partnerRole) {
        String oldPartnerRole = getPartnerRole();
    	setAttribute(PARTNER_ROLE, partnerRole);
        
    	//if partnerRole is changed we need to clear out cached partnerRole portType object
        //so that next call to getPartnerRolePortType can find new portType object
        if(partnerRole == null || !partnerRole.equals(oldPartnerRole)) {
        	this.mPartnerRolePortType = null;
        }

    }
    
    
    
    public PartnerLinkType getBPELPartnerLinkType() {
		if(mServiceLinkType != null) {
			return mServiceLinkType;
		}
		
		return BPELHelper.getMatchingPartnerLinkType(this);
		
	}

	public void setBPELPartnerLinkType(PartnerLinkType serviceLinkType) {
		//update the serviceLinkType attribute value
		if(serviceLinkType != null) {
			if(serviceLinkType.getName().getNamespaceURI() != null) {
				setPartnerLinkType(serviceLinkType.getName());
			} else {
				throw new IllegalArgumentException("can not set serviceLinkType, serviceLinkType "+ serviceLinkType.getName() + " 's owner WSDLDocument does not have targetNamespace attribute defined");
			}
			
		}
		
		this.mServiceLinkType = serviceLinkType;
		
	}

	
	public PortType getMyRoleWSDLPortType() {
		if(this.mMyRolePortType != null) {
			return this.mMyRolePortType;
		}
		
		PartnerLinkType partnerLinkType = getBPELPartnerLinkType();
		if(partnerLinkType != null && getMyRole() != null) {
			PartnerLinkRole role = partnerLinkType.getRole(getMyRole());
			if(role != null) {
				 this.mMyRolePortType = role.getPort();
			}
			
		}
		
		return this.mMyRolePortType;
	}

	public PortType getPartnerRoleWSDLPortType() {
		if(this.mPartnerRolePortType != null) {
			return this.mPartnerRolePortType;
		}
		
		PartnerLinkType partnerLinkType = getBPELPartnerLinkType();
		if(partnerLinkType != null && getPartnerRole() != null) {
			PartnerLinkRole role = partnerLinkType.getRole(getPartnerRole());
			if(role != null) {
			 	 this.mPartnerRolePortType = role.getPort();
			}
			
		}
		
		return this.mPartnerRolePortType;
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
