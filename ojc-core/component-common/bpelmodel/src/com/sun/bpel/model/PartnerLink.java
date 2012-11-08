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
 * @(#)PartnerLink.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

/**
 * Describes the &lt;partner&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PartnerLink extends BPELElement, NamedElement {
    
    /** Tag for this element */
    public static final String TAG = "partnerLink";
    
    /** Describes the attributes of this element.
     */
    public interface ATTR extends NamedElement.ATTR {
        
        /** "serviceLinkType" attribute token */
        public static final String PARTNER_LINK_TYPE = "partnerLinkType";
        
        /** "myRole" attribute token */
        public static final String MY_ROLE = "myRole";
        
        /** "partnerRole" attribute token */
        public static final String PARTNER_ROLE = "partnerRole";
    }
    
    /** Ordinal position of serviceLinkType attribute */
    public static final int PARTNER_LINK_TYPE = NAME + 1;
    
    /** Ordinal position of myRole attribute */
    public static final int MY_ROLE = PARTNER_LINK_TYPE + 1;
    
    /** Ordinal position of partnerRole attribute */
    public static final int PARTNER_ROLE = MY_ROLE + 1;
    
    
    /** Getter for property partnerLinkType.
     * @return Value of property partnerLinkType.
     *
     */
    QName getPartnerLinkType();
    
    /** Setter for property partnerLinkType.
     * @param partnerLinkType New value of property partnerLinkType.
     *
     */
    void setPartnerLinkType(QName partnerLinkType);
    

    /** Getter for property partnerLinkType.
     * @return Value of property partnerLinkType.
     *
     */
    PartnerLinkType getBPELPartnerLinkType();
    
    /** Setter for property partnerLinkType.
     * @param serviceLinkType New value of property partnerLinkType.
     *
     */
    void setBPELPartnerLinkType(PartnerLinkType serviceLinkType);
    
    /** Getter for property myRole.
     * @return Value of property myRole.
     *
     */
    String getMyRole();
    
    /** Setter for property myRole.
     * @param myRole New value of property myRole.
     *
     */
    void setMyRole(String myRole);
    
    
    /** Getter for property partnerRole.
     * @return Value of property partnerRole.
     *
     */
    String getPartnerRole();
    
    /** Setter for property partnerRole.
     * @param partnerRole New value of property partnerRole.
     *
     */
    void setPartnerRole(String partnerRole);
    
    /**
     * Get the correspoinding portType for my role
     * from the partnerLinkType this partner refers to.
     * @return PortType which myRole in parnerLinkType refers to.
     */
    PortType getMyRoleWSDLPortType();
    
    
    /**
     * Get the correspoinding portType for my role
     * from the partnerLinkType this partner refers to.
     * @return PortType which myRole in parnerLinkType refers to.
     */
    PortType getPartnerRoleWSDLPortType();
    
 }
