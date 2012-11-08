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
 * @(#)PartnerLinkType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

import java.util.Collection;

/**
 * Describes the SLT &lt;serviceLinkType&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PartnerLinkType
    extends ExtensibilityElement {
        
    /** Tag for this element */
    public static final String TAG = "partnerLinkType";
    
    /** Describes the attributes for this element */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
    }
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    void setName(String name);
    
    /**
     * Getter for property role.
     * @param index index of the property to get
     * @return Value of property role.
     *
     */
    PartnerLinkRole getRole(int index);
    
    /**
     * Getter for property role.
     * @param index index of the property to get
     * @return Value of property role.
     *
     */
    PartnerLinkRole getRole(String roleName);
    
    /**
     * Adds a new role.
     * @param role the role to add
     */
    void addRole(PartnerLinkRole role);
    
    /**
     * Removes the role at the specified position.
     * @param index the position of the role to remove
     */
    void removeRole(int index);
    
    /**
     * Removes the given role.
     * @param role to be removed
     */
    void removeRole(PartnerLinkRole role);
    
    /**
     * Gets index of role element within list.
     * @param   role    role element to index
     * @return  Index of role element within list
     */
    int indexOfRole(XMLNode role);

    /**
     * Gets the list of all roles.
     * @return a read-only collection of ServiceLinkRoles.
     */
    Collection getRoles();
}
